## server.R

shinyServer(function(input, output, session) {
  
  # Enable automatic ggplot2 theming matching the bslib theme
  thematic::thematic_shiny(font = "auto")
  
  # ── Login Logic ──────────────────────────────────────────
  USER <- login_server(
    id = "login",
    db_conn = db_conn,
    # Gmail SMTP, built in global.R from GMAIL_USER / GMAIL_PASS. NULL when the
    # variables are missing, which switches the reset panel to "Email server has
    # not been configured." - ui.R hides the sign-up card in that case too.
    emailer = app_emailer,
    # Backstop for that case: the package default is
    # verify_email = !is.null(emailer), i.e. with no emailer a signup would be
    # accepted immediately. Forcing it on means a signup can never insert a row
    # without a code being sent first.
    verify_email = TRUE,
    enclosing_panel = login_card
  )
  
  observe({
    if (USER$logged_in) {
      shinyjs::hide("login_screen")
      shinyjs::show("main_app")
    } else {
      shinyjs::show("login_screen")
      shinyjs::hide("main_app")
    }
  })

  # ── Sync flag to prevent infinite loops ─────────────────
  sync_source <- reactiveVal("none")  # "slider", "date", or "none"
  
  # ── Reactives ───────────────────────────────────────────
  
  # What (if anything) is wrong with the two quantile inputs right now.
  # NULL means the pair can define a log-normal distribution.
  input_problem <- reactive({
    quantile_problem(input$q50, input$q90)
  })

  dist_params <- reactive({
    # Reject an impossible pair (q90 <= q50, or a missing/non-positive value)
    # with a plain-English message instead of letting
    # fit_lognormal_from_quantiles() trip its stopifnot(). validate() renders
    # the message in outputs and halts dependants silently, like req().
    problem <- input_problem()
    validate(need(is.null(problem), problem))

    fit_lognormal_from_quantiles(q50 = input$q50,
                                 q90 = input$q90)
  })
  
  # Months between lodgement and a given date
  months_between <- function(from_date, to_date) {
    as.numeric(difftime(to_date, from_date, units = "days")) / 30.4375
  }
  
  # The "active" ceremony date in months after lodgement.
  ceremony_months <- reactive({
    months_between(input$lodgement_date, input$ceremony_date_pick)
  })
  
  # Confidence level implied by the current ceremony date
  implied_confidence <- reactive({
    params <- dist_params()
    m <- ceremony_months()
    if (m <= 0) return(0.5)
    visa_cdf(m, params$mu, params$sigma)
  })
  
  # Map confidence back to impatience (inverse of the forward mapping)
  confidence_to_impatience <- function(conf) {
    imp <- (conf - 0.45) / 0.05
    imp <- max(1, min(10, round(imp)))
    as.integer(imp)
  }
  
  # Map impatience to confidence (forward mapping)
  impatience_to_confidence <- function(imp) {
    0.45 + (imp / 10) * 0.50
  }
  
  # ── Bidirectional sync: slider → date ───────────────────
  observeEvent(input$impatience, {
    if (sync_source() == "date") {
      sync_source("none")
      return()
    }
    
    params <- dist_params()
    target_prob <- impatience_to_confidence(input$impatience)
    d1 <- find_optimal_d1(params$mu, params$sigma, target_prob)
    
    months_int <- round(d1)
    new_date <- seq(
      from       = input$lodgement_date,
      by         = "1 month",
      length.out = months_int + 1
    )[months_int + 1]
    
    sync_source("slider")
    updateDateInput(session, "ceremony_date_pick", value = new_date)
  })
  
  # ── Bidirectional sync: date → slider ───────────────────
  observeEvent(input$ceremony_date_pick, {
    if (sync_source() == "slider") {
      sync_source("none")
      return()
    }
    
    conf <- implied_confidence()
    imp  <- confidence_to_impatience(conf)
    
    sync_source("date")
    updateSliderInput(session, "impatience", value = imp)
  })
  
  # ── Summary data (uses the ceremony date picker as truth) ─
  summary_data <- reactive({
    params <- dist_params()
    mu     <- params$mu
    sigma  <- params$sigma
    
    booking_cost <- input$booking_cost
    resched_cost <- input$resched_cost
    
    d1_opt <- ceremony_months()
    if (d1_opt <= 0) d1_opt <- 1
    
    target_prob <- visa_cdf(d1_opt, mu, sigma)
    
    scenarios <- scenario_analysis(
      d1           = d1_opt,
      mu           = mu,
      sigma        = sigma,
      booking_cost = booking_cost,
      resched_cost = resched_cost
    )
    
    ceremony_date <- input$ceremony_date_pick
    
    list(
      mu            = mu,
      sigma         = sigma,
      d1_opt        = d1_opt,
      target_prob   = target_prob,
      scenarios     = scenarios,
      ceremony_date = ceremony_date,
      booking_cost  = booking_cost,
      resched_cost  = resched_cost
    )
  })
  
  # ── Top Navigation Bar Live Badge ─────────────────────────
  output$nav_status_badge <- renderUI({
    s <- tryCatch(summary_data(), error = function(e) NULL)
    if (is.null(s)) return(NULL)
    
    tags$div(
      class = "wedding-meta-pill",
      fontawesome::fa("calendar-check", fill = "#D4AF37", height = "13px"),
      tags$span(format(s$ceremony_date, "%d %b %Y")),
      tags$span(style = "color: #CBD5E1; margin: 0 4px;", "|"),
      tags$strong(sprintf("%.1f%% confidence", 100 * s$target_prob))
    )
  })

  # ── Results KPI Cards ────────────────────────────────────
  output$results_kpi_cards <- renderUI({
    problem <- input_problem()
    validate(need(is.null(problem), ""))
    
    s  <- summary_data()
    sc <- s$scenarios
    d1 <- s$d1_opt
    conf <- s$target_prob
    expected_cost <- sc$p_on_time * sc$cost_on_time +
      sc$p_resched * sc$cost_resched +
      sc$p_new_book * sc$cost_new_book
    
    risk_badge_class <- if (conf >= 0.90) "badge-on-time" else if (conf >= 0.60) "badge-resched" else "badge-expired"
    risk_label <- if (conf >= 0.90) "Low Risk" else if (conf >= 0.75) "Moderate Risk" else if (conf >= 0.60) "Elevated Risk" else if (conf >= 0.45) "High Risk" else "Very High Risk"
    
    tags$div(
      class = "kpi-grid",
      
      # Card 1: Target Ceremony Date
      tags$div(
        class = "kpi-card",
        tags$div(
          class = "kpi-label",
          tags$span("Target Ceremony"),
          fontawesome::fa("calendar-day", fill = "#8B263E", height = "14px")
        ),
        tags$div(class = "kpi-value", format(s$ceremony_date, "%d %b %Y")),
        tags$div(class = "kpi-subtext", sprintf("%.1f months after lodgement", d1))
      ),
      
      # Card 2: On-Time Probability
      tags$div(
        class = "kpi-card",
        tags$div(
          class = "kpi-label",
          tags$span("On-Time Probability"),
          fontawesome::fa("shield-heart", fill = "#0D9488", height = "14px")
        ),
        tags$div(class = "kpi-value", sprintf("%.1f%%", 100 * conf)),
        tags$div(
          class = "kpi-subtext",
          tags$span(class = paste("scenario-badge", risk_badge_class), risk_label)
        )
      ),
      
      # Card 3: Expected Financial Cost
      tags$div(
        class = "kpi-card",
        tags$div(
          class = "kpi-label",
          tags$span("Expected Cost"),
          fontawesome::fa("sack-dollar", fill = "#D4AF37", height = "14px")
        ),
        tags$div(class = "kpi-value", sprintf("$%s", formatC(expected_cost, format = "f", digits = 0, big.mark = ","))),
        tags$div(class = "kpi-subtext", sprintf("Base $%s + $%s risk buffer",
                 formatC(s$booking_cost, big.mark = ","),
                 formatC(max(0, expected_cost - s$booking_cost), format = "f", digits = 0, big.mark = ",")))
      ),
      
      # Card 4: Grace Window Outcome
      tags$div(
        class = "kpi-card",
        tags$div(
          class = "kpi-label",
          tags$span("Rebooking Grace"),
          fontawesome::fa("clock-rotate-left", fill = "#D97706", height = "14px")
        ),
        tags$div(class = "kpi-value", sprintf("%.1f%%", 100 * sc$p_resched)),
        tags$div(class = "kpi-subtext", sprintf("Within 12mo • Loss risk: %.1f%%", 100 * sc$p_new_book))
      )
    )
  })
  
  # ── Risk assessment panel ───────────────────────────────
  output$risk_assessment <- renderUI({
    problem <- input_problem()
    validate(need(is.null(problem), problem))
    
    s  <- summary_data()
    sc <- s$scenarios
    
    d1 <- s$d1_opt
    conf <- s$target_prob
    
    risk <- if (conf >= 0.90) {
      list(label = "LOW RISK", gradient = "linear-gradient(135deg, #059669 0%, #10b981 100%)", icon = "\u2705",
           desc = "Very likely the visa will be granted well before this date. Proceed with full confidence.")
    } else if (conf >= 0.75) {
      list(label = "MODERATE RISK", gradient = "linear-gradient(135deg, #0284c7 0%, #38bdf8 100%)", icon = "\U0001F44D",
           desc = "Good chance the visa arrives in time with only a small chance of needing to reschedule.")
    } else if (conf >= 0.60) {
      list(label = "ELEVATED RISK", gradient = "linear-gradient(135deg, #d97706 0%, #f59e0b 100%)", icon = "\u26A0\uFE0F",
           desc = "Decent chance you'll need to reschedule. Ensure your vendors allow flexible date adjustments.")
    } else if (conf >= 0.45) {
      list(label = "HIGH RISK", gradient = "linear-gradient(135deg, #ea580c 0%, #f97316 100%)", icon = "\U0001F536",
           desc = "Roughly coin-flip odds. There is a significant chance of incurring reschedule fees.")
    } else {
      list(label = "VERY HIGH RISK", gradient = "linear-gradient(135deg, #dc2626 0%, #ef4444 100%)", icon = "\U0001F534",
           desc = "More likely than not you'll need to reschedule or forfeit the initial booking fee.")
    }
    
    expected_cost <- sc$p_on_time * sc$cost_on_time +
      sc$p_resched * sc$cost_resched +
      sc$p_new_book * sc$cost_new_book
    
    tags$div(
      style = "margin-bottom: 24px;",
      
      # Styled Executive Risk Banner
      tags$div(
        class = "risk-banner-card",
        style = paste0("background: ", risk$gradient, ";"),
        tags$div(
          class = "risk-banner-title",
          tags$span(style = "font-size: 1.5rem;", risk$icon),
          tags$span(tags$strong(risk$label)),
          tags$span(style = "opacity: 0.9; font-weight: normal; font-size: 0.98rem;", paste("—", risk$desc))
        ),
        tags$div(
          class = "risk-banner-pill",
          sprintf("%.1f%% chance visa arrives in time", 100 * conf)
        )
      ),
      
      # Details Card & Scenario Table
      tags$div(
        class = "risk-details-card",
        tags$div(
          style = "display: flex; justify-content: space-between; align-items: baseline; flex-wrap: wrap; gap: 8px; margin-bottom: 12px;",
          tags$div(
            tags$span(style = "font-size: 1.15rem; font-weight: 700; color: #1F2937;",
                      "Target Date: ", format(s$ceremony_date, "%A, %d %B %Y")),
            tags$span(style = "color: #64748B; margin-left: 8px; font-size: 0.95rem;",
                      sprintf("(%.1f months after lodgement)", d1))
          ),
          tags$div(
            style = "font-size: 0.88rem; color: #64748B;",
            sprintf("Model: LogNormal(\u03BC = %.2f, \u03C3 = %.2f)", s$mu, s$sigma)
          )
        ),
        
        # Scenario table
        tags$table(
          class = "wedding-table",
          tags$thead(
            tags$tr(
              tags$th(style = "text-align: left; width: 45%;", "Outcome Scenario"),
              tags$th(style = "text-align: right; width: 25%;", "Probability"),
              tags$th(style = "text-align: right; width: 30%;", "Total Accumulated Cost")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(
                tags$span(class = "scenario-badge badge-on-time",
                          fontawesome::fa("circle-check", fill = "#059669", height = "13px"),
                          "Visa arrives in time"),
                tags$div(style = "font-size: 0.82rem; color: #64748B; margin-top: 4px;",
                         "Granted on or before ceremony date; original booking valid.")
              ),
              tags$td(
                style = "text-align: right; font-weight: 700; color: #059669; font-size: 1.05rem;",
                sprintf("%.1f%%", 100 * sc$p_on_time)
              ),
              tags$td(
                style = "text-align: right; font-family: 'JetBrains Mono', monospace; font-size: 1rem;",
                sprintf("$%s", formatC(sc$cost_on_time, format = "f", digits = 0, big.mark = ","))
              )
            ),
            tags$tr(
              tags$td(
                tags$span(class = "scenario-badge badge-resched",
                          fontawesome::fa("arrows-rotate", fill = "#D97706", height = "13px"),
                          "Reschedule within 12 months"),
                tags$div(style = "font-size: 0.82rem; color: #64748B; margin-top: 4px;",
                         "Granted within grace window; pays booking + flat reschedule fee.")
              ),
              tags$td(
                style = "text-align: right; font-weight: 700; color: #D97706; font-size: 1.05rem;",
                sprintf("%.1f%%", 100 * sc$p_resched)
              ),
              tags$td(
                style = "text-align: right; font-family: 'JetBrains Mono', monospace; font-size: 1rem;",
                sprintf("$%s", formatC(sc$cost_resched, format = "f", digits = 0, big.mark = ","))
              )
            ),
            tags$tr(
              tags$td(
                tags$span(class = "scenario-badge badge-expired",
                          fontawesome::fa("circle-xmark", fill = "#DC2626", height = "13px"),
                          "Window expired, full rebooking"),
                tags$div(style = "font-size: 0.82rem; color: #64748B; margin-top: 4px;",
                         "Exceeds 12 months grace; original booking lost, replacement booking needed.")
              ),
              tags$td(
                style = "text-align: right; font-weight: 700; color: #DC2626; font-size: 1.05rem;",
                sprintf("%.1f%%", 100 * sc$p_new_book)
              ),
              tags$td(
                style = "text-align: right; font-family: 'JetBrains Mono', monospace; font-size: 1rem;",
                sprintf("$%s", formatC(sc$cost_new_book, format = "f", digits = 0, big.mark = ","))
              )
            )
          ),
          tags$tfoot(
            tags$tr(
              tags$td(tags$strong("Expected Financial Cost"),
                      tags$div(style = "font-size: 0.8rem; font-weight: normal; color: #64748B;",
                               "Probability-weighted expectation across all outcomes")),
              tags$td(style = "text-align: right; color: #64748B;", "100.0%"),
              tags$td(
                style = "text-align: right; font-family: 'JetBrains Mono', monospace; font-size: 1.15rem; color: #8B263E;",
                sprintf("$%s", formatC(expected_cost, format = "f", digits = 0, big.mark = ","))
              )
            )
          )
        )
      )
    )
  })
  
  # ── Tradeoff plot ───────────────────────────────────────
  output$tradeoff_plot <- renderPlot({
    params <- dist_params()
    mu     <- params$mu
    sigma  <- params$sigma
    
    booking_cost <- input$booking_cost
    resched_cost <- input$resched_cost
    
    d1_seq <- seq(input$search_range[1],
                  input$search_range[2],
                  by = 0.5)
    
    df <- metrics_grid(
      mu           = mu,
      sigma        = sigma,
      d1_seq       = d1_seq,
      booking_cost = booking_cost,
      resched_cost = resched_cost
    )
    
    s <- summary_data()
    cost_new_total <- booking_cost + resched_cost + booking_cost
    
    df_long <- df %>%
      select(d1, p_on_time, p_resched, p_new_booking) %>%
      pivot_longer(
        cols      = -d1,
        names_to  = "scenario",
        values_to = "probability"
      ) %>%
      mutate(
        scenario = case_when(
          scenario == "p_on_time"     ~ paste0("Visa in time (pay $",
                                               formatC(booking_cost, big.mark = ","), ")"),
          scenario == "p_resched"     ~ paste0("Reschedule (pay $",
                                               formatC(booking_cost + resched_cost, big.mark = ","), ")"),
          scenario == "p_new_booking" ~ paste0("Window expired (pay $",
                                               formatC(cost_new_total, big.mark = ","), ")")
        ),
        scenario = factor(scenario, levels = c(
          paste0("Visa in time (pay $", formatC(booking_cost, big.mark = ","), ")"),
          paste0("Reschedule (pay $", formatC(booking_cost + resched_cost, big.mark = ","), ")"),
          paste0("Window expired (pay $", formatC(cost_new_total, big.mark = ","), ")")
        ))
      )
    
    ggplot(df_long, aes(x = d1, y = probability, fill = scenario)) +
      geom_area(alpha = 0.85) +
      geom_vline(xintercept = s$d1_opt, linetype = "dashed", linewidth = 1.1,
                 colour = "#1F2937") +
      annotate("label", x = s$d1_opt, y = 0.98,
               label = paste0("Your date: ", round(s$d1_opt, 1), " months"),
               fill = "#FAF8F5", colour = "#8B263E", fontface = "bold", size = 4,
               label.padding = unit(0.35, "lines"), label.r = unit(0.25, "lines")) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.05), expand = c(0, 0)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
      scale_fill_manual(values = c("#0D9488", "#D97706", "#E11D48")) +
      labs(
        x     = "Ceremony date (months after lodgement)",
        y     = "Probability",
        fill  = "Outcome Scenario",
        title = "Probability tradeoffs across ceremony timings",
        subtitle = "Earlier = riskier but faster wedding | Later = safer with higher certainty"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", colour = "#8B263E", size = 15),
        plot.subtitle = element_text(colour = "#64748B", size = 11, margin = margin(b = 10)),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "#FAF8F5", colour = "transparent"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#E2E8F0", linetype = "dashed", linewidth = 0.4),
        plot.background = element_rect(fill = "transparent", colour = NA)
      )
  })
  
  output$dist_plot <- renderPlot({
    params <- dist_params()
    mu     <- params$mu
    sigma  <- params$sigma
    s      <- summary_data()
    
    t_seq <- seq(0.1, input$search_range[2] + 12, by = 0.1)
    
    df <- tibble(
      t   = t_seq,
      pdf = visa_pdf(t_seq, mu, sigma),
      cdf = visa_cdf(t_seq, mu, sigma)
    )
    
    ggplot(df, aes(x = t)) +
      geom_area(data = filter(df, t <= s$d1_opt), aes(y = cdf), fill = "#0D9488", alpha = 0.15) +
      geom_line(aes(y = cdf), colour = "#8B263E", linewidth = 1.3) +
      geom_vline(xintercept = s$d1_opt, linetype = "dashed", colour = "#0D9488",
                 linewidth = 1) +
      geom_vline(xintercept = s$d1_opt + 12, linetype = "dotted", colour = "#D97706",
                 linewidth = 1) +
      annotate("label", x = s$d1_opt, y = 0.12,
               label = paste0("Ceremony: ", round(s$d1_opt, 1), "m (", round(100 * s$target_prob, 1), "%)"),
               fill = "#FAF8F5", colour = "#0D9488", fontface = "bold", size = 3.6,
               hjust = if (s$d1_opt > 20) 1.05 else -0.05) +
      annotate("label", x = s$d1_opt + 12, y = 0.25,
               label = paste0("Window ends: ", round(s$d1_opt + 12, 1), "m"),
               fill = "#FAF8F5", colour = "#D97706", fontface = "bold", size = 3.6,
               hjust = -0.05) +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.02), expand = c(0, 0)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
      labs(
        x        = "Months after lodgement",
        y        = "Cumulative probability of visa grant P(T \u2264 t)",
        title    = "Cumulative Visa Grant Probability Curve",
        subtitle = "Green = ceremony date | Amber = 12-month rebooking cutoff"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", colour = "#8B263E", size = 15),
        plot.subtitle = element_text(colour = "#64748B", size = 11, margin = margin(b = 10)),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#E2E8F0", linetype = "dashed", linewidth = 0.4),
        plot.background = element_rect(fill = "transparent", colour = NA)
      )
  })
  
  # ══════════════════════════════════════════════════════════
  # MONTE CARLO TAB
  # ══════════════════════════════════════════════════════════
  
  # Store MC results in a reactiveVal so they persist until re-run
  mc_results <- reactiveVal(NULL)
  mc_summary <- reactiveVal(NULL)
  mc_closed_form <- reactiveVal(NULL)
  
  # Flag for conditionalPanel. Only advertise results while the current inputs
  # are valid, so the tab can never show a simulation that belongs to an input
  # state the app now rejects.
  output$mc_has_results <- reactive({
    is.null(input_problem()) && !is.null(mc_results())
  })
  outputOptions(output, "mc_has_results", suspendWhenHidden = FALSE)

  # Validation banner for the Monte Carlo tab. Appears as soon as the inputs
  # go bad - even when no simulation has ever run - so the tab reflects the
  # current input state rather than the last successful run.
  output$mc_input_problem <- renderUI({
    problem <- input_problem()
    if (is.null(problem)) return(NULL)

    tags$div(
      class = "alert alert-warning",
      style = "margin: 16px 0; padding: 14px 18px; border-radius: 10px; border-left: 5px solid #d97706;",
      tags$strong("Cannot run a simulation: "),
      problem
    )
  })

  # Throw away a saved simulation as soon as the inputs become invalid, so
  # fixing the inputs does not resurrect results computed under old ones.
  observe({
    if (!is.null(input_problem())) {
      mc_results(NULL)
      mc_summary(NULL)
      mc_closed_form(NULL)
    }
  })
  
  # Run simulation on button click
  observeEvent(input$mc_run, {
    s <- summary_data()
    params <- dist_params()
    
    seed_val <- if (input$mc_seed == 0) NULL else input$mc_seed
    
    results <- run_monte_carlo(
      n_sims       = input$mc_n_sims,
      d1           = s$d1_opt,
      mu           = params$mu,
      sigma        = params$sigma,
      booking_cost = s$booking_cost,
      resched_cost = s$resched_cost,
      seed         = seed_val
    )
    
    mc_results(results)
    mc_summary(summarise_monte_carlo(results))
    mc_closed_form(s)
  })

  # ── Monte Carlo KPI Stat Cards ───────────────────────────
  output$mc_kpi_cards <- renderUI({
    req(mc_summary(), mc_closed_form())
    mcs <- mc_summary()
    cf  <- mc_closed_form()
    sc  <- cf$scenarios
    cf_expected <- sc$p_on_time * sc$cost_on_time +
      sc$p_resched * sc$cost_resched +
      sc$p_new_book * sc$cost_new_book
    cost_diff <- abs(mcs$cost_mean - cf_expected)

    tags$div(
      class = "kpi-grid mb-4",
      tags$div(
        class = "kpi-card",
        tags$div(class = "kpi-label", tags$span("Simulations Run"), fontawesome::fa("dice-d20", fill = "#8B263E", height = "14px")),
        tags$div(class = "kpi-value", formatC(mcs$n_sims, format = "d", big.mark = ",")),
        tags$div(class = "kpi-subtext", "Empirical synthetic lifecycles")
      ),
      tags$div(
        class = "kpi-card",
        tags$div(class = "kpi-label", tags$span("Simulated Mean Cost"), fontawesome::fa("dollar-sign", fill = "#0D9488", height = "14px")),
        tags$div(class = "kpi-value", sprintf("$%.2f", mcs$cost_mean)),
        tags$div(class = "kpi-subtext", sprintf("Theory: $%.2f (Delta $%.2f)", cf_expected, cost_diff))
      ),
      tags$div(
        class = "kpi-card",
        tags$div(class = "kpi-label", tags$span("95% Cost Interval"), fontawesome::fa("arrows-left-right", fill = "#D4AF37", height = "14px")),
        tags$div(class = "kpi-value", sprintf("$%.0f \u2013 $%.0f", mcs$cost_ci[1], mcs$cost_ci[2])),
        tags$div(class = "kpi-subtext", "Empirical cost spread")
      ),
      tags$div(
        class = "kpi-card",
        tags$div(class = "kpi-label", tags$span("Standard Error"), fontawesome::fa("chart-line", fill = "#3B82F6", height = "14px")),
        tags$div(class = "kpi-value", sprintf("\u00B1$%.2f", mcs$cost_se)),
        tags$div(class = "kpi-subtext", "Precision of mean cost estimate")
      )
    )
  })
  
  # ── Comparison table: MC vs closed-form ─────────────────
  output$mc_comparison_table <- renderUI({
    req(mc_summary(), mc_closed_form())
    
    mcs <- mc_summary()
    cf  <- mc_closed_form()
    sc  <- cf$scenarios
    
    # Closed-form values
    cf_expected_cost <- sc$p_on_time * sc$cost_on_time +
      sc$p_resched * sc$cost_resched +
      sc$p_new_book * sc$cost_new_book
    
    cf_p_on_time  <- sc$p_on_time
    cf_p_resched  <- sc$p_resched
    cf_p_new_book <- sc$p_new_book
    
    # MC values
    mc_counts <- mcs$scenario_counts
    mc_p_on_time  <- mc_counts$proportion[mc_counts$scenario == "on_time"]
    mc_p_resched  <- mc_counts$proportion[mc_counts$scenario == "reschedule"]
    mc_p_new_book <- mc_counts$proportion[mc_counts$scenario == "new_booking"]
    
    # Handle edge case where a scenario has zero observations
    if (length(mc_p_on_time) == 0)  mc_p_on_time  <- 0
    if (length(mc_p_resched) == 0)  mc_p_resched  <- 0
    if (length(mc_p_new_book) == 0) mc_p_new_book <- 0
    
    # Helper to format a signed difference string
    format_signed <- function(value, is_pct = FALSE, is_cost = FALSE) {
      sign_char <- if (value >= 0) "+" else "\u2212"
      abs_val <- abs(value)
      if (is_pct) {
        paste0(sign_char, sprintf("%.2f pp", abs_val))
      } else if (is_cost) {
        paste0(sign_char, sprintf("$%.2f", abs_val))
      } else {
        paste0(sign_char, sprintf("%.2f", abs_val))
      }
    }
    
    make_row <- function(label, cf_val, mc_val, is_pct = TRUE, is_cost = FALSE) {
      if (is_pct) {
        cf_str <- sprintf("%.2f%%", 100 * cf_val)
        mc_str <- sprintf("%.2f%%", 100 * mc_val)
        diff_str <- format_signed(100 * (mc_val - cf_val), is_pct = TRUE)
      } else if (is_cost) {
        cf_str <- sprintf("$%.2f", cf_val)
        mc_str <- sprintf("$%.2f", mc_val)
        diff_str <- format_signed(mc_val - cf_val, is_cost = TRUE)
      } else {
        cf_str <- sprintf("%.2f", cf_val)
        mc_str <- sprintf("%.2f", mc_val)
        diff_str <- format_signed(mc_val - cf_val)
      }
      
      diff_badge_class <- if (abs(mc_val - cf_val) / max(abs(cf_val), 0.001) < 0.02) {
        "badge-on-time"
      } else if (abs(mc_val - cf_val) / max(abs(cf_val), 0.001) < 0.05) {
        "badge-resched"
      } else {
        "badge-expired"
      }
      
      tags$tr(
        tags$td(style = "padding: 12px 16px; font-weight: 500;", label),
        tags$td(style = "text-align: right; padding: 12px 16px; font-family: 'JetBrains Mono', monospace;", cf_str),
        tags$td(style = "text-align: right; padding: 12px 16px; font-family: 'JetBrains Mono', monospace;", mc_str),
        tags$td(style = "text-align: right; padding: 12px 16px;",
                tags$span(class = paste("scenario-badge", diff_badge_class),
                          style = "font-family: 'JetBrains Mono', monospace;", diff_str))
      )
    }
    
    tags$div(
      tags$table(
        class = "wedding-table",
        tags$thead(
          tags$tr(
            tags$th(style = "text-align: left;", "Metric"),
            tags$th(style = "text-align: right;", "Theoretical Closed-Form"),
            tags$th(style = "text-align: right;", "Monte Carlo Empirical"),
            tags$th(style = "text-align: right;", "Convergence Difference")
          )
        ),
        tags$tbody(
          make_row("P(Visa in time)", cf_p_on_time, mc_p_on_time, is_pct = TRUE),
          make_row("P(Reschedule within 12mo)", cf_p_resched, mc_p_resched, is_pct = TRUE),
          make_row("P(Window expired, rebook)", cf_p_new_book, mc_p_new_book, is_pct = TRUE),
          make_row("Expected Cost", cf_expected_cost, mcs$cost_mean, is_pct = FALSE, is_cost = TRUE),
          make_row("Median grant time (months)", exp(cf$mu), mcs$time_median, is_pct = FALSE, is_cost = FALSE),
          make_row("Mean grant time (months)", exp(cf$mu + cf$sigma^2 / 2), mcs$time_mean, is_pct = FALSE, is_cost = FALSE)
        )
      )
    )
  })
  
  # ── Histogram of simulated grant times ──────────────────
  output$mc_histogram <- renderPlot({
    req(mc_results(), mc_closed_form())
    
    results <- mc_results()
    cf <- mc_closed_form()
    d1 <- cf$d1_opt
    
    # Theoretical PDF overlay
    t_seq <- seq(0.1, max(results$grant_time) * 1.1, length.out = 500)
    theory_df <- tibble(
      t   = t_seq,
      pdf = visa_pdf(t_seq, cf$mu, cf$sigma)
    )
    
    scenario_labels <- c(
      "on_time"     = "Visa in time",
      "reschedule"  = "Reschedule",
      "new_booking" = "Window expired"
    )
    
    ggplot(results, aes(x = grant_time, fill = scenario)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 80, alpha = 0.8, colour = "white", linewidth = 0.2) +
      geom_line(data = theory_df, aes(x = t, y = pdf),
                inherit.aes = FALSE,
                colour = "#1F2937", linewidth = 1.1, linetype = "solid") +
      geom_vline(xintercept = d1, linetype = "dashed", colour = "#0D9488",
                 linewidth = 1) +
      geom_vline(xintercept = d1 + 12, linetype = "dotted", colour = "#D97706",
                 linewidth = 1) +
      annotate("label", x = d1, y = Inf, label = "Ceremony",
               vjust = 1.5, hjust = -0.05, colour = "#0D9488", fill = "#FAF8F5", size = 3.6, fontface = "bold") +
      annotate("label", x = d1 + 12, y = Inf, label = "Window ends",
               vjust = 1.5, hjust = -0.05, colour = "#D97706", fill = "#FAF8F5", size = 3.6, fontface = "bold") +
      scale_fill_manual(
        values = c("on_time" = "#0D9488", "reschedule" = "#D97706", "new_booking" = "#E11D48"),
        labels = scenario_labels
      ) +
      labs(
        x        = "Visa grant time (months after lodgement)",
        y        = "Density",
        fill     = "Outcome Scenario",
        title    = "Simulated Visa Grant Times",
        subtitle = "Empirical histogram colored by outcome | Dark line = theoretical log-normal probability density"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", colour = "#8B263E", size = 15),
        plot.subtitle = element_text(colour = "#64748B", size = 11, margin = margin(b = 10)),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#E2E8F0", linetype = "dashed", linewidth = 0.4),
        plot.background = element_rect(fill = "transparent", colour = NA)
      ) +
      coord_cartesian(xlim = c(0, quantile(results$grant_time, 0.99)))
  })
  
  # ── Cost distribution bar chart ─────────────────────────
  output$mc_cost_plot <- renderPlot({
    req(mc_results(), mc_closed_form())
    
    results <- mc_results()
    cf <- mc_closed_form()
    sc <- cf$scenarios
    
    cost_summary <- results %>%
      count(scenario, cost) %>%
      mutate(
        proportion = n / sum(n),
        label = case_when(
          scenario == "on_time"     ~ sprintf("$%s\nVisa in time",
                                              formatC(cost, format = "f", digits = 0, big.mark = ",")),
          scenario == "reschedule"  ~ sprintf("$%s\nReschedule",
                                              formatC(cost, format = "f", digits = 0, big.mark = ",")),
          scenario == "new_booking" ~ sprintf("$%s\nWindow expired",
                                              formatC(cost, format = "f", digits = 0, big.mark = ","))
        ),
        label = factor(label, levels = unique(label[order(cost)]))
      )
    
    cf_expected <- sc$p_on_time * sc$cost_on_time +
      sc$p_resched * sc$cost_resched +
      sc$p_new_book * sc$cost_new_book
    
    ggplot(cost_summary, aes(x = label, y = proportion, fill = scenario)) +
      geom_col(alpha = 0.9, width = 0.55) +
      geom_label(aes(label = sprintf("%.1f%%\n(%s runs)",
                                     100 * proportion,
                                     formatC(n, format = "d", big.mark = ",")),
                     colour = scenario),
                 fill = "#FFFFFF",
                 vjust = -0.2, size = 3.6, fontface = "bold", linewidth = 0.3) +
      scale_fill_manual(
        values = c("on_time" = "#0D9488", "reschedule" = "#D97706", "new_booking" = "#E11D48"),
        guide  = "none"
      ) +
      scale_colour_manual(
        values = c("on_time" = "#065F46", "reschedule" = "#92400E", "new_booking" = "#991B1B"),
        guide  = "none"
      ) +
      scale_y_continuous(labels = scales::percent_format(),
                         expand = expansion(mult = c(0, 0.2))) +
      labs(
        x        = "",
        y        = "Proportion of simulations",
        title    = "Financial Outcome Breakdown Across Simulations",
        subtitle = sprintf("Observed mean cost: $%.2f (MC) vs $%.2f (Theory)",
                           mean(results$cost), cf_expected)
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", colour = "#8B263E", size = 15),
        plot.subtitle = element_text(colour = "#64748B", size = 11, margin = margin(b = 10)),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#E2E8F0", linetype = "dashed", linewidth = 0.4),
        plot.background = element_rect(fill = "transparent", colour = NA)
      )
  })
  
  # ── Convergence plot ────────────────────────────────────
  output$mc_convergence_plot <- renderPlot({
    req(mc_results(), mc_closed_form())
    
    results <- mc_results()
    cf <- mc_closed_form()
    sc <- cf$scenarios
    
    cf_expected <- sc$p_on_time * sc$cost_on_time +
      sc$p_resched * sc$cost_resched +
      sc$p_new_book * sc$cost_new_book
    
    n <- nrow(results)
    
    # Sample points for the convergence line (don't plot every single point)
    if (n <= 500) {
      idx <- seq_len(n)
    } else {
      idx <- unique(c(
        seq(1, 500, by = 1),
        seq(500, min(5000, n), by = 10),
        seq(5000, n, by = 100),
        n
      ))
      idx <- sort(unique(idx[idx <= n]))
    }
    
    running_mean <- cumsum(results$cost) / seq_len(n)
    
    conv_df <- tibble(
      sim  = idx,
      mean = running_mean[idx]
    )
    
    ggplot(conv_df, aes(x = sim, y = mean)) +
      geom_line(colour = "#3B82F6", linewidth = 1) +
      geom_hline(yintercept = cf_expected, linetype = "dashed",
                 colour = "#E11D48", linewidth = 1) +
      annotate("label", x = n * 0.98, y = cf_expected,
               label = sprintf("Closed-form: $%.2f", cf_expected),
               hjust = 1, vjust = -0.5, colour = "#E11D48", fill = "#FAF8F5", size = 3.6,
               fontface = "bold") +
      scale_x_continuous(labels = scales::comma_format()) +
      labs(
        x        = "Number of simulated iterations",
        y        = "Running average expected cost ($)",
        title    = "Convergence of Expected Cost towards Theoretical Mean",
        subtitle = "Blue line approaches red dashed line as Law of Large Numbers takes effect"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", colour = "#8B263E", size = 15),
        plot.subtitle = element_text(colour = "#64748B", size = 11, margin = margin(b = 10)),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#E2E8F0", linetype = "dashed", linewidth = 0.4),
        plot.background = element_rect(fill = "transparent", colour = NA)
      )
  })
  
  # ── Raw summary text ────────────────────────────────────
  output$mc_raw_summary <- renderPrint({
    req(mc_summary(), mc_closed_form())
    
    mcs <- mc_summary()
    cf  <- mc_closed_form()
    sc  <- cf$scenarios
    
    cf_expected <- sc$p_on_time * sc$cost_on_time +
      sc$p_resched * sc$cost_resched +
      sc$p_new_book * sc$cost_new_book
    
    cat(sprintf("Monte Carlo Simulation Summary (%s runs)\n",
                formatC(mcs$n_sims, format = "d", big.mark = ",")))
    cat(paste(rep("=", 55), collapse = ""), "\n\n")
    
    cat("Scenario breakdown:\n")
    for (i in seq_len(nrow(mcs$scenario_counts))) {
      row <- mcs$scenario_counts[i, ]
      label <- switch(as.character(row$scenario),
                      "on_time"     = "Visa in time   ",
                      "reschedule"  = "Reschedule     ",
                      "new_booking" = "Window expired ")
      cat(sprintf("  %s  %6s runs  (%5.2f%%)\n",
                  label,
                  formatC(row$n, format = "d", big.mark = ","),
                  100 * row$proportion))
    }
    
    cat("\nCost statistics:\n")
    cat(sprintf("  Mean (MC)          = $%.2f\n", mcs$cost_mean))
    cat(sprintf("  Mean (closed-form) = $%.2f\n", cf_expected))
    cat(sprintf("  Std deviation      = $%.2f\n", mcs$cost_sd))
    cat(sprintf("  Standard error     = $%.2f\n", mcs$cost_se))
    cat(sprintf("  Median             = $%.0f\n", mcs$cost_median))
    cat(sprintf("  95%% CI             = [$%.0f, $%.0f]\n",
                mcs$cost_ci[1], mcs$cost_ci[2]))
    
    cat("\nGrant time statistics:\n")
    cat(sprintf("  Mean (MC)          = %.2f months\n", mcs$time_mean))
    cat(sprintf("  Mean (theoretical) = %.2f months\n",
                exp(cf$mu + cf$sigma^2 / 2)))
    cat(sprintf("  Std deviation      = %.2f months\n", mcs$time_sd))
    cat(sprintf("  Median (MC)        = %.2f months\n", mcs$time_median))
    cat(sprintf("  Median (theory)    = %.2f months\n", exp(cf$mu)))
    cat(sprintf("  95%% CI             = [%.1f, %.1f] months\n",
                mcs$time_ci[1], mcs$time_ci[2]))
    
    cat("\nConvergence:\n")
    cat(sprintf("  |MC mean - theory| = $%.4f\n", abs(mcs$cost_mean - cf_expected)))
    cat(sprintf("  Relative error     = %.4f%%\n",
                100 * abs(mcs$cost_mean - cf_expected) / cf_expected))
  })
  
  # ══════════════════════════════════════════════════════════
  # WORKING TAB
  # ══════════════════════════════════════════════════════════
  
  ## Step 1: Distribution fitting
  output$working_fit <- renderPrint({
    s <- summary_data()
    
    cat("Given:\n")
    cat(sprintf("  q50 = %g months  (median processing time)\n", input$q50))
    cat(sprintf("  q90 = %g months  (90th percentile)\n\n", input$q90))
    
    cat("The log-normal distribution has CDF:\n")
    cat("  P(T <= t) = Phi( (ln(t) - mu) / sigma )\n\n")
    
    cat("where Phi is the standard normal CDF.\n\n")
    
    cat("From the median (50th percentile):\n")
    cat(sprintf("  P(T <= %g) = 0.5\n", input$q50))
    cat(sprintf("  => Phi( (ln(%g) - mu) / sigma ) = 0.5\n", input$q50))
    cat(sprintf("  => (ln(%g) - mu) / sigma = 0       [since Phi(0) = 0.5]\n", input$q50))
    cat(sprintf("  => mu = ln(%g) = %.6f\n\n", input$q50, s$mu))
    
    cat("From the 90th percentile:\n")
    cat(sprintf("  P(T <= %g) = 0.9\n", input$q90))
    cat(sprintf("  => Phi( (ln(%g) - mu) / sigma ) = 0.9\n", input$q90))
    cat(sprintf("  => (ln(%g) - mu) / sigma = Phi^-1(0.9) = %.6f\n",
                input$q90, qnorm(0.9)))
    cat(sprintf("  => sigma = (ln(%g) - %.6f) / %.6f\n",
                input$q90, s$mu, qnorm(0.9)))
    cat(sprintf("  => sigma = (%.6f - %.6f) / %.6f\n",
                log(input$q90), s$mu, qnorm(0.9)))
    cat(sprintf("  => sigma = %.6f\n\n", s$sigma))
    
    cat("Result:\n")
    cat(sprintf("  T ~ LogNormal(mu = %.6f, sigma = %.6f)\n", s$mu, s$sigma))
    
    cat("\nVerification:\n")
    cat(sprintf("  P(T <= %g) = %.6f  (should be 0.5)\n",
                input$q50, visa_cdf(input$q50, s$mu, s$sigma)))
    cat(sprintf("  P(T <= %g) = %.6f  (should be 0.9)\n",
                input$q90, visa_cdf(input$q90, s$mu, s$sigma)))
    
    mean_t <- exp(s$mu + s$sigma^2 / 2)
    var_t  <- (exp(s$sigma^2) - 1) * exp(2 * s$mu + s$sigma^2)
    cat(sprintf("\nDerived statistics:\n"))
    cat(sprintf("  Mean processing time  = exp(mu + sigma^2/2) = %.1f months\n", mean_t))
    cat(sprintf("  Std dev               = %.1f months\n", sqrt(var_t)))
    cat(sprintf("  Mode                  = exp(mu - sigma^2) = %.1f months\n",
                exp(s$mu - s$sigma^2)))
  })
  
  ## Step 2: Impatience mapping
  output$working_impatience <- renderPrint({
    s <- summary_data()
    
    cat("Your ceremony date:", format(s$ceremony_date, "%Y-%m-%d"), "\n")
    cat(sprintf("Months after lodgement: %.1f\n", s$d1_opt))
    cat(sprintf("Implied confidence: %.1f%%\n\n", 100 * s$target_prob))
    
    imp <- confidence_to_impatience(s$target_prob)
    cat(sprintf("Equivalent impatience level: %d / 10\n\n", imp))
    
    cat("Mapping formula:\n")
    cat("  target_prob = 0.45 + (impatience / 10) x 0.50\n\n")
    
    cat("Inverse (date -> impatience):\n")
    cat("  confidence  = F(d1)  [CDF at ceremony months]\n")
    cat("  impatience  = round((confidence - 0.45) / 0.05)\n")
    cat("  impatience  = clamp(result, 1, 10)\n\n")
    
    cat("Full mapping table:\n")
    cat("  Impatience  ->  Confidence  ->  Ceremony (months)\n")
    cat("  ----------     ----------     -----------------\n")
    params <- dist_params()
    for (i in 1:10) {
      p <- 0.45 + (i / 10) * 0.50
      d <- find_optimal_d1(params$mu, params$sigma, p)
      marker <- if (i == imp) "  < closest" else ""
      cat(sprintf("  %2d            ->  %5.1f%%       ->  %5.1f months%s\n",
                  i, 100 * p, d, marker))
    }
  })
  
  ## Step 3: Optimal ceremony date
  output$working_optimal <- renderPrint({
    s <- summary_data()
    
    cat("Your selected ceremony date:", format(s$ceremony_date, "%Y-%m-%d"), "\n")
    cat(sprintf("This is d1 = %.4f months after lodgement.\n\n", s$d1_opt))
    
    cat("The confidence level for this date:\n")
    cat(sprintf("  P(T <= d1) = P(T <= %.4f)\n", s$d1_opt))
    cat(sprintf("             = F(%.4f)\n", s$d1_opt))
    cat(sprintf("             = Phi( (ln(%.4f) - %.6f) / %.6f )\n",
                s$d1_opt, s$mu, s$sigma))
    cat(sprintf("             = Phi( (%.6f - %.6f) / %.6f )\n",
                log(s$d1_opt), s$mu, s$sigma))
    cat(sprintf("             = Phi( %.6f )\n",
                (log(s$d1_opt) - s$mu) / s$sigma))
    cat(sprintf("             = %.6f\n", s$target_prob))
    cat(sprintf("             = %.1f%%\n\n", 100 * s$target_prob))
    
    cat("Interpretation:\n")
    cat(sprintf("  There is a %.1f%% chance the visa will be granted\n",
                100 * s$target_prob))
    cat(sprintf("  within %.1f months of lodgement (i.e. by %s).\n",
                s$d1_opt, format(s$ceremony_date, "%Y-%m-%d")))
  })
  
  ## Step 4: Scenario probabilities
  output$working_scenarios <- renderPrint({
    s  <- summary_data()
    sc <- s$scenarios
    
    cat(sprintf("Ceremony date: d1 = %.4f months\n", s$d1_opt))
    cat(sprintf("Rebooking window ends: d1 + 12 = %.4f months\n\n", s$d1_opt + 12))
    
    cat("Let T be the visa processing time (log-normal).\n")
    cat("Let F(t) = P(T <= t) be the CDF.\n\n")
    
    cat("--- Scenario 1: Visa granted in time ---\n")
    cat("  Condition: T <= d1\n")
    cat(sprintf("  P1 = F(d1) = F(%.4f)\n", s$d1_opt))
    cat(sprintf("     = %.6f\n", sc$p_on_time))
    cat(sprintf("     = %.2f%%\n\n", 100 * sc$p_on_time))
    
    cat("--- Scenario 2: Reschedule within 12 months ---\n")
    cat("  Condition: d1 < T <= d1 + 12\n")
    cat(sprintf("  P2 = F(d1 + 12) - F(d1)\n"))
    cat(sprintf("     = F(%.4f) - F(%.4f)\n", s$d1_opt + 12, s$d1_opt))
    cat(sprintf("     = %.6f - %.6f\n",
                visa_cdf(s$d1_opt + 12, s$mu, s$sigma),
                visa_cdf(s$d1_opt, s$mu, s$sigma)))
    cat(sprintf("     = %.6f\n", sc$p_resched))
    cat(sprintf("     = %.2f%%\n\n", 100 * sc$p_resched))
    
    cat("--- Scenario 3: Window expired, new booking ---\n")
    cat("  Condition: T > d1 + 12\n")
    cat(sprintf("  P3 = 1 - F(d1 + 12)\n"))
    cat(sprintf("     = 1 - F(%.4f)\n", s$d1_opt + 12))
    cat(sprintf("     = 1 - %.6f\n",
                visa_cdf(s$d1_opt + 12, s$mu, s$sigma)))
    cat(sprintf("     = %.6f\n", sc$p_new_book))
    cat(sprintf("     = %.2f%%\n\n", 100 * sc$p_new_book))
    
    cat("Verification: P1 + P2 + P3 = 1\n")
    cat(sprintf("  %.6f + %.6f + %.6f = %.6f\n",
                sc$p_on_time, sc$p_resched, sc$p_new_book,
                sc$p_on_time + sc$p_resched + sc$p_new_book))
  })
  
  ## Step 5: Cost breakdown
  output$working_costs <- renderPrint({
    s  <- summary_data()
    sc <- s$scenarios
    
    b <- s$booking_cost
    r <- s$resched_cost
    
    cat("Input costs:\n")
    cat(sprintf("  Booking cost (B)    = $%s\n",
                formatC(b, format = "f", digits = 0, big.mark = ",")))
    cat(sprintf("  Reschedule fee (R)  = $%s\n\n",
                formatC(r, format = "f", digits = 0, big.mark = ",")))
    
    cat("Costs are CUMULATIVE -- money already spent is not refunded.\n\n")
    
    cat("--- Scenario 1: Visa in time ---\n")
    cat(sprintf("  Cost1 = B = $%s\n\n",
                formatC(sc$cost_on_time, format = "f", digits = 0, big.mark = ",")))
    
    cat("--- Scenario 2: Reschedule ---\n")
    cat("  You already paid B. Now you pay R to reschedule.\n")
    cat(sprintf("  Cost2 = B + R = $%s + $%s = $%s\n\n",
                formatC(b, format = "f", digits = 0, big.mark = ","),
                formatC(r, format = "f", digits = 0, big.mark = ","),
                formatC(sc$cost_resched, format = "f", digits = 0, big.mark = ",")))
    
    cat("--- Scenario 3: Window expired ---\n")
    cat("  You already paid B + R. The 12-month window expired,\n")
    cat("  so you need a completely new booking (B again).\n")
    cat(sprintf("  Cost3 = B + R + B = $%s + $%s + $%s = $%s\n\n",
                formatC(b, format = "f", digits = 0, big.mark = ","),
                formatC(r, format = "f", digits = 0, big.mark = ","),
                formatC(b, format = "f", digits = 0, big.mark = ","),
                formatC(sc$cost_new_book, format = "f", digits = 0, big.mark = ",")))
    
    cat("--- Expected cost ---\n")
    expected <- sc$p_on_time * sc$cost_on_time +
      sc$p_resched * sc$cost_resched +
      sc$p_new_book * sc$cost_new_book
    cat("  E[Cost] = P1 x Cost1 + P2 x Cost2 + P3 x Cost3\n")
    cat(sprintf("          = %.4f x $%s + %.4f x $%s + %.4f x $%s\n",
                sc$p_on_time,
                formatC(sc$cost_on_time, format = "f", digits = 0, big.mark = ","),
                sc$p_resched,
                formatC(sc$cost_resched, format = "f", digits = 0, big.mark = ","),
                sc$p_new_book,
                formatC(sc$cost_new_book, format = "f", digits = 0, big.mark = ",")))
    cat(sprintf("          = $%.2f + $%.2f + $%.2f\n",
                sc$p_on_time * sc$cost_on_time,
                sc$p_resched * sc$cost_resched,
                sc$p_new_book * sc$cost_new_book))
    cat(sprintf("          = $%.2f\n", expected))
  })
  
  ## Reference formulas (HTML)
  output$working_formulas <- renderUI({
    tags$div(
      style = "font-family: 'JetBrains Mono', monospace; font-size: 13px; line-height: 1.8;
               background: #F8FAFC; padding: 20px; border-radius: 12px;
               border: 1px solid #E2E8F0;",
      
      tags$div(style = "font-weight: 700; color: #8B263E; font-size: 14px; margin-bottom: 8px;",
               "1. Log-normal Distribution"),
      tags$p("If T ~ LogNormal(\u03BC, \u03C3), then ln(T) ~ Normal(\u03BC, \u03C3\u00B2)"),
      tags$p("CDF:      F(t) = \u03A6( (ln(t) - \u03BC) / \u03C3 )"),
      tags$p("PDF:      f(t) = (1 / (t \u03C3 \u221A(2\u03C0))) exp(-(ln(t) - \u03BC)\u00B2 / (2 \u03C3\u00B2))"),
      tags$p("Quantile: Q(p) = exp(\u03BC + \u03C3 \u03A6\u207B\u00B9(p))"),
      
      tags$hr(style = "border-color: rgba(212, 175, 55, 0.3); margin: 16px 0;"),
      
      tags$div(style = "font-weight: 700; color: #8B263E; font-size: 14px; margin-bottom: 8px;",
               "2. Parameter Estimation from Published Quantiles"),
      tags$p("Given q50 (median) and q90 (90th percentile):"),
      tags$p("  \u03BC     = ln(q50)"),
      tags$p("  \u03C3     = (ln(q90) - \u03BC) / \u03A6\u207B\u00B9(0.9)"),
      tags$p(sprintf("  \u03A6\u207B\u00B9(0.9) \u2248 %.6f", qnorm(0.9))),
      
      tags$hr(style = "border-color: rgba(212, 175, 55, 0.3); margin: 16px 0;"),
      
      tags$div(style = "font-weight: 700; color: #8B263E; font-size: 14px; margin-bottom: 8px;",
               "3. Scenario Probabilities"),
      tags$p("P1 = F(d1)                -- Visa granted before ceremony date"),
      tags$p("P2 = F(d1 + 12) - F(d1)    -- Visa granted within 12-month rebooking grace window"),
      tags$p("P3 = 1 - F(d1 + 12)         -- Visa not granted after window; original booking lost"),
      tags$p("P1 + P2 + P3 = 1.0"),
      
      tags$hr(style = "border-color: rgba(212, 175, 55, 0.3); margin: 16px 0;"),
      
      tags$div(style = "font-weight: 700; color: #8B263E; font-size: 14px; margin-bottom: 8px;",
               "4. Cumulative Financial Costs & Expectation"),
      tags$p("Cost1 = Booking Cost (B)"),
      tags$p("Cost2 = Booking Cost + Reschedule Fee (B + R)"),
      tags$p("Cost3 = B + R + B (Cumulative: booking + reschedule + replacement booking)"),
      tags$p("E[Cost] = P1 \u00D7 Cost1 + P2 \u00D7 Cost2 + P3 \u00D7 Cost3"),
      
      tags$hr(style = "border-color: rgba(212, 175, 55, 0.3); margin: 16px 0;"),
      
      tags$div(style = "font-weight: 700; color: #8B263E; font-size: 14px; margin-bottom: 8px;",
               "5. Impatience Mapping & Bidirectional Synchronisation"),
      tags$p("target_prob = 0.45 + (impatience / 10) \u00D7 0.50   [Range: 1 \u2192 50%, 10 \u2192 95%]"),
      tags$p("Slider \u2192 Date:  d1 = Q(target_prob), then convert months to calendar date"),
      tags$p("Date \u2192 Slider:  confidence = F(d1), then impatience = round((confidence - 0.45) / 0.05)")
    )
  })
})
