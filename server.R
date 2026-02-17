## server.R

library(shiny)
library(tidyverse)

shinyServer(function(input, output, session) {
  
  # ── Sync flag to prevent infinite loops ─────────────────
  sync_source <- reactiveVal("none")  # "slider", "date", or "none"
  
  # ── Reactives ───────────────────────────────────────────
  
  dist_params <- reactive({
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
  
  # ── Risk assessment panel ───────────────────────────────
  output$risk_assessment <- renderUI({
    s  <- summary_data()
    sc <- s$scenarios
    
    d1 <- s$d1_opt
    conf <- s$target_prob
    
    risk <- if (conf >= 0.90) {
      list(label = "LOW RISK", colour = "#27ae60", icon = "\u2705",
           desc = "Very likely the visa will be granted before this date.")
    } else if (conf >= 0.75) {
      list(label = "MODERATE RISK", colour = "#2980b9", icon = "\U0001F44D",
           desc = "Good chance the visa arrives in time, small chance of rescheduling.")
    } else if (conf >= 0.60) {
      list(label = "ELEVATED RISK", colour = "#f39c12", icon = "\u26A0\uFE0F",
           desc = "Decent chance you'll need to reschedule.")
    } else if (conf >= 0.45) {
      list(label = "HIGH RISK", colour = "#e67e22", icon = "\U0001F536",
           desc = "Roughly coin-flip odds. Significant chance of extra costs.")
    } else {
      list(label = "VERY HIGH RISK", colour = "#e74c3c", icon = "\U0001F534",
           desc = "More likely than not you'll need to reschedule or rebook.")
    }
    
    expected_cost <- sc$p_on_time * sc$cost_on_time +
      sc$p_resched * sc$cost_resched +
      sc$p_new_book * sc$cost_new_book
    
    tags$div(
      style = "margin-bottom: 20px;",
      
      # Risk banner
      tags$div(
        style = paste0(
          "background: ", risk$colour, "; color: white; padding: 16px 20px; ",
          "border-radius: 8px; margin-bottom: 16px; font-size: 16px;"
        ),
        tags$span(style = "font-size: 24px; margin-right: 10px;", risk$icon),
        tags$strong(risk$label),
        tags$span(
          style = "margin-left: 16px;",
          sprintf("%.1f%% chance visa arrives in time", 100 * conf)
        )
      ),
      
      # Details card
      tags$div(
        style = "background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px; padding: 20px;",
        
        tags$p(style = "font-size: 15px; margin-bottom: 12px;",
               tags$strong("Ceremony date: "),
               format(s$ceremony_date, "%A, %d %B %Y"),
               tags$span(style = "color: #666; margin-left: 8px;",
                         sprintf("(%.1f months after lodgement)", d1))),
        
        tags$p(style = "font-size: 14px; color: #555; margin-bottom: 16px;",
               risk$desc),
        
        # Scenario table
        tags$table(
          style = "width: 100%; border-collapse: collapse; font-size: 14px;",
          tags$thead(
            tags$tr(
              style = "border-bottom: 2px solid #dee2e6;",
              tags$th(style = "text-align: left; padding: 8px;", "Scenario"),
              tags$th(style = "text-align: right; padding: 8px;", "Chance"),
              tags$th(style = "text-align: right; padding: 8px;", "Total cost")
            )
          ),
          tags$tbody(
            tags$tr(
              style = "border-bottom: 1px solid #eee;",
              tags$td(style = "padding: 8px;", "\u2705 Visa in time"),
              tags$td(style = "text-align: right; padding: 8px; font-weight: bold; color: #27ae60;",
                      sprintf("%.1f%%", 100 * sc$p_on_time)),
              tags$td(style = "text-align: right; padding: 8px;",
                      sprintf("$%s", formatC(sc$cost_on_time, format = "f", digits = 0, big.mark = ",")))
            ),
            tags$tr(
              style = "border-bottom: 1px solid #eee;",
              tags$td(style = "padding: 8px;", "\U0001F504 Reschedule within 12 months"),
              tags$td(style = "text-align: right; padding: 8px; font-weight: bold; color: #f39c12;",
                      sprintf("%.1f%%", 100 * sc$p_resched)),
              tags$td(style = "text-align: right; padding: 8px;",
                      sprintf("$%s", formatC(sc$cost_resched, format = "f", digits = 0, big.mark = ",")))
            ),
            tags$tr(
              tags$td(style = "padding: 8px;", "\u274C Window expired, rebook"),
              tags$td(style = "text-align: right; padding: 8px; font-weight: bold; color: #e74c3c;",
                      sprintf("%.1f%%", 100 * sc$p_new_book)),
              tags$td(style = "text-align: right; padding: 8px;",
                      sprintf("$%s", formatC(sc$cost_new_book, format = "f", digits = 0, big.mark = ",")))
            )
          ),
          tags$tfoot(
            tags$tr(
              style = "border-top: 2px solid #dee2e6; font-weight: bold;",
              tags$td(style = "padding: 8px;", "Expected cost"),
              tags$td(style = "text-align: right; padding: 8px;", ""),
              tags$td(style = "text-align: right; padding: 8px;",
                      sprintf("$%s", formatC(expected_cost, format = "f", digits = 0, big.mark = ",")))
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
      geom_area(alpha = 0.8) +
      geom_vline(xintercept = s$d1_opt, linetype = "dashed", linewidth = 0.8,
                 colour = "black") +
      annotate("text", x = s$d1_opt, y = 1.02,
               label = paste0("Your date: ", round(s$d1_opt, 1), " months"),
               hjust = -0.05, size = 4, fontface = "bold") +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.05)) +
      scale_fill_manual(values = c("#2ecc71", "#f39c12", "#e74c3c")) +
      labs(
        x     = "Ceremony date (months after lodgement)",
        y     = "Probability",
        fill  = "Scenario (and total cost)",
        title = "How ceremony timing affects your chances and costs",
        subtitle = "Earlier = riskier but sooner | Later = safer but longer wait"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom",
            legend.direction = "vertical")
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
      geom_line(aes(y = cdf), colour = "#3498db", linewidth = 1.2) +
      geom_vline(xintercept = s$d1_opt, linetype = "dashed", colour = "#e74c3c",
                 linewidth = 0.8) +
      geom_vline(xintercept = s$d1_opt + 12, linetype = "dotted", colour = "#e67e22",
                 linewidth = 0.8) +
      annotate("text", x = s$d1_opt, y = 0.05,
               label = paste0("Ceremony: ", round(s$d1_opt, 1), "m"),
               hjust = -0.1, colour = "#e74c3c", size = 3.5) +
      annotate("text", x = s$d1_opt + 12, y = 0.05,
               label = paste0("Window ends: ", round(s$d1_opt + 12, 1), "m"),
               hjust = -0.1, colour = "#e67e22", size = 3.5) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        x        = "Months after lodgement",
        y        = "Cumulative probability of visa granted",
        title    = "Visa processing time distribution",
        subtitle = "Red = your ceremony date | Orange = 12-month rebooking window end"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # ══════════════════════════════════════════════════════════
  # MONTE CARLO TAB
  # ══════════════════════════════════════════════════════════
  
  # Store MC results in a reactiveVal so they persist until re-run
  mc_results <- reactiveVal(NULL)
  mc_summary <- reactiveVal(NULL)
  mc_closed_form <- reactiveVal(NULL)
  
  # Flag for conditionalPanel
  output$mc_has_results <- reactive({
    !is.null(mc_results())
  })
  outputOptions(output, "mc_has_results", suspendWhenHidden = FALSE)
  
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
      
      diff_colour <- if (abs(mc_val - cf_val) / max(abs(cf_val), 0.001) < 0.02) {
        "#27ae60"
      } else if (abs(mc_val - cf_val) / max(abs(cf_val), 0.001) < 0.05) {
        "#f39c12"
      } else {
        "#e74c3c"
      }
      
      tags$tr(
        style = "border-bottom: 1px solid #eee;",
        tags$td(style = "padding: 8px;", label),
        tags$td(style = "text-align: right; padding: 8px; font-family: monospace;", cf_str),
        tags$td(style = "text-align: right; padding: 8px; font-family: monospace;", mc_str),
        tags$td(style = paste0("text-align: right; padding: 8px; font-family: monospace; color: ", diff_colour, ";"),
                diff_str)
      )
    }
    
    tags$div(
      tags$div(
        style = "background: #f0f7ff; border: 1px solid #b8d4f0; border-radius: 8px; padding: 12px 16px; margin-bottom: 16px;",
        tags$p(style = "margin: 0; font-size: 14px;",
               sprintf("Ran %s simulations. Standard error of mean cost: $%.2f",
                       formatC(mcs$n_sims, format = "d", big.mark = ","),
                       mcs$cost_se)),
        tags$p(style = "margin: 4px 0 0 0; font-size: 13px; color: #666;",
               "Green = within 2% of theory | Orange = within 5% | Red = >5% deviation")
      ),
      
      tags$table(
        style = "width: 100%; border-collapse: collapse; font-size: 14px;",
        tags$thead(
          tags$tr(
            style = "border-bottom: 2px solid #dee2e6;",
            tags$th(style = "text-align: left; padding: 8px;", "Metric"),
            tags$th(style = "text-align: right; padding: 8px;", "Closed-form"),
            tags$th(style = "text-align: right; padding: 8px;", "Monte Carlo"),
            tags$th(style = "text-align: right; padding: 8px;", "Difference")
          )
        ),
        tags$tbody(
          make_row("P(Visa in time)", cf_p_on_time, mc_p_on_time, is_pct = TRUE),
          make_row("P(Reschedule)", cf_p_resched, mc_p_resched, is_pct = TRUE),
          make_row("P(Window expired)", cf_p_new_book, mc_p_new_book, is_pct = TRUE),
          tags$tr(style = "border-top: 1px solid #ccc;",
            tags$td(colspan = "4", style = "padding: 4px;", "")
          ),
          make_row("Expected cost", cf_expected_cost, mcs$cost_mean, is_pct = FALSE, is_cost = TRUE),
          make_row("Median grant time (months)",
                   exp(cf$mu),
                   mcs$time_median,
                   is_pct = FALSE, is_cost = FALSE),
          make_row("Mean grant time (months)",
                   exp(cf$mu + cf$sigma^2 / 2),
                   mcs$time_mean,
                   is_pct = FALSE, is_cost = FALSE)
        )
      ),
      
      tags$div(
        style = "margin-top: 12px; padding: 12px 16px; background: #f8f9fa; border-radius: 6px; font-size: 13px;",
        tags$p(style = "margin: 0;",
               tags$strong("95% empirical cost interval: "),
               sprintf("$%.0f \u2013 $%.0f", mcs$cost_ci[1], mcs$cost_ci[2])),
        tags$p(style = "margin: 4px 0 0 0;",
               tags$strong("95% empirical grant time interval: "),
               sprintf("%.1f \u2013 %.1f months", mcs$time_ci[1], mcs$time_ci[2]))
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
                     bins = 80, alpha = 0.7, colour = "white", linewidth = 0.2) +
      geom_line(data = theory_df, aes(x = t, y = pdf),
                inherit.aes = FALSE,
                colour = "black", linewidth = 1, linetype = "solid") +
      geom_vline(xintercept = d1, linetype = "dashed", colour = "#e74c3c",
                 linewidth = 0.8) +
      geom_vline(xintercept = d1 + 12, linetype = "dotted", colour = "#e67e22",
                 linewidth = 0.8) +
      annotate("text", x = d1, y = Inf, label = "Ceremony",
               vjust = 2, hjust = -0.1, colour = "#e74c3c", size = 3.5, fontface = "bold") +
      annotate("text", x = d1 + 12, y = Inf, label = "Window ends",
               vjust = 2, hjust = -0.1, colour = "#e67e22", size = 3.5, fontface = "bold") +
      scale_fill_manual(
        values = c("on_time" = "#2ecc71", "reschedule" = "#f39c12", "new_booking" = "#e74c3c"),
        labels = scenario_labels
      ) +
      labs(
        x        = "Visa grant time (months after lodgement)",
        y        = "Density",
        fill     = "Scenario",
        title    = "Simulated visa grant times",
        subtitle = "Histogram coloured by scenario | Black curve = theoretical log-normal PDF"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") +
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
      geom_col(alpha = 0.85, width = 0.6) +
      geom_text(aes(label = sprintf("%.1f%%\n(%s runs)",
                                     100 * proportion,
                                     formatC(n, format = "d", big.mark = ","))),
                vjust = -0.3, size = 3.5) +
      scale_fill_manual(
        values = c("on_time" = "#2ecc71", "reschedule" = "#f39c12", "new_booking" = "#e74c3c"),
        guide  = "none"
      ) +
      scale_y_continuous(labels = scales::percent_format(),
                         expand = expansion(mult = c(0, 0.15))) +
      labs(
        x        = "",
        y        = "Proportion of simulations",
        title    = "Cost outcomes across all simulations",
        subtitle = sprintf("Expected cost: $%.0f (MC) vs $%.0f (closed-form)",
                           mean(results$cost), cf_expected)
      ) +
      theme_minimal(base_size = 14)
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
      geom_line(colour = "#3498db", linewidth = 0.8) +
      geom_hline(yintercept = cf_expected, linetype = "dashed",
                 colour = "#e74c3c", linewidth = 0.8) +
      annotate("text", x = n * 0.98, y = cf_expected,
               label = sprintf("Closed-form: $%.2f", cf_expected),
               hjust = 1, vjust = -0.8, colour = "#e74c3c", size = 3.5,
               fontface = "bold") +
      scale_x_continuous(labels = scales::comma_format()) +
      labs(
        x        = "Number of simulations",
        y        = "Running average cost ($)",
        title    = "Convergence of Monte Carlo expected cost",
        subtitle = "Blue line should approach the red dashed line (closed-form solution)"
      ) +
      theme_minimal(base_size = 14)
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
      style = "font-family: monospace; font-size: 13px; line-height: 1.8;
               background: #f8f9fa; padding: 16px; border-radius: 6px;
               border: 1px solid #dee2e6;",
      
      tags$p(tags$strong("Log-normal distribution")),
      tags$p("If T ~ LogNormal(mu, sigma), then ln(T) ~ Normal(mu, sigma^2)"),
      tags$p("CDF:  F(t) = Phi( (ln(t) - mu) / sigma )"),
      tags$p("PDF:  f(t) = (1 / (t sigma sqrt(2 pi))) exp(-(ln(t) - mu)^2 / (2 sigma^2))"),
      tags$p("Quantile:  Q(p) = exp(mu + sigma Phi^-1(p))"),
      
      tags$hr(),
      
      tags$p(tags$strong("Parameter estimation from quantiles")),
      tags$p("Given q50 (median) and q90 (90th percentile):"),
      tags$p("  mu = ln(q50)"),
      tags$p("  sigma = (ln(q90) - mu) / Phi^-1(0.9)"),
      tags$p(sprintf("  Phi^-1(0.9) = %.6f", qnorm(0.9))),
      
      tags$hr(),
      
      tags$p(tags$strong("Scenario probabilities")),
      tags$p("P1 = F(d1)                    -- visa before ceremony"),
      tags$p("P2 = F(d1 + 12) - F(d1)      -- visa within reschedule window"),
      tags$p("P3 = 1 - F(d1 + 12)           -- visa after window expires"),
      tags$p("P1 + P2 + P3 = 1"),
      
      tags$hr(),
      
      tags$p(tags$strong("Cumulative costs")),
      tags$p("Cost1 = B"),
      tags$p("Cost2 = B + R"),
      tags$p("Cost3 = B + R + B"),
      tags$p("E[Cost] = P1 * Cost1 + P2 * Cost2 + P3 * Cost3"),
      
      tags$hr(),
      
      tags$p(tags$strong("Impatience mapping")),
      tags$p("target_prob = 0.45 + (impatience / 10) x 0.50"),
      tags$p("Range: impatience 1 -> 50%  ...  impatience 10 -> 95%"),
      
      tags$hr(),
      
      tags$p(tags$strong("Bidirectional sync")),
      tags$p("Slider -> Date:  d1 = Q(target_prob), then convert months to calendar date"),
      tags$p("Date -> Slider:  confidence = F(d1), then impatience = round((confidence - 0.45) / 0.05)")
    )
  })
})
