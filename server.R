## server.R

library(shiny)
library(tidyverse)

shinyServer(function(input, output, session) {
  
  # ── Sync flag to prevent infinite loops ─────────────────
  # When one input programmatically updates the other, we set

  # this flag so the observer for the *other* input knows to
  # skip its update cycle.
  
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
  # This is the single source of truth — both the slider and

  # the date picker feed into it.
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
    # target_prob = 0.45 + (impatience / 10) * 0.50
    # impatience  = (target_prob - 0.45) / 0.05
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
    
    # Risk level label and colour
    risk <- if (conf >= 0.90) {
      list(label = "LOW RISK", colour = "#27ae60", icon = "✅",
           desc = "Very likely the visa will be granted before this date.")
    } else if (conf >= 0.75) {
      list(label = "MODERATE RISK", colour = "#2980b9", icon = "👍",
           desc = "Good chance the visa arrives in time, small chance of rescheduling.")
    } else if (conf >= 0.60) {
      list(label = "ELEVATED RISK", colour = "#f39c12", icon = "⚠️",
           desc = "Decent chance you'll need to reschedule.")
    } else if (conf >= 0.45) {
      list(label = "HIGH RISK", colour = "#e67e22", icon = "🔶",
           desc = "Roughly coin-flip odds. Significant chance of extra costs.")
    } else {
      list(label = "VERY HIGH RISK", colour = "#e74c3c", icon = "🔴",
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
              tags$td(style = "padding: 8px;", "✅ Visa in time"),
              tags$td(style = "text-align: right; padding: 8px; font-weight: bold; color: #27ae60;",
                      sprintf("%.1f%%", 100 * sc$p_on_time)),
              tags$td(style = "text-align: right; padding: 8px;",
                      sprintf("$%s", formatC(sc$cost_on_time, format = "f", digits = 0, big.mark = ",")))
            ),
            tags$tr(
              style = "border-bottom: 1px solid #eee;",
              tags$td(style = "padding: 8px;", "🔄 Reschedule within 12 months"),
              tags$td(style = "text-align: right; padding: 8px; font-weight: bold; color: #f39c12;",
                      sprintf("%.1f%%", 100 * sc$p_resched)),
              tags$td(style = "text-align: right; padding: 8px;",
                      sprintf("$%s", formatC(sc$cost_resched, format = "f", digits = 0, big.mark = ",")))
            ),
            tags$tr(
              tags$td(style = "padding: 8px;", "❌ Window expired, rebook"),
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
  
  # ── Working tab outputs ─────────────────────────────────
  
  ## Step 1: Distribution fitting
  output$working_fit <- renderPrint({
    s <- summary_data()
    
    cat("Given:\n")
    cat(sprintf("  q50 = %g months  (median processing time)\n", input$q50))
    cat(sprintf("  q90 = %g months  (90th percentile)\n\n", input$q90))
    
    cat("The log-normal distribution has CDF:\n")
    cat("  P(T ≤ t) = Φ( (ln(t) - μ) / σ )\n\n")
    
    cat("where Φ is the standard normal CDF.\n\n")
    
    cat("From the median (50th percentile):\n")
    cat(sprintf("  P(T ≤ %g) = 0.5\n", input$q50))
    cat(sprintf("  ⟹  Φ( (ln(%g) - μ) / σ ) = 0.5\n", input$q50))
    cat(sprintf("  ⟹  (ln(%g) - μ) / σ = 0       [since Φ(0) = 0.5]\n", input$q50))
    cat(sprintf("  ⟹  μ = ln(%g) = %.6f\n\n", input$q50, s$mu))
    
    cat("From the 90th percentile:\n")
    cat(sprintf("  P(T ≤ %g) = 0.9\n", input$q90))
    cat(sprintf("  ⟹  Φ( (ln(%g) - μ) / σ ) = 0.9\n", input$q90))
    cat(sprintf("  ⟹  (ln(%g) - μ) / σ = Φ⁻¹(0.9) = %.6f\n",
                input$q90, qnorm(0.9)))
    cat(sprintf("  ⟹  σ = (ln(%g) - %.6f) / %.6f\n",
                input$q90, s$mu, qnorm(0.9)))
    cat(sprintf("  ⟹  σ = (%.6f - %.6f) / %.6f\n",
                log(input$q90), s$mu, qnorm(0.9)))
    cat(sprintf("  ⟹  σ = %.6f\n\n", s$sigma))
    
    cat("Result:\n")
    cat(sprintf("  T ~ LogNormal(μ = %.6f, σ = %.6f)\n", s$mu, s$sigma))
    
    # Sanity checks
    cat("\nVerification:\n")
    cat(sprintf("  P(T ≤ %g) = %.6f  ✓ (should be 0.5)\n",
                input$q50, visa_cdf(input$q50, s$mu, s$sigma)))
    cat(sprintf("  P(T ≤ %g) = %.6f  ✓ (should be 0.9)\n",
                input$q90, visa_cdf(input$q90, s$mu, s$sigma)))
    
    # Derived stats
    mean_t <- exp(s$mu + s$sigma^2 / 2)
    var_t  <- (exp(s$sigma^2) - 1) * exp(2 * s$mu + s$sigma^2)
    cat(sprintf("\nDerived statistics:\n"))
    cat(sprintf("  Mean processing time  = exp(μ + σ²/2) = %.1f months\n", mean_t))
    cat(sprintf("  Std dev               = %.1f months\n", sqrt(var_t)))
    cat(sprintf("  Mode                  = exp(μ - σ²) = %.1f months\n",
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
    cat("  target_prob = 0.45 + (impatience / 10) × 0.50\n\n")
    
    cat("Inverse (date → impatience):\n")
    cat("  confidence  = F(d₁)  [CDF at ceremony months]\n")
    cat("  impatience  = round((confidence - 0.45) / 0.05)\n")
    cat("  impatience  = clamp(result, 1, 10)\n\n")
    
    cat("Full mapping table:\n")
    cat("  Impatience  →  Confidence  →  Ceremony (months)\n")
    cat("  ──────────     ──────────     ─────────────────\n")
    params <- dist_params()
    for (i in 1:10) {
      p <- 0.45 + (i / 10) * 0.50
      d <- find_optimal_d1(params$mu, params$sigma, p)
      marker <- if (i == imp) "  ◀ closest" else ""
      cat(sprintf("  %2d            →  %5.1f%%       →  %5.1f months%s\n",
                  i, 100 * p, d, marker))
    }
  })
  
  ## Step 3: Optimal ceremony date
  output$working_optimal <- renderPrint({
    s <- summary_data()
    
    cat("Your selected ceremony date:", format(s$ceremony_date, "%Y-%m-%d"), "\n")
    cat(sprintf("This is d₁ = %.4f months after lodgement.\n\n", s$d1_opt))
    
    cat("The confidence level for this date:\n")
    cat(sprintf("  P(T ≤ d₁) = P(T ≤ %.4f)\n", s$d1_opt))
    cat(sprintf("            = F(%.4f)\n", s$d1_opt))
    cat(sprintf("            = Φ( (ln(%.4f) - %.6f) / %.6f )\n",
                s$d1_opt, s$mu, s$sigma))
    cat(sprintf("            = Φ( (%.6f - %.6f) / %.6f )\n",
                log(s$d1_opt), s$mu, s$sigma))
    cat(sprintf("            = Φ( %.6f )\n",
                (log(s$d1_opt) - s$mu) / s$sigma))
    cat(sprintf("            = %.6f\n", s$target_prob))
    cat(sprintf("            = %.1f%%\n\n", 100 * s$target_prob))
    
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
    
    cat(sprintf("Ceremony date: d₁ = %.4f months\n", s$d1_opt))
    cat(sprintf("Rebooking window ends: d₁ + 12 = %.4f months\n\n", s$d1_opt + 12))
    
    cat("Let T be the visa processing time (log-normal).\n")
    cat("Let F(t) = P(T ≤ t) be the CDF.\n\n")
    
    cat("─── Scenario 1: Visa granted in time ───\n")
    cat("  Condition: T ≤ d₁\n")
    cat(sprintf("  P₁ = F(d₁) = F(%.4f)\n", s$d1_opt))
    cat(sprintf("     = %.6f\n", sc$p_on_time))
    cat(sprintf("     = %.2f%%\n\n", 100 * sc$p_on_time))
    
    cat("─── Scenario 2: Reschedule within 12 months ───\n")
    cat("  Condition: d₁ < T ≤ d₁ + 12\n")
    cat(sprintf("  P₂ = F(d₁ + 12) - F(d₁)\n"))
    cat(sprintf("     = F(%.4f) - F(%.4f)\n", s$d1_opt + 12, s$d1_opt))
    cat(sprintf("     = %.6f - %.6f\n",
                visa_cdf(s$d1_opt + 12, s$mu, s$sigma),
                visa_cdf(s$d1_opt, s$mu, s$sigma)))
    cat(sprintf("     = %.6f\n", sc$p_resched))
    cat(sprintf("     = %.2f%%\n\n", 100 * sc$p_resched))
    
    cat("─── Scenario 3: Window expired, new booking ───\n")
    cat("  Condition: T > d₁ + 12\n")
    cat(sprintf("  P₃ = 1 - F(d₁ + 12)\n"))
    cat(sprintf("     = 1 - F(%.4f)\n", s$d1_opt + 12))
    cat(sprintf("     = 1 - %.6f\n",
                visa_cdf(s$d1_opt + 12, s$mu, s$sigma)))
    cat(sprintf("     = %.6f\n", sc$p_new_book))
    cat(sprintf("     = %.2f%%\n\n", 100 * sc$p_new_book))
    
    cat("Verification: P₁ + P₂ + P₃ = 1\n")
    cat(sprintf("  %.6f + %.6f + %.6f = %.6f  ✓\n",
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
    
    cat("Costs are CUMULATIVE — money already spent is not refunded.\n\n")
    
    cat("─── Scenario 1: Visa in time ───\n")
    cat(sprintf("  Cost₁ = B = $%s\n\n",
                formatC(sc$cost_on_time, format = "f", digits = 0, big.mark = ",")))
    
    cat("─── Scenario 2: Reschedule ───\n")
    cat("  You already paid B. Now you pay R to reschedule.\n")
    cat(sprintf("  Cost₂ = B + R = $%s + $%s = $%s\n\n",
                formatC(b, format = "f", digits = 0, big.mark = ","),
                formatC(r, format = "f", digits = 0, big.mark = ","),
                formatC(sc$cost_resched, format = "f", digits = 0, big.mark = ",")))
    
    cat("─── Scenario 3: Window expired ───\n")
    cat("  You already paid B + R. The 12-month window expired,\n")
    cat("  so you need a completely new booking (B again).\n")
    cat(sprintf("  Cost₃ = B + R + B = $%s + $%s + $%s = $%s\n\n",
                formatC(b, format = "f", digits = 0, big.mark = ","),
                formatC(r, format = "f", digits = 0, big.mark = ","),
                formatC(b, format = "f", digits = 0, big.mark = ","),
                formatC(sc$cost_new_book, format = "f", digits = 0, big.mark = ",")))
    
    cat("─── Expected cost ───\n")
    expected <- sc$p_on_time * sc$cost_on_time +
      sc$p_resched * sc$cost_resched +
      sc$p_new_book * sc$cost_new_book
    cat("  E[Cost] = P₁×Cost₁ + P₂×Cost₂ + P₃×Cost₃\n")
    cat(sprintf("          = %.4f × $%s + %.4f × $%s + %.4f × $%s\n",
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
      tags$p("If T ~ LogNormal(μ, σ), then ln(T) ~ Normal(μ, σ²)"),
      tags$p("CDF:  F(t) = Φ( (ln(t) − μ) / σ )"),
      tags$p("PDF:  f(t) = (1 / (tσ√(2π))) exp(−(ln(t) − μ)² / (2σ²))"),
      tags$p("Quantile:  Q(p) = exp(μ + σ Φ⁻¹(p))"),
      
      tags$hr(),
      
      tags$p(tags$strong("Parameter estimation from quantiles")),
      tags$p("Given q₅₀ (median) and q₉₀ (90th percentile):"),
      tags$p("  μ = ln(q₅₀)"),
      tags$p("  σ = (ln(q₉₀) − μ) / Φ⁻¹(0.9)"),
      tags$p(sprintf("  Φ⁻¹(0.9) = %.6f", qnorm(0.9))),
      
      tags$hr(),
      
      tags$p(tags$strong("Scenario probabilities")),
      tags$p("P₁ = F(d₁)                    — visa before ceremony"),
      tags$p("P₂ = F(d₁ + 12) − F(d₁)      — visa within reschedule window"),
      tags$p("P₃ = 1 − F(d₁ + 12)           — visa after window expires"),
      tags$p("P₁ + P₂ + P₃ = 1"),
      
      tags$hr(),
      
      tags$p(tags$strong("Cumulative costs")),
      tags$p("Cost₁ = B"),
      tags$p("Cost₂ = B + R"),
      tags$p("Cost₃ = B + R + B"),
      tags$p("E[Cost] = P₁·Cost₁ + P₂·Cost₂ + P₃·Cost₃"),
      
      tags$hr(),
      
      tags$p(tags$strong("Impatience mapping")),
      tags$p("target_prob = 0.45 + (impatience / 10) × 0.50"),
      tags$p("Range: impatience 1 → 50%  ...  impatience 10 → 95%"),
      
      tags$hr(),
      
      tags$p(tags$strong("Bidirectional sync")),
      tags$p("Slider → Date:  d₁ = Q(target_prob), then convert months to calendar date"),
      tags$p("Date → Slider:  confidence = F(d₁), then impatience = round((confidence − 0.45) / 0.05)")
    )
  })
})
