## server.R

library(shiny)
library(tidyverse)

shinyServer(function(input, output, session) {
  
  # ── Reactives ───────────────────────────────────────────
  
  dist_params <- reactive({
    fit_lognormal_from_quantiles(q50 = input$q50,
                                 q90 = input$q90)
  })
  
  summary_data <- reactive({
    params <- dist_params()
    mu     <- params$mu
    sigma  <- params$sigma
    
    booking_cost <- input$booking_cost
    resched_cost <- input$resched_cost
    
    target_prob <- 0.45 + (input$impatience / 10) * 0.50
    
    d1_opt <- find_optimal_d1(
      mu          = mu,
      sigma       = sigma,
      target_prob = target_prob
    )
    
    scenarios <- scenario_analysis(
      d1           = d1_opt,
      mu           = mu,
      sigma        = sigma,
      booking_cost = booking_cost,
      resched_cost = resched_cost
    )
    
    lodgement <- input$lodgement_date
    months_to_add <- round(d1_opt)
    ceremony_date <- seq(
      from       = lodgement,
      by         = "1 month",
      length.out = months_to_add + 1
    )[months_to_add + 1]
    
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
  
  # ── Results tab outputs ─────────────────────────────────
  
  output$summary_text <- renderPrint({
    s  <- summary_data()
    sc <- s$scenarios
    
    cat("Target confidence level:",
        sprintf("%.0f%%", 100 * s$target_prob),
        "(based on impatience setting)\n\n")
    
    cat("Recommended first ceremony:",
        round(s$d1_opt, 1), "months after lodgement\n")
    cat("Recommended ceremony date:",
        format(s$ceremony_date, "%Y-%m-%d"), "\n\n")
    
    cat("=== What could happen ===\n\n")
    
    cat(sprintf("1. Visa granted in time (%s chance)\n",
                sprintf("%.1f%%", 100 * sc$p_on_time)))
    cat(sprintf("   You pay: $%s (booking only)\n\n",
                formatC(sc$cost_on_time, format = "f", digits = 0, big.mark = ",")))
    
    cat(sprintf("2. Need to reschedule within 12 months (%s chance)\n",
                sprintf("%.1f%%", 100 * sc$p_resched)))
    cat(sprintf("   You pay: $%s (booking) + $%s (reschedule fee) = $%s\n\n",
                formatC(s$booking_cost, format = "f", digits = 0, big.mark = ","),
                formatC(s$resched_cost, format = "f", digits = 0, big.mark = ","),
                formatC(sc$cost_resched, format = "f", digits = 0, big.mark = ",")))
    
    cat(sprintf("3. 12-month window expires, new booking needed (%s chance)\n",
                sprintf("%.1f%%", 100 * sc$p_new_book)))
    cat(sprintf("   You pay: $%s (booking) + $%s (reschedule fee) + $%s (new booking) = $%s\n",
                formatC(s$booking_cost, format = "f", digits = 0, big.mark = ","),
                formatC(s$resched_cost, format = "f", digits = 0, big.mark = ","),
                formatC(s$booking_cost, format = "f", digits = 0, big.mark = ","),
                formatC(sc$cost_new_book, format = "f", digits = 0, big.mark = ",")))
  })
  
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
               label = paste0("Recommended: ", round(s$d1_opt, 1), " months"),
               hjust = -0.05, size = 4, fontface = "bold") +
      scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.05)) +
      scale_fill_manual(values = c("#2ecc71", "#f39c12", "#e74c3c")) +
      labs(
        x     = "First ceremony date (months after lodgement)",
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
        subtitle = "Red = ceremony date | Orange = 12-month rebooking window end"
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
    
    cat("Impatience slider value:", input$impatience, "/ 10\n\n")
    
    cat("Mapping formula:\n")
    cat("  target_prob = 0.45 + (impatience / 10) × 0.50\n\n")
    
    cat("Substituting:\n")
    cat(sprintf("  target_prob = 0.45 + (%d / 10) × 0.50\n", input$impatience))
    cat(sprintf("              = 0.45 + %.2f × 0.50\n", input$impatience / 10))
    cat(sprintf("              = 0.45 + %.2f\n", (input$impatience / 10) * 0.50))
    cat(sprintf("              = %.2f\n\n", s$target_prob))
    
    cat(sprintf("Interpretation: you want a %.0f%% chance that the visa\n",
                100 * s$target_prob))
    cat("is granted BEFORE the ceremony date.\n\n")
    
    cat("Full mapping table:\n")
    cat("  Impatience  →  Target probability\n")
    cat("  ──────────     ──────────────────\n")
    for (i in 1:10) {
      p <- 0.45 + (i / 10) * 0.50
      marker <- if (i == input$impatience) "  ◀ you" else ""
      cat(sprintf("  %2d            →  %.0f%%%s\n", i, 100 * p, marker))
    }
  })
  
  ## Step 3: Optimal ceremony date
  output$working_optimal <- renderPrint({
    s <- summary_data()
    
    cat("We need d₁ such that P(T ≤ d₁) = target_prob\n\n")
    
    cat("This is the quantile function of the log-normal:\n")
    cat("  d₁ = exp(μ + σ × Φ⁻¹(target_prob))\n\n")
    
    cat("Substituting:\n")
    cat(sprintf("  d₁ = exp(%.6f + %.6f × Φ⁻¹(%.4f))\n",
                s$mu, s$sigma, s$target_prob))
    cat(sprintf("     = exp(%.6f + %.6f × %.6f)\n",
                s$mu, s$sigma, qnorm(s$target_prob)))
    cat(sprintf("     = exp(%.6f + %.6f)\n",
                s$mu, s$sigma * qnorm(s$target_prob)))
    cat(sprintf("     = exp(%.6f)\n",
                s$mu + s$sigma * qnorm(s$target_prob)))
    cat(sprintf("     = %.4f months\n\n", s$d1_opt))
    
    cat(sprintf("Rounded: ~%.1f months after lodgement\n", s$d1_opt))
    cat(sprintf("Calendar date: %s\n\n", format(s$ceremony_date, "%Y-%m-%d")))
    
    cat("Verification:\n")
    cat(sprintf("  P(T ≤ %.4f) = %.6f  ✓ (should be %.4f)\n",
                s$d1_opt,
                visa_cdf(s$d1_opt, s$mu, s$sigma),
                s$target_prob))
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
      tags$p("Range: impatience 1 → 50%  ...  impatience 10 → 95%")
    )
  })
})
