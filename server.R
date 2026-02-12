## server.R

library(shiny)
library(tidyverse)

shinyServer(function(input, output, session) {
  
  # Reactive: fit log-normal params from quantiles
  dist_params <- reactive({
    fit_lognormal_from_quantiles(q50 = input$q50,
                                 q90 = input$q90)
  })
  
  # Reactive: compute optimal d1 and scenario analysis
  summary_data <- reactive({
    params <- dist_params()
    mu     <- params$mu
    sigma  <- params$sigma
    
    booking_cost <- input$booking_cost
    resched_cost <- input$resched_cost
    
    # Convert impatience slider (1-10) to a target probability
    # 1 = very impatient (willing to accept ~50% chance) 
    # 10 = very patient (wants ~95% chance)
    target_prob <- 0.45 + (input$impatience / 10) * 0.50
    # This maps: 1 -> 0.50, 5 -> 0.70, 10 -> 0.95
    
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
    
    # Approximate ceremony date
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
  
  # Text summary
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
  
  # Plot: scenario probabilities vs ceremony date
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
    
    # Compute cumulative cost for scenario 3 for the legend
    cost_new_total <- booking_cost + resched_cost + booking_cost
    
    # Reshape for plotting
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
  
  # Plot: visa processing time distribution
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
})
