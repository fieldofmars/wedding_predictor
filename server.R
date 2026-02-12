## server.R

library(shiny)
library(tidyverse)

shinyServer(function(input, output, session) {
  
  # Reactive: fit log-normal params from quantiles
  dist_params <- reactive({
    fit_lognormal_from_quantiles(q50 = input$q50,
                                 q90 = input$q90)
  })
  
  # Reactive: compute optimal d1 and all summary metrics
  summary_data <- reactive({
    params <- dist_params()
    mu     <- params$mu
    sigma  <- params$sigma
    
    # Bound search range to positive values
    lower <- max(0.1, input$search_range[1])
    upper <- max(lower + 0.5, input$search_range[2])
    
    opt <- find_optimal_d1(
      mu     = mu,
      sigma  = sigma,
      lambda = input$lambda,
      d2     = input$d2_safe,
      lower  = lower,
      upper  = upper
    )
    
    d1_opt <- opt$d1_opt
    
    p_resched <- 1 - visa_cdf(d1_opt, mu, sigma)
    ecost     <- expected_cost(d1_opt, mu = mu, sigma = sigma)
    ewait     <- expected_wait(d1_opt, d2 = input$d2_safe, mu = mu, sigma = sigma)
    
    # Approximate ceremony date using month steps
    lodgement <- input$lodgement_date
    months_to_add <- round(d1_opt)
    ceremony_date <- seq(
      from = lodgement,
      by   = "1 month",
      length.out = months_to_add + 1
    )[months_to_add + 1]
    
    list(
      mu           = mu,
      sigma        = sigma,
      d1_opt       = d1_opt,
      p_resched    = p_resched,
      expected_cost = ecost,
      expected_wait = ewait,
      ceremony_date = ceremony_date
    )
  })
  
  # Text summary
  output$summary_text <- renderPrint({
    s <- summary_data()
    
    cat("Optimal first ceremony time (d1):",
        round(s$d1_opt, 2), "months after lodgement\n")
    
    cat("Recommended ceremony date:",
        format(s$ceremony_date, "%Y-%m-%d"), "\n\n")
    
    cat("Probability you will need to reschedule:",
        sprintf("%.1f%%", 100 * s$p_resched), "\n")
    
    cat("Expected total cost: $",
        round(s$expected_cost, 2), "\n", sep = "")
    
    cat("Expected waiting time between grant and ceremony:",
        round(s$expected_wait, 2), "months\n")
  })
  
  # Plot cost vs d1, waiting vs d1, and combined loss
  output$tradeoff_plot <- renderPlot({
    params <- dist_params()
    mu     <- params$mu
    sigma  <- params$sigma
    
    d1_seq <- seq(input$search_range[1],
                  input$search_range[2],
                  by = 0.5)
    
    df <- metrics_grid(
      mu     = mu,
      sigma  = sigma,
      lambda = input$lambda,
      d2     = input$d2_safe,
      d1_seq = d1_seq
    )
    
    s <- summary_data()
    
    ggplot(df, aes(x = d1)) +
      geom_line(aes(y = expected_cost, colour = "Expected cost ($)")) +
      geom_line(aes(y = expected_wait * 50, colour = "Expected wait (months) x 50"),
                linetype = "dashed") +
      geom_line(aes(y = loss, colour = "Loss = cost + lambda * wait"),
                linetype = "dotdash") +
      geom_vline(xintercept = s$d1_opt, linetype = "dotted") +
      labs(
        x = "First ceremony time d1 (months after lodgement)",
        y = "Scale (dollars; waiting rescaled by 50 for visibility)",
        colour = "Metric",
        title = "Trade-off between ceremony timing, cost, and waiting"
      ) +
      theme_minimal()
  })
})
