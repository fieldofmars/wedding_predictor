## ui.R

library(shiny)

shinyUI(
  fluidPage(
    titlePanel("Visa Processing & Wedding Scheduling Planner"),
    
    sidebarLayout(
      sidebarPanel(
        dateInput(
          inputId = "lodgement_date",
          label   = "Lodgement date",
          value   = Sys.Date()
        ),
        
        numericInput(
          inputId = "q50",
          label   = "50% of visas processed within (months)",
          value   = 12,
          min     = 1,
          max     = 60,
          step    = 1
        ),
        
        numericInput(
          inputId = "q90",
          label   = "90% of visas processed within (months)",
          value   = 23,
          min     = 1,
          max     = 60,
          step    = 1
        ),
        
        hr(),
        h4("Costs"),
        
        numericInput(
          inputId = "booking_cost",
          label   = "Wedding booking cost ($)",
          value   = 400,
          min     = 0,
          max     = 10000,
          step    = 50
        ),
        
        numericInput(
          inputId = "resched_cost",
          label   = "Reschedule fee ($) — full flat fee",
          value   = 200,
          min     = 0,
          max     = 10000,
          step    = 50
        ),
        
        helpText("If the visa isn't granted by the ceremony date, you can",
                 "reschedule within 12 months for the flat reschedule fee.",
                 "If the visa still isn't granted after 12 months, the",
                 "original booking is lost and you need a new booking."),
        
        hr(),
        h4("How soon do you want to get married?"),
        
        sliderInput(
          inputId = "impatience",
          label   = "Impatience (1 = ASAP, risky | 10 = happy to wait, safe)",
          min     = 1,
          max     = 10,
          value   = 5,
          step    = 1
        ),
        
        helpText("This controls how much risk you're willing to accept.",
                 "Lower = book earlier (higher chance of needing to reschedule).",
                 "Higher = book later (more likely the visa is granted in time)."),
        
        hr(),
        
        sliderInput(
          inputId = "search_range",
          label   = "Display range for ceremony dates (months)",
          min     = 3,
          max     = 48,
          value   = c(6, 30),
          step    = 1
        )
      ),
      
      mainPanel(
        h3("Recommended ceremony plan"),
        verbatimTextOutput("summary_text"),
        
        h3("Scenario probabilities by ceremony date"),
        plotOutput("tradeoff_plot", height = "400px"),
        
        h3("Visa processing time distribution"),
        plotOutput("dist_plot", height = "350px")
      )
    )
  )
)
