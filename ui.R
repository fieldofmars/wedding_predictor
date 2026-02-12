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
          label   = "50% processed within (months)",
          value   = 12,
          min     = 1,
          max     = 60,
          step    = 1
        ),
        
        numericInput(
          inputId = "q90",
          label   = "90% processed within (months)",
          value   = 23,
          min     = 1,
          max     = 60,
          step    = 1
        ),
        
        numericInput(
          inputId = "d2_safe",
          label   = "Safe rescheduled ceremony time d2 (months after lodgement)",
          value   = 30,
          min     = 6,
          max     = 60,
          step    = 1
        ),
        
        sliderInput(
          inputId = "lambda",
          label   = "Impatience: $ per month of waiting (lambda)",
          min     = 0,
          max     = 200,
          value   = 50,
          step    = 10
        ),
        
        sliderInput(
          inputId = "search_range",
          label   = "Search range for first ceremony date d1 (months)",
          min     = 3,
          max     = 48,
          value   = c(6, 30),
          step    = 1
        )
      ),
      
      mainPanel(
        h3("Recommended ceremony plan"),
        verbatimTextOutput("summary_text"),
        
        h3("Cost and waiting trade-off"),
        plotOutput("tradeoff_plot")
      )
    )
  )
)
