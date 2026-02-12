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
        h4("When do you want to get married?"),
        
        helpText("Use the slider OR the date picker — they stay in sync.",
                 "The slider controls your risk tolerance; the date picker",
                 "lets you explore a specific date."),
        
        sliderInput(
          inputId = "impatience",
          label   = "Impatience (1 = ASAP, risky | 10 = happy to wait, safe)",
          min     = 1,
          max     = 10,
          value   = 5,
          step    = 1
        ),
        
        dateInput(
          inputId = "ceremony_date_pick",
          label   = "Or pick a ceremony date",
          value   = Sys.Date() + 365
        ),
        
        helpText(
          tags$em("Changing the slider updates the date. Changing the date updates the slider.")
        ),
        
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
        tabsetPanel(
          id   = "main_tabs",
          type = "tabs",
          
          ## ── Results tab ──────────────────────────────────
          tabPanel(
            "Results",
            h3("Your selected ceremony date"),
            uiOutput("risk_assessment"),
            
            hr(),
            
            h3("Scenario probabilities by ceremony date"),
            plotOutput("tradeoff_plot", height = "400px"),
            
            h3("Visa processing time distribution"),
            plotOutput("dist_plot", height = "350px")
          ),
          
          ## ── Working tab ──────────────────────────────────
          tabPanel(
            "Show Working",
            
            h3("Step 1 — Fitting the distribution"),
            helpText("We model visa processing time as a",
                     tags$strong("log-normal"), "random variable.",
                     "Two published quantiles pin down the two parameters."),
            verbatimTextOutput("working_fit"),
            
            h3("Step 2 — Impatience → target probability"),
            helpText("Your impatience slider is converted to a",
                     "target confidence level: the probability that",
                     "the visa is granted before the ceremony date."),
            verbatimTextOutput("working_impatience"),
            
            h3("Step 3 — Optimal ceremony date"),
            helpText("We invert the CDF (i.e. take the quantile) at",
                     "the target probability to find the recommended",
                     "ceremony date in months after lodgement."),
            verbatimTextOutput("working_optimal"),
            
            h3("Step 4 — Scenario probabilities"),
            helpText("Given the ceremony date d₁, three mutually",
                     "exclusive and exhaustive scenarios arise."),
            verbatimTextOutput("working_scenarios"),
            
            h3("Step 5 — Cost breakdown"),
            helpText("Each scenario has a fixed, cumulative cost.",
                     "You never get back money already spent."),
            verbatimTextOutput("working_costs"),
            
            hr(),
            h4("Reference: key formulas"),
            uiOutput("working_formulas")
          )
        )
      )
    )
  )
)
