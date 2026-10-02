## ui.R

shinyUI(
  fluidPage(
    theme = wedding_theme,
    useShinyjs(),
    tags$head(
      tags$title("The Wedding Predictor | Visa & Ceremony Scheduling Planner"),
      tags$meta(name = "description", content = "Stochastic visa wait time modelling, risk optimization, and wedding ceremony scheduling planner."),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      local({
        candidates <- c("www/custom.css", "../../www/custom.css", "../www/custom.css")
        hit <- candidates[file.exists(candidates)][1]
        if (!is.na(hit)) includeCSS(hit) else NULL
      })
    ),
    
    # ── Login Screen ──────────────────────────────────────────
    # Sign-in, sign-up and password-reset panels. All three are built by the
    # `login` package on the server (they share one enclosing_panel -> our
    # login_card), and server.R hides this whole block once the user is in.
    div(
      id = "login_screen",
      
      div(
        class = "login-hero",
        div(
          class = "login-hero-badge",
          fontawesome::fa("gem", fill = "#D4AF37", height = "13px"),
          tags$span("Probabilistic Ceremony Planning")
        ),
        tags$h1(class = "brand-title", "The Wedding Predictor"),
        tags$p(
          "Plan your special day with statistical confidence. Model visa wait times, optimize ceremony dates, and quantify financial risk with log-normal & Monte Carlo engines."
        )
      ),
      
      fluidRow(
        column(6, login_ui(id = "login")),
        # Sign-up is offered only when it can be verified by email: with no
        # emailer login_server() would accept accounts immediately, so a public
        # form would let anyone create one. signup_enabled() lives in global.R.
        column(6, if (signup_enabled(app_emailer)) {
          new_user_ui(id = "login")
        } else {
          div(
            class = "bslib-card wedding-card p-4 text-center",
            style = "border: 2px dashed rgba(212, 175, 55, 0.4); background: rgba(255, 255, 255, 0.7);",
            div(
              style = "margin-bottom: 12px;",
              fontawesome::fa("envelope-circle-check", fill = "#D4AF37", height = "36px")
            ),
            tags$h5(tags$strong("Sign-up is disabled.")),
            tags$p(
              class = "text-muted small mb-0",
              "Creating an account needs an emailed verification code, and no ",
              "email server is configured - set GMAIL_USER and GMAIL_PASS, then ",
              "restart the app."
            )
          )
        })
      ),
      fluidRow(
        class = "mt-3",
        column(12, reset_password_ui(id = "login"))
      )
    ),
    
    # ── Main Application ──────────────────────────────────────
    div(
      id = "main_app",
      style = "display: none;",
      
      # Top Navigation Bar with branding & session meta
      div(
        class = "wedding-navbar",
        div(
          class = "wedding-brand",
          div(
            class = "wedding-logo-icon",
            fontawesome::fa("ring", fill = "#D4AF37", height = "22px")
          ),
          div(
            class = "wedding-brand-text",
            tags$h1("The Wedding Predictor"),
            tags$p(class = "brand-tagline", "Partner Visa & Ceremony Timing Optimizer")
          )
        ),
        div(
          class = "wedding-nav-meta",
          uiOutput("nav_status_badge"),
          div(class = "wedding-logout-wrap", logout_button(id = "login", label = "Sign Out"))
        )
      ),
      
      div(
        class = "container-fluid px-4 pb-5",
        sidebarLayout(
          sidebarPanel(
            class = "wedding-sidebar",
            width = 4,
            
            # Section 1: Visa processing parameters
            div(
              class = "sidebar-section-title",
              fontawesome::fa("passport", fill = "#8B263E", height = "16px"),
              tags$span("Visa Processing Model")
            ),
            
            dateInput(
              inputId = "lodgement_date",
              label   = "Visa lodgement date",
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
            
            # Section 2: Financial parameters
            div(
              class = "sidebar-section-title",
              fontawesome::fa("coins", fill = "#8B263E", height = "16px"),
              tags$span("Wedding Financials")
            ),
            
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
            
            helpText(
              fontawesome::fa("circle-info", fill = "#94A3B8", height = "13px"),
              "If the visa isn't granted by the ceremony date, you can ",
              "reschedule within 12 months for the flat reschedule fee. ",
              "After 12 months, the original booking is forfeited and a new booking is required."
            ),
            
            # Section 3: Ceremony timing
            div(
              class = "sidebar-section-title",
              fontawesome::fa("heart", fill = "#8B263E", height = "16px"),
              tags$span("Ceremony Timing & Risk Appetite")
            ),
            
            helpText(
              "Use the slider OR date picker — they stay continuously in sync. ",
              "The slider reflects your appetite for risk; the date picker lets you inspect a specific date."
            ),
            
            sliderInput(
              inputId = "impatience",
              label   = "Risk tolerance (1 = ASAP, risky | 10 = patient, safe)",
              min     = 1,
              max     = 10,
              value   = 5,
              step    = 1
            ),
            
            dateInput(
              inputId = "ceremony_date_pick",
              label   = "Target ceremony date",
              value   = Sys.Date() + 365
            ),
            
            helpText(
              tags$em("Changing the slider calculates the date. Changing the date updates the slider.")
            ),
            
            # Section 4: Chart display controls
            div(
              class = "sidebar-section-title",
              fontawesome::fa("sliders", fill = "#8B263E", height = "16px"),
              tags$span("Chart Display Range")
            ),
            
            sliderInput(
              inputId = "search_range",
              label   = "Display range for ceremony dates (months)",
              min     = 3,
              max     = 48,
              value   = c(6, 30),
              step    = 1
            ),
            
            hr(style = "margin: 20px 0; border-color: rgba(212, 175, 55, 0.25);"),
            div(
              class = "d-grid wedding-logout-wrap",
              logout_button(id = "login", label = "Sign Out")
            )
          ),
          
          mainPanel(
            width = 8,
            tabsetPanel(
              id   = "main_tabs",
              type = "tabs",
              
              ## ── Results tab ──────────────────────────────────
              tabPanel(
                title = tagList(fontawesome::fa("chart-pie", fill = "#8B263E", height = "14px"), "Results"),
                
                # Dynamic KPI Cards / Summary Stat Tiles
                uiOutput("results_kpi_cards"),
                
                # Risk assessment banner & detailed scenario table
                uiOutput("risk_assessment"),
                
                # Tradeoff Plot Card
                div(
                  class = "plot-card",
                  div(
                    class = "plot-card-header",
                    tags$h3(class = "plot-card-title", "Scenario Probabilities & Cost Tradeoffs"),
                    tags$p(class = "plot-card-desc", "Evaluate how advancing your wedding date increases risk vs delaying increases certainty.")
                  ),
                  plotOutput("tradeoff_plot", height = "420px")
                ),
                
                # Distribution Plot Card
                div(
                  class = "plot-card",
                  div(
                    class = "plot-card-header",
                    tags$h3(class = "plot-card-title", "Visa Processing Time Distribution (Fitted Log-Normal)"),
                    tags$p(class = "plot-card-desc", "Cumulative probability of visa grant by month after lodgement, marking your ceremony and 12-month rebooking cutoff.")
                  ),
                  plotOutput("dist_plot", height = "360px")
                )
              ),
              
              ## ── Monte Carlo tab ──────────────────────────────
              tabPanel(
                title = tagList(fontawesome::fa("dice", fill = "#8B263E", height = "14px"), "Monte Carlo"),
                
                div(
                  class = "mc-hero-box",
                  div(
                    style = "display: flex; align-items: flex-start; gap: 14px;",
                    div(
                      style = "color: #D4AF37; font-size: 24px; margin-top: 2px;",
                      fontawesome::fa("brain", fill = "#D4AF37", height = "28px")
                    ),
                    div(
                      tags$h4(style = "margin: 0 0 6px 0; color: #8B263E; font-family: 'Playfair Display', serif;", "Why Monte Carlo Simulation?"),
                      tags$p(
                        style = "margin: 0; font-size: 14px; color: #4B5563; line-height: 1.55;",
                        "The Results tab uses exact closed-form equations from the log-normal distribution. ",
                        "This engine simulates thousands of individual synthetic visa applicant lifecycles to observe outcomes empirically. ",
                        "Watch the empirical estimates converge tightly to the theoretical values as simulations increase — an interactive demonstration of the ",
                        tags$strong("Law of Large Numbers.")
                      )
                    )
                  )
                ),
                
                uiOutput("mc_input_problem"),
                
                div(
                  class = "bslib-card wedding-card p-4 mb-4",
                  fluidRow(
                    column(4,
                      numericInput(
                        inputId = "mc_n_sims",
                        label   = "Number of simulations",
                        value   = 10000,
                        min     = 100,
                        max     = 100000,
                        step    = 1000
                      )
                    ),
                    column(4,
                      numericInput(
                        inputId = "mc_seed",
                        label   = "Random seed (optional, 0 = random)",
                        value   = 0,
                        min     = 0,
                        max     = 999999,
                        step    = 1
                      )
                    ),
                    column(4,
                      tags$div(
                        style = "margin-top: 28px;",
                        actionButton(
                          inputId = "mc_run",
                          label   = " Run Simulation",
                          icon    = icon("play"),
                          class   = "btn btn-mc-run w-100"
                        )
                      )
                    )
                  )
                ),
                
                conditionalPanel(
                  condition = "output.mc_has_results",
                  
                  # Monte Carlo KPI Stat Cards
                  uiOutput("mc_kpi_cards"),
                  
                  # Convergence Comparison Table Card
                  div(
                    class = "plot-card",
                    div(
                      class = "plot-card-header",
                      tags$h3(class = "plot-card-title", "Convergence: Monte Carlo vs Closed-Form Theory"),
                      tags$p(class = "plot-card-desc", "Direct comparison between theoretical log-normal parameters and simulated empirical results.")
                    ),
                    uiOutput("mc_comparison_table")
                  ),
                  
                  # MC Histogram Card
                  div(
                    class = "plot-card",
                    div(
                      class = "plot-card-header",
                      tags$h3(class = "plot-card-title", "Simulated Visa Grant Times"),
                      tags$p(class = "plot-card-desc", "Distribution of simulated grant dates partitioned by outcome scenario, overlaid with the theoretical PDF.")
                    ),
                    plotOutput("mc_histogram", height = "420px")
                  ),
                  
                  # MC Cost Distribution Card
                  div(
                    class = "plot-card",
                    div(
                      class = "plot-card-header",
                      tags$h3(class = "plot-card-title", "Simulated Financial Outcomes"),
                      tags$p(class = "plot-card-desc", "Observed proportion and frequency of each total cost category across all simulation runs.")
                    ),
                    plotOutput("mc_cost_plot", height = "360px")
                  ),
                  
                  # MC Convergence Plot Card
                  div(
                    class = "plot-card",
                    div(
                      class = "plot-card-header",
                      tags$h3(class = "plot-card-title", "Expected Cost Convergence Trajectory"),
                      tags$p(class = "plot-card-desc", "Running average expected cost stabilising against the theoretical mean as sample size grows.")
                    ),
                    plotOutput("mc_convergence_plot", height = "320px")
                  ),
                  
                  # Raw Simulation Summary
                  div(
                    class = "working-card",
                    div(
                      class = "working-card-header",
                      fontawesome::fa("terminal", fill = "#8B263E", height = "16px"),
                      tags$h4(style = "margin: 0; font-family: 'Playfair Display', serif; color: #8B263E;", "Raw Simulation Diagnostics")
                    ),
                    div(
                      class = "working-card-body",
                      verbatimTextOutput("mc_raw_summary")
                    )
                  )
                )
              ),
              
              ## ── Working tab ──────────────────────────────────
              tabPanel(
                title = tagList(fontawesome::fa("calculator", fill = "#8B263E", height = "14px"), "Show Working"),
                
                div(
                  class = "mc-hero-box mb-4",
                  tags$h4(style = "margin: 0 0 6px 0; color: #8B263E; font-family: 'Playfair Display', serif;", "Mathematical Foundations & Analytical Derivation"),
                  tags$p(
                    style = "margin: 0; font-size: 14px; color: #4B5563;",
                    "Transparent, step-by-step mathematical proof demonstrating parameter fitting, quantile inversion, scenario probabilities, and cost expectations."
                  )
                ),
                
                # Step 1 Card
                div(
                  class = "working-card",
                  div(
                    class = "working-card-header",
                    span(class = "step-number-badge", "1"),
                    tags$h3("Step 1 — Fitting the Log-Normal Distribution")
                  ),
                  div(
                    class = "working-card-body",
                    tags$p(class = "text-muted mb-3",
                      "We model visa processing time as a ", tags$strong("log-normal"), " random variable. Two published quantiles pin down the parameters mu and sigma."
                    ),
                    verbatimTextOutput("working_fit")
                  )
                ),
                
                # Step 2 Card
                div(
                  class = "working-card",
                  div(
                    class = "working-card-header",
                    span(class = "step-number-badge", "2"),
                    tags$h3("Step 2 — Impatience Mapping to Target Confidence")
                  ),
                  div(
                    class = "working-card-body",
                    tags$p(class = "text-muted mb-3",
                      "Your impatience slider is converted to a target confidence level: the probability that the visa arrives before the ceremony date."
                    ),
                    verbatimTextOutput("working_impatience")
                  )
                ),
                
                # Step 3 Card
                div(
                  class = "working-card",
                  div(
                    class = "working-card-header",
                    span(class = "step-number-badge", "3"),
                    tags$h3("Step 3 — Quantile Inversion for Recommended Date")
                  ),
                  div(
                    class = "working-card-body",
                    tags$p(class = "text-muted mb-3",
                      "We invert the cumulative distribution function (take the quantile) at the target probability to find the recommended ceremony date."
                    ),
                    verbatimTextOutput("working_optimal")
                  )
                ),
                
                # Step 4 Card
                div(
                  class = "working-card",
                  div(
                    class = "working-card-header",
                    span(class = "step-number-badge", "4"),
                    tags$h3("Step 4 — Scenario Probability Decomposition")
                  ),
                  div(
                    class = "working-card-body",
                    tags$p(class = "text-muted mb-3",
                      "Given the ceremony date d1, three mutually exclusive and exhaustive scenarios arise across the timeline."
                    ),
                    verbatimTextOutput("working_scenarios")
                  )
                ),
                
                # Step 5 Card
                div(
                  class = "working-card",
                  div(
                    class = "working-card-header",
                    span(class = "step-number-badge", "5"),
                    tags$h3("Step 5 — Expected Financial Cost Breakdown")
                  ),
                  div(
                    class = "working-card-body",
                    tags$p(class = "text-muted mb-3",
                      "Each scenario has a fixed cumulative financial impact. Costs already incurred are non-refundable."
                    ),
                    verbatimTextOutput("working_costs")
                  )
                ),
                
                # Reference Formulas Card
                div(
                  class = "working-card",
                  div(
                    class = "working-card-header",
                    fontawesome::fa("book", fill = "#8B263E", height = "16px"),
                    tags$h3("Reference: Key Analytical Formulas")
                  ),
                  div(
                    class = "working-card-body",
                    uiOutput("working_formulas")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
