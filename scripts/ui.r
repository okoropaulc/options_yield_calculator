library(shiny)

ui <- fluidPage(
  titlePanel("Options Yield Calculator (CSP and Covered Calls)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "strategy", "Strategy",
        choices = c("Cash-Secured Put (CSP)" = "CSP",
                    "Covered Call (CC)" = "CC")
      ),
      
      numericInput("contracts", "Contracts", 1, min = 1),
      numericInput("multiplier", "Contract multiplier", 100),
      numericInput("dte", "Days to Expiration (DTE)", 30),
      
      numericInput("strike", "Strike price ($)", 50),
      numericInput("premium", "Premium per share ($)", 1.20),
      numericInput("fees", "Total fees ($)", 0),
      
      selectInput(
        "report_period", "Report yield as",
        c("Per-cycle (DTE)", "Weekly", "Bi-weekly",
          "Monthly", "Quarterly", "Annualized", "Custom (X days)")
      ),
      
      conditionalPanel(
        condition = "input.report_period == 'Custom (X days)'",
        numericInput("custom_days", "Custom days", 10)
      ),
      
      conditionalPanel(
        condition = "input.report_period == 'Annualized'",
        selectInput("day_basis", "Day basis", c(365, 252)),
        selectInput("annual_method", "Method", c("Simple", "Compounding"))
      ),
      
      conditionalPanel(
        condition = "input.strategy == 'CC'",
        numericInput("stock_basis", "Stock basis ($)", 50),
        numericInput("stock_current", "Current price ($)", 50)
      )
    ),
    
    mainPanel(
      verbatimTextOutput("summary_text"),
      tableOutput("metrics_table")
    )
  )
)
