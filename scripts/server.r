
source("helpers.r")

server <- function(input, output, session) {
  
  calc <- reactive({
    
    strategy      <- input$strategy
    contracts     <- num_or_zero(input$contracts)
    multiplier    <- num_or_zero(input$multiplier)
    dte           <- num_or_zero(input$dte)
    
    strike        <- num_or_zero(input$strike)
    prem_ps       <- num_or_zero(input$premium)
    fees          <- num_or_zero(input$fees)
    
    report_period <- input$report_period
    custom_days   <- num_or_zero(input$custom_days)
    
    day_basis     <- as.numeric(input$day_basis %||% 365)
    annual_method <- input$annual_method %||% "Simple"
    
    # Core amounts
    gross_premium <- prem_ps * contracts * multiplier
    premium_total <- gross_premium - fees
    premium_total <- max(premium_total, 0) # avoid negative if fees > premium
    
    out <- list(
      strategy = strategy,
      contracts = contracts,
      multiplier = multiplier,
      dte = dte,
      strike = strike,
      prem_ps = prem_ps,
      fees = fees,
      gross_premium = gross_premium,
      premium_total = premium_total,
      report_period = report_period,
      custom_days = custom_days,
      day_basis = day_basis,
      annual_method = annual_method
    )
    
    if (strategy == "CSP") {
      
      cash_secured <- strike * contracts * multiplier
      cycle_ret    <- safe_div(premium_total, cash_secured)  # decimal return over DTE
      
      reported_ret <- scale_return(
        cycle_ret_decimal = cycle_ret,
        dte = dte,
        period_label = report_period,
        day_basis = day_basis,
        annual_method = annual_method,
        custom_days = if (report_period == "Custom (X days)") custom_days else NA_real_
      )
      
      breakeven <- strike - prem_ps
      
      out$capital_base_name <- "Cash secured"
      out$capital_base <- cash_secured
      out$cycle_ret <- cycle_ret
      out$reported_ret <- reported_ret
      out$breakeven <- breakeven
      out$max_profit <- premium_total
      out$max_profit_note <- "Max profit (if put expires worthless) equals net premium."
      
    } else {
      
      stock_basis   <- num_or_zero(input$stock_basis)
      stock_current <- num_or_zero(input$stock_current)
      
      basis_capital   <- stock_basis * contracts * multiplier
      current_capital <- stock_current * contracts * multiplier
      
      cycle_ret_basis   <- safe_div(premium_total, basis_capital)
      cycle_ret_current <- safe_div(premium_total, current_capital)
      
      reported_ret_basis <- scale_return(
        cycle_ret_decimal = cycle_ret_basis,
        dte = dte,
        period_label = report_period,
        day_basis = day_basis,
        annual_method = annual_method,
        custom_days = if (report_period == "Custom (X days)") custom_days else NA_real_
      )
      
      reported_ret_current <- scale_return(
        cycle_ret_decimal = cycle_ret_current,
        dte = dte,
        period_label = report_period,
        day_basis = day_basis,
        annual_method = annual_method,
        custom_days = if (report_period == "Custom (X days)") custom_days else NA_real_
      )
      
      # Called away scenario
      upside_per_share <- max(0, strike - stock_basis)
      max_profit <- premium_total + (upside_per_share * contracts * multiplier)
      
      cycle_max_ret_basis   <- safe_div(max_profit, basis_capital)
      cycle_max_ret_current <- safe_div(max_profit, current_capital)
      
      reported_max_ret_basis <- scale_return(
        cycle_ret_decimal = cycle_max_ret_basis,
        dte = dte,
        period_label = report_period,
        day_basis = day_basis,
        annual_method = annual_method,
        custom_days = if (report_period == "Custom (X days)") custom_days else NA_real_
      )
      
      reported_max_ret_current <- scale_return(
        cycle_ret_decimal = cycle_max_ret_current,
        dte = dte,
        period_label = report_period,
        day_basis = day_basis,
        annual_method = annual_method,
        custom_days = if (report_period == "Custom (X days)") custom_days else NA_real_
      )
      
      cc_effective_sale <- strike + prem_ps
      
      out$stock_basis <- stock_basis
      out$stock_current <- stock_current
      
      out$basis_capital <- basis_capital
      out$current_capital <- current_capital
      
      out$cycle_ret_basis <- cycle_ret_basis
      out$cycle_ret_current <- cycle_ret_current
      
      out$reported_ret_basis <- reported_ret_basis
      out$reported_ret_current <- reported_ret_current
      
      out$upside_per_share <- upside_per_share
      out$max_profit <- max_profit
      
      out$cycle_max_ret_basis <- cycle_max_ret_basis
      out$cycle_max_ret_current <- cycle_max_ret_current
      
      out$reported_max_ret_basis <- reported_max_ret_basis
      out$reported_max_ret_current <- reported_max_ret_current
      
      out$cc_effective_sale <- cc_effective_sale
    }
    
    out
  })
  
  output$summary_text <- renderText({
    x <- calc()
    
    period_line <- if (x$report_period == "Annualized") {
      paste0("Reporting period: Annualized (", x$day_basis, " day basis, ", x$annual_method, ")")
    } else if (x$report_period == "Custom (X days)") {
      paste0("Reporting period: Custom (", x$custom_days, " days)")
    } else {
      paste0("Reporting period: ", x$report_period)
    }
    
    common <- paste0(
      period_line, "\n\n",
      "Premium received (gross): ", money(x$gross_premium), "\n",
      "Fees: ", money(x$fees), "\n",
      "Premium received (net): ", money(x$premium_total), "\n"
    )
    
    if (x$strategy == "CSP") {
      paste0(
        "CSP summary\n",
        common,
        "Cash secured: ", money(x$capital_base), "\n",
        "Yield (reported): ", pcts(pct(x$reported_ret)), "\n",
        "Yield (per-cycle / DTE): ", pcts(pct(x$cycle_ret)), "\n",
        "Breakeven (strike - premium): ", money(x$breakeven), "\n",
        x$max_profit_note
      )
    } else {
      paste0(
        "Covered call summary\n",
        common,
        "Stock basis used: ", money(x$stock_basis), "\n",
        "Current stock price: ", money(x$stock_current), "\n\n",
        
        "Premium yield (reported) on COST BASIS: ", pcts(pct(x$reported_ret_basis)), "\n",
        "Premium yield (reported) on CURRENT VALUE: ", pcts(pct(x$reported_ret_current)), "\n\n",
        
        "Max return if called (reported) on COST BASIS: ", pcts(pct(x$reported_max_ret_basis)), "\n",
        "Max return if called (reported) on CURRENT VALUE: ", pcts(pct(x$reported_max_ret_current)), "\n\n",
        
        "Effective sale price if called (strike + premium): ", money(x$cc_effective_sale)
      )
    }
  })
  
  output$metrics_table <- renderTable({
    x <- calc()
    
    if (x$strategy == "CSP") {
      data.frame(
        Metric = c(
          "Premium received (net)",
          "Capital base (cash secured)",
          "Yield (reported period)",
          "Yield (per-cycle / DTE)",
          "Breakeven price"
        ),
        Value = c(
          money(x$premium_total),
          money(x$capital_base),
          pcts(pct(x$reported_ret)),
          pcts(pct(x$cycle_ret)),
          money(x$breakeven)
        ),
        check.names = FALSE
      )
    } else {
      data.frame(
        Metric = c(
          "Premium received (net)",
          "Capital base (cost basis)",
          "Capital base (current value)",
          "Premium yield (reported) on cost basis",
          "Premium yield (reported) on current value",
          "Premium yield (per-cycle) on cost basis",
          "Premium yield (per-cycle) on current value",
          "Upside per share (if called, vs basis)",
          "Max profit if called (dollars)",
          "Max return (reported) on cost basis",
          "Max return (reported) on current value",
          "Effective sale price (strike + premium)"
        ),
        Value = c(
          money(x$premium_total),
          money(x$basis_capital),
          money(x$current_capital),
          pcts(pct(x$reported_ret_basis)),
          pcts(pct(x$reported_ret_current)),
          pcts(pct(x$cycle_ret_basis)),
          pcts(pct(x$cycle_ret_current)),
          money(x$upside_per_share),
          money(x$max_profit),
          pcts(pct(x$reported_max_ret_basis)),
          pcts(pct(x$reported_max_ret_current)),
          money(x$cc_effective_sale)
        ),
        check.names = FALSE
      )
    }
  }, striped = TRUE, spacing = "s", width = "100%")
  
  output$formula_text <- renderText({
    x <- calc()
    
    base <- paste0(
      "Common:\n",
      "gross_premium = premium_per_share * contracts * multiplier\n",
      "premium_total = gross_premium - fees\n\n",
      "Per-cycle return (decimal) = premium_total / capital_base\n",
      "Per-cycle return (percent) = (premium_total / capital_base) * 100\n\n",
      "Reporting period selected: ", x$report_period,
      if (x$report_period == "Custom (X days)") paste0(" (", x$custom_days, " days)") else "",
      "\n",
      if (x$report_period == "Annualized") paste0("Annualization: ", x$day_basis, " day basis, ", x$annual_method, "\n") else "",
      "\n"
    )
    
    period_math <- paste0(
      "Period conversion (decimal):\n",
      "- Per-cycle: reported = cycle\n",
      "- Weekly: reported = cycle * (7 / DTE)\n",
      "- Bi-weekly: reported = cycle * (14 / DTE)\n",
      "- Monthly: reported = cycle * (30.4375 / DTE)\n",
      "- Quarterly: reported = cycle * (91.3125 / DTE)\n",
      "- Custom (X days): reported = cycle * (X / DTE)\n",
      "- Annualized (Simple): reported = cycle * (DayBasis / DTE)\n",
      "- Annualized (Compounding): reported = (1 + cycle)^(DayBasis / DTE) - 1\n\n"
    )
    
    if (x$strategy == "CSP") {
      paste0(
        base,
        period_math,
        "CSP:\n",
        "cash_secured = strike * contracts * multiplier\n",
        "cycle = premium_total / cash_secured\n",
        "breakeven = strike - premium_per_share\n"
      )
    } else {
      paste0(
        base,
        period_math,
        "Covered Call:\n",
        "basis_capital = stock_basis_per_share * contracts * multiplier\n",
        "current_capital = current_stock_price * contracts * multiplier\n",
        "cycle_premium_yield_basis = premium_total / basis_capital\n",
        "cycle_premium_yield_current = premium_total / current_capital\n\n",
        "If called away:\n",
        "upside_per_share = max(0, strike - stock_basis_per_share)\n",
        "max_profit = premium_total + (upside_per_share * contracts * multiplier)\n",
        "cycle_max_return_basis = max_profit / basis_capital\n",
        "cycle_max_return_current = max_profit / current_capital\n",
        "effective_sale_price = strike + premium_per_share\n"
      )
    }
  })
}
