# helpers.R

pct <- function(x) {
  if (is.na(x) || is.nan(x) || is.infinite(x)) return(NA_real_)
  100 * x
}

safe_div <- function(num, den) {
  if (is.na(num) || is.na(den) || den == 0) return(NA_real_)
  num / den
}

num_or_zero <- function(x) {
  if (is.null(x) || is.na(x)) return(0)
  x
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

period_days <- function(period_label, day_basis, custom_days = NA_real_) {
  if (period_label == "Per-cycle (DTE)") return(NA_real_)
  if (period_label == "Weekly") return(7)
  if (period_label == "Bi-weekly") return(14)
  if (period_label == "Monthly") return(30.4375)
  if (period_label == "Quarterly") return(91.3125)
  if (period_label == "Annualized") return(day_basis)
  if (period_label == "Custom (X days)") return(custom_days)
  NA_real_
}

scale_return <- function(cycle_ret_decimal, dte, period_label,
                         day_basis, annual_method, custom_days) {
  
  if (is.na(cycle_ret_decimal) || is.na(dte) || dte <= 0) return(NA_real_)
  
  if (period_label == "Per-cycle (DTE)") return(cycle_ret_decimal)
  
  p_days <- period_days(period_label, day_basis, custom_days)
  if (is.na(p_days) || p_days <= 0) return(NA_real_)
  
  if (period_label %in% c(
    "Weekly", "Bi-weekly", "Monthly", "Quarterly", "Custom (X days)"
  )) {
    return(cycle_ret_decimal * (p_days / dte))
  }
  
  if (annual_method == "Simple") {
    return(cycle_ret_decimal * (p_days / dte))
  } else {
    (1 + cycle_ret_decimal)^(p_days / dte) - 1
  }
}

money <- function(x) ifelse(is.na(x), "NA", sprintf("$%.2f", x))
pcts  <- function(x) ifelse(is.na(x), "NA", sprintf("%.2f%%", x))
