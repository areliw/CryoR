# R/utils.R

#' Format numbers for display
#' @param x Numeric value to format
#' @param digits Number of decimal places
#' @return Formatted number as character
format_numbers <- function(x, digits = 2) {
  if (is.numeric(x)) {
    return(round(x, digits))
  }
  return(x)
}

#' Calculate percentiles safely
#' @param x Numeric vector
#' @param probs Probability values for quantiles
#' @return Vector of quantiles
calculate_percentiles <- function(x, probs = c(0.25, 0.5, 0.75)) {
  if (length(x[!is.na(x)]) == 0) return(rep(NA, length(probs)))
  quantile(x, probs = probs, na.rm = TRUE)
}

#' Check for outliers using IQR method
#' @param x Numeric vector
#' @return Named vector with counts of low and high outliers
check_outliers <- function(x) {
  if (length(x[!is.na(x)]) < 4) return(c(low = NA, high = NA))
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lower <- q[1] - 1.5 * iqr
  upper <- q[2] + 1.5 * iqr
  c(low = sum(x < lower, na.rm = TRUE), 
    high = sum(x > upper, na.rm = TRUE))
}

#' Generate custom ggplot theme
#' @return ggplot theme object
custom_theme <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )
}

#' Interpret correlation coefficient
#' @param r Correlation coefficient
#' @return Character string with interpretation
interpret_correlation <- function(r) {
  if (is.na(r)) return("ไม่สามารถคำนวณได้")
  if (abs(r) >= 0.8) return("มีความสัมพันธ์สูงมาก")
  if (abs(r) >= 0.6) return("มีความสัมพันธ์สูง")
  if (abs(r) >= 0.4) return("มีความสัมพันธ์ปานกลาง")
  if (abs(r) >= 0.2) return("มีความสัมพันธ์ต่ำ")
  return("มีความสัมพันธ์ต่ำมาก")
}

#' Validate numeric data
#' @param data Data frame
#' @param columns Column names to validate
#' @return List with validation results
validate_numeric_data <- function(data, columns) {
  results <- list(
    valid = TRUE,
    messages = character(0)
  )
  
  for (col in columns) {
    if (!col %in% names(data)) {
      results$valid <- FALSE
      results$messages <- c(results$messages,
                          paste("ไม่พบคอลัมน์:", col))
      next
    }
    
    values <- data[[col]]
    n_missing <- sum(is.na(values))
    n_invalid <- sum(!is.numeric(values))
    
    if (n_missing > 0) {
      results$messages <- c(results$messages,
                          paste(col, "มีค่า NA", n_missing, "ค่า"))
    }
    if (n_invalid > 0) {
      results$valid <- FALSE
      results$messages <- c(results$messages,
                          paste(col, "มีค่าที่ไม่ใช่ตัวเลข", n_invalid, "ค่า"))
    }
  }
  
  return(results)
}

#' Create summary statistics
#' @param x Numeric vector
#' @param var_name Variable name
#' @return Data frame with summary statistics
create_summary_stats <- function(x, var_name) {
  stats <- compute_stats(x[!is.na(x)])  # ใช้ C++ function ที่มีอยู่
  outliers <- check_outliers(x)
  q <- calculate_percentiles(x)
  
  data.frame(
    Variable = var_name,
    N = length(x[!is.na(x)]),
    Missing = sum(is.na(x)),
    Mean = stats[1],
    SD = stats[2],
    CV = (stats[2] / stats[1]) * 100,
    Median = q[2],
    Q1 = q[1],
    Q3 = q[3],
    Min = stats[3],
    Max = stats[4],
    Outliers_Low = outliers["low"],
    Outliers_High = outliers["high"]
  )
}

#' Format p-values
#' @param p P-value
#' @param digits Number of decimal places
#' @return Formatted p-value as character
format_pvalue <- function(p, digits = 4) {
  if (is.na(p)) return("NA")
  if (p < 0.0001) return("p < 0.0001")
  return(paste("p =", format(round(p, digits), nsmall = digits)))
}

#' Create formatted data table
#' @param data Data frame
#' @param caption Table caption
#' @param filter Include filter
#' @return DT datatable object
create_formatted_dt <- function(data, caption = NULL, filter = 'top') {
  DT::datatable(
    data,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel'),
      columnDefs = list(
        list(className = 'dt-center', targets = '_all')
      )
    ),
    rownames = FALSE,
    caption = if (!is.null(caption)) {
      htmltools::tags$caption(
        style = 'caption-side: top; text-align: center; color: black; font-size: 16px;',
        caption
      )
    },
    filter = filter
  )
}

#' Create error notification
#' @param message Error message
show_error <- function(message) {
  showNotification(
    message,
    type = "error",
    duration = NULL
  )
}

#' Ensure plot device is initialized
#' @return NULL
ensure_plot_device <- function() {
  if (!dev.cur()) {
    pdf(NULL)
  }
}
