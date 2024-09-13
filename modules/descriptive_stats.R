# modules/descriptive_stats.R

descriptive_stats_module <- function(input, output, data_clean) {
  output$result_table <- renderTable({
    data <- data_clean()
    req(data)

    vars <- c("FFP Volume (ml)", "Cryo Volume (ml/unit)", "Fibrinogen (mg)")
    result_table <- lapply(vars, function(var) {
      x <- data[[var]]
      data.frame(
        Variable = var,
        Mean = mean(x, na.rm = TRUE),
        SD = sd(x, na.rm = TRUE),
        Min = min(x, na.rm = TRUE),
        Max = max(x, na.rm = TRUE)
      )
    })
    result_table <- do.call(rbind, result_table)
    return(result_table)
  })
}
