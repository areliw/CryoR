# modules/descriptive_stats.R

descriptive_stats_module <- function(input, output, data_clean) {
  output$result_table <- renderTable({
    data <- data_clean()
    req(data)
    
    vars <- c("FFP Volume (ml)", "Cryo Volume (ml/unit)", "Fibrinogen (mg)")
    result_table <- data.table(Variable = character(), Mean = numeric(), SD = numeric(), Min = numeric(), Max = numeric())
    
    for (var in vars) {
      x <- as.numeric(data[[var]])
      stats <- compute_stats(x)
      result_table <- rbind(result_table, data.table(
        Variable = var,
        Mean = stats[1],
        SD = stats[2],
        Min = stats[3],
        Max = stats[4]
      ))
    }
    
    return(result_table)
  })
}
