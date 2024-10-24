# modules/descriptive_stats.R
descriptive_stats_module <- function(input, output, data_clean) {
  # ฟังก์ชันคำนวณสถิติเชิงพรรณนา
  calculate_descriptive_stats <- function(x, var_name) {
    tryCatch({
      x_clean <- x[!is.na(x)]
      if (length(x_clean) == 0) {
        return(data.frame(
          Variable = var_name,
          N = 0,
          Mean = NA,
          SD = NA,
          Median = NA,
          Min = NA,
          Max = NA,
          Q1 = NA,
          Q3 = NA
        ))
      }
      
      # ใช้ C++ function ที่มีอยู่
      basic_stats <- compute_stats(x_clean)
      
      # คำนวณสถิติเพิ่มเติม
      quantiles <- quantile(x_clean, probs = c(0.25, 0.5, 0.75))
      
      data.frame(
        Variable = var_name,
        N = length(x_clean),
        Mean = round(basic_stats[1], 2),
        SD = round(basic_stats[2], 2),
        Median = round(quantiles[2], 2),
        Min = basic_stats[3],
        Max = basic_stats[4],
        Q1 = round(quantiles[1], 2),
        Q3 = round(quantiles[3], 2)
      )
    }, error = function(e) {
      warning(paste("Error calculating stats for", var_name, ":", e$message))
      return(NULL)
    })
  }
  
  # Render table with error handling
  output$result_table <- renderDT({
    data <- data_clean()
    req(data)
    
    # Variables to analyze
    vars <- c("FFP Volume (ml)", 
              "Cryo Volume (ml/unit)", 
              "Fibrinogen (mg)")
    
    # Calculate stats for each variable
    result_list <- lapply(vars, function(var) {
      calculate_descriptive_stats(data[[var]], var)
    })
    
    # Remove NULL results and combine
    result_table <- do.call(rbind, result_list[!sapply(result_list, is.null)])
    
    # Render with datatable
    datatable(result_table,
             options = list(
               pageLength = 10,
               dom = 't',
               scrollX = TRUE
             ),
             rownames = FALSE) %>%
      formatRound(columns = c("Mean", "SD", "Median", "Q1", "Q3"), digits = 2)
  })
}
