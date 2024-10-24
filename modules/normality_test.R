# modules/normality_test.R
normality_test_module <- function(input, output, data_clean) {
  # ฟังก์ชันสำหรับทดสอบการแจกแจงแบบปกติ
  perform_normality_test <- function(data, var_name) {
    tryCatch({
      # ตรวจสอบและทำความสะอาดข้อมูล
      test_data <- as.numeric(data[[var_name]])
      test_data <- test_data[!is.na(test_data)]
      
      if (length(test_data) < 3) {
        return(list(
          success = FALSE,
          message = paste("ไม่สามารถทำการทดสอบ", var_name, 
                         "ได้เนื่องจากจำนวนข้อมูลน้อยเกินไป")
        ))
      }
      
      # ทำ Shapiro-Wilk test
      test_result <- shapiro.test(test_data)
      
      # คำนวณ skewness และ kurtosis
      n <- length(test_data)
      z_scores <- scale(test_data)
      skewness <- sum(z_scores^3)/n
      kurtosis <- sum(z_scores^4)/n - 3
      
      return(list(
        success = TRUE,
        var_name = var_name,
        n = length(test_data),
        w_stat = test_result$statistic,
        p_value = test_result$p.value,
        skewness = skewness,
        kurtosis = kurtosis,
        interpretation = if(test_result$p.value > 0.05) 
          "มีการแจกแจงแบบปกติ" else "ไม่มีการแจกแจงแบบปกติ"
      ))
      
    }, error = function(e) {
      return(list(
        success = FALSE,
        message = paste("เกิดข้อผิดพลาดในการทดสอบ", var_name, ":", e$message)
      ))
    })
  }
  
  # Event handler for normality test button
  observeEvent(input$test_normality, {
    data <- data_clean()
    req(data)
    
    # Variables to test
    vars <- c("FFP Volume (ml)", 
              "Cryo Volume (ml/unit)", 
              "Fibrinogen (mg)")
    
    # Perform tests
    test_results <- lapply(vars, function(var) {
      perform_normality_test(data, var)
    })
    
    # Generate output
    output$normality_result <- renderText({
      result_text <- character(0)
      
      for (result in test_results) {
        if (result$success) {
          result_text <- c(result_text, 
            paste0("\nผลการทดสอบสำหรับ ", result$var_name, ":",
                  "\nจำนวนข้อมูล: ", result$n,
                  "\nShapiro-Wilk Test:",
                  "\n  W = ", round(result$w_stat, 4),
                  "\n  p-value = ", format.pval(result$p_value, digits = 4),
                  "\nSkewness = ", round(result$skewness, 4),
                  "\nKurtosis = ", round(result$kurtosis, 4),
                  "\nการแปลผล: ", result$interpretation,
                  "\n-------------------"))
        } else {
          result_text <- c(result_text, 
                          paste0("\n", result$message, "\n-------------------"))
        }
      }
      
      paste(result_text, collapse = "\n")
    })
  })
}
