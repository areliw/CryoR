# modules/normality_test.R

normality_test_module <- function(input, output, data_clean) {
  observeEvent(input$test_normality, {
    data <- data_clean()
    req(data)
    
    test_result <- shapiro.test(as.numeric(data$`Cryo Volume (ml/unit)`))
    output$normality_result <- renderText({
      paste("Shapiro-Wilk Test: W =", round(test_result$statistic, 4), 
            "p-value =", format.p.value(test_result$p.value, digits = 4))
    })
  })
}
