# modules/correlation_analysis.R

correlation_analysis_module <- function(input, output, data_clean) {
  observeEvent(input$analyze_correlation, {
    data <- data_clean()
    req(data)
    
    correlation_result <- cor(as.numeric(data$`Cryo Volume (ml/unit)`), as.numeric(data$`FFP Volume (ml)`), use = "complete.obs")
    output$correlation_result <- renderText({
      paste("Correlation between Cryo Volume and FFP Volume:", round(correlation_result, 4))
    })
  })
}
