# modules/correlation_analysis.R

correlation_analysis_module <- function(input, output, data_clean) {
  correlation_result <- reactiveVal(NULL)

  observeEvent(input$analyze_correlation, {
    data <- data_clean()
    req(data)

    result <- cor(data$`Cryo Volume (ml/unit)`, data$`FFP Volume (ml)`, use = "complete.obs")
    correlation_result(result)
  })

  output$correlation_result <- renderText({
    result <- correlation_result()
    req(result)
    paste("Correlation between Cryo Volume and FFP Volume:", round(result, 4))
  })
}
