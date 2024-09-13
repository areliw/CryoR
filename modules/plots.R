# modules/plots.R

plots_module <- function(input, output, data_clean) {
  # Q-Q plot for Cryo Volume
  output$qq_plot <- renderPlot({
    data <- data_clean()
    req(data)
    
    ggplot(data, aes(sample = as.numeric(`Cryo Volume (ml/unit)`))) +
      stat_qq() +
      stat_qq_line() +
      labs(title = "Q-Q Plot for Cryo Volume",
           x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_minimal()
  })
  
  # Histogram of Time Intervals
  observeEvent(input$plot_histogram, {
    data <- data_clean()
    req(data)
    
    output$histogram_plot <- renderPlot({
      ggplot(data, aes(x = time_interval)) +
        geom_bar(fill = "skyblue", color = "black") +
        labs(title = "Histogram of Time Intervals",
             x = "Time Interval", y = "Count") +
        theme_minimal()
    })
  })
  
  # Scatter Plot
  output$scatter_plot <- renderPlot({
    data <- data_clean()
    req(data)
    
    ggplot(data, aes(x = as.numeric(`FFP Volume (ml)`), y = as.numeric(`Cryo Volume (ml/unit)`))) +
      geom_point() +
      labs(title = "Scatter Plot of Cryo Volume vs FFP Volume",
           x = "FFP Volume (ml)", y = "Cryo Volume (ml/unit)") +
      theme_minimal()
  })
}
