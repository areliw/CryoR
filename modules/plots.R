# modules/plots.R

plots_module <- function(input, output, data_clean) {
  # Q-Q plot for Cryo Volume
  output$qq_plot <- renderPlot({
    data <- data_clean()
    req(data)
    
    if (nrow(data) == 0) {
      showNotification("ไม่มีข้อมูลเพียงพอในการแสดงกราฟ Q-Q", type = "warning")
      return(NULL)
    }
    
    ggplot(data, aes(sample = `Cryo Volume (ml/unit)`)) +
      stat_qq() +
      stat_qq_line() +
      labs(title = "Q-Q Plot for Cryo Volume",
           x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_minimal()
  })

  # Histogram of Cryo Volume by Time Interval
  observeEvent(input$plot_histogram, {
    data <- data_clean()
    req(data)
    
    if (nrow(data) == 0) {
      showNotification("ไม่มีข้อมูลเพียงพอในการสร้าง Histogram", type = "error")
      return(NULL)
    }
    
    output$histogram_plot <- renderPlot({
      ggplot(data, aes(x = `Cryo Volume (ml/unit)`)) +
        geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
        facet_wrap(~ time_interval, ncol = 2, scales = "free_y") +
        labs(title = "Histogram of Cryo Volume by Time Interval",
             x = "Cryo Volume (ml/unit)", y = "Count") +
        theme_minimal()
    })
  })

  # Scatter Plot
  output$scatter_plot <- renderPlot({
    data <- data_clean()
    req(data)
    
    if (nrow(data) == 0) {
      showNotification("ไม่มีข้อมูลเพียงพอในการแสดง Scatter Plot", type = "warning")
      return(NULL)
    }
    
    ggplot(data, aes(x = `FFP Volume (ml)`, y = `Cryo Volume (ml/unit)`)) +
      geom_point() +
      labs(title = "Scatter Plot of Cryo Volume vs FFP Volume",
           x = "FFP Volume (ml)", y = "Cryo Volume (ml/unit)") +
      theme_minimal()
  })
}
