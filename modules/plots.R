# modules/plots.R

plots_module <- function(input, output, data_clean) {
  # Q-Q plot for Cryo Volume
  output$qq_plot <- renderPlot({
    data <- data_clean()
    req(data)
    
    ggplot(data, aes(sample = data$`Cryo Volume (ml/unit)`)) +
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
    
    # แปลงคอลัมน์ 'Cryo Volume (ml/unit)' เป็นตัวเลขและลบค่า NA
    data <- data[!is.na(data$`Cryo Volume (ml/unit)`), ]
    data$`Cryo Volume (ml/unit)` <- as.numeric(data$`Cryo Volume (ml/unit)`)
    
    # สร้างตารางสรุปจำนวน Cryo ในแต่ละช่วงเวลา
    summary_table <- data[, .N, by = .(time_interval)]
    setnames(summary_table, "N", "Count")
    
    # สร้างกราฟ Histogram
    output$histogram_plot <- renderPlot({
      ggplot(summary_table, aes(x = time_interval, y = Count, fill = time_interval)) +
        geom_bar(stat = "identity") +
        labs(title = "Histogram of Cryo Counts by Time Interval",
             x = "Time Interval", y = "Number of Cryo Units") +
        theme_minimal() +
        theme(legend.position = "none")
    })
  })
  
  # Scatter Plot
  output$scatter_plot <- renderPlot({
    data <- data_clean()
    req(data)
    
    ggplot(data, aes(x = `FFP Volume (ml)`, y = `Cryo Volume (ml/unit)`)) +
      geom_point() +
      labs(title = "Scatter Plot of Cryo Volume vs FFP Volume",
           x = "FFP Volume (ml)", y = "Cryo Volume (ml/unit)") +
      theme_minimal()
  })
}
