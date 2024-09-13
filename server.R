server <- function(input, output) {
  # ดึงข้อมูลจาก Google Sheets เมื่อกดปุ่ม
  data_clean <- eventReactive(input$load_data, {
    tryCatch({
      data <- read_sheet(sheet_url, sheet = "CryoR")  # ดึงข้อมูลจากแผ่น CryoR
      data$time_interval <- cut(
        as.numeric(data$`Cryo Storage Duration (Days)`),
        breaks = c(0, 1, 15, 30, 90, 180, Inf),
        labels = c("1 Day","2-15 days", "16-30 days", "1-3 months", "3-6 months", "6+ months"),
        right = FALSE
      )
      data <- na.omit(data)  # ลบข้อมูลที่มีค่า NA ออก
      data
    }, error = function(e) {
      showNotification("Error loading Google Sheets data.", type = "error")
      return(NULL)
    })
  })

  # แสดงตารางสถิติเชิงพรรณนา
  output$result_table <- renderTable({
    data <- data_clean()
    if (is.null(data)) return(NULL)
    
    vars <- c("FFP Volume (ml)", "Cryo Volume (ml/unit)", "Fibrinogen (mg)")
    
    stats_list <- lapply(vars, function(var) {
      x <- as.numeric(data[[var]])
      data.frame(
        Variable = var,
        Mean = mean(x, na.rm = TRUE),
        Median = median(x, na.rm = TRUE),
        SD = sd(x, na.rm = TRUE),
        Min = min(x, na.rm = TRUE),
        Max = max(x, na.rm = TRUE)
      )
    })
    
    result_table <- do.call(rbind, stats_list)
    
    return(result_table)
  })

  # Q-Q plot for Cryo Volume
  output$qq_plot <- renderPlot({
    data <- data_clean()
    if (is.null(data)) return(NULL)

    ggplot(data, aes(sample = `Cryo Volume (ml/unit)`)) +
      stat_qq() +
      stat_qq_line() +
      labs(title = "Q-Q Plot for Cryo Volume",
           x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_minimal()
  })
  
  # ทดสอบการกระจายแบบปกติ (Shapiro-Wilk Test)
  observeEvent(input$test_normality, {
    data <- data_clean()
    if (is.null(data)) return(NULL)
    
    test_result <- shapiro.test(data$`Cryo Volume (ml/unit)`)
    output$normality_result <- renderText({
      paste("Shapiro-Wilk Test: W =", round(test_result$statistic, 4), 
            "p-value =", format.p.value(test_result$p.value, digits = 4))
    })
  })

  # วิเคราะห์ความสัมพันธ์
  observeEvent(input$analyze_correlation, {
    data <- data_clean()
    if (is.null(data)) return(NULL)
    
    correlation_result <- cor(data$`Cryo Volume (ml/unit)`, data$`FFP Volume (ml)`, use = "complete.obs")
    output$correlation_result <- renderText({
      paste("Correlation between Cryo Volume and FFP Volume:", round(correlation_result, 4))
    })
  })

  # วิเคราะห์ Power
  observeEvent(input$power_analysis, {
    tryCatch({
      power_result <- pwr::pwr.t.test(d = input$effect_size, 
                                      sig.level = input$significance_level, 
                                      power = input$power, 
                                      type = "two.sample")
      output$power_result <- renderText({
        paste("Sample Size needed:", ceiling(power_result$n))
      })
    }, error = function(e) {
      output$power_result <- renderText("Error in power analysis: check your inputs.")
    })
  })

  # แสดง Histogram ของ Time Intervals
  output$histogram_plot <- renderPlot({
    data <- data_clean()
    if (is.null(data)) return(NULL)
    req(input$plot_histogram)
    
    ggplot(data, aes(x = time_interval)) +
      geom_bar(fill = "skyblue", color = "black") +
      labs(title = "Histogram of Time Intervals",
           x = "Time Interval", y = "Count") +
      theme_minimal()
  })
  
  # สร้าง Scatter Plot
  output$scatter_plot <- renderPlot({
    data <- data_clean()
    if (is.null(data)) return(NULL)
    
    ggplot(data, aes(x = `FFP Volume (ml)`, y = `Cryo Volume (ml/unit)`)) +
      geom_point() +
      labs(title = "Scatter Plot of Cryo Volume vs FFP Volume",
           x = "FFP Volume (ml)", y = "Cryo Volume (ml/unit)") +
      theme_minimal()
  })
}
