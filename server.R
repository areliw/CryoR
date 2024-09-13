server <- function(input, output) {
  # ดึงข้อมูลจาก Google Sheets เมื่อกดปุ่ม
  data_clean <- eventReactive(input$load_data, {
    tryCatch({
      data <- read_sheet(sheet_url, sheet = "CryoR")  # ดึงข้อมูลจากแผ่น CryoR
      data$time_interval <- cut(
        as.numeric(data$`Cryo Storage Duration (Days)`),
        breaks = c(0, 1, 15, 30, 90, 180, Inf),
        labels = c("1 Days","2-15 days", "16-30 days", "1-3 months", "3-6 months", "6+ months")
      )
      na.omit(data)  # ลบข้อมูลที่มีค่า NA ออก
    }, error = function(e) {
      showNotification("Error loading Google Sheets data.", type = "error")
      return(NULL)
    })
  })

  # แสดงตารางสถิติเชิงพรรณนา
  output$result_table <- renderTable({
    data <- data_clean()
    if (is.null(data)) return(NULL)
    
    mean_ffp <- mean(data$`FFP Volume (ml)`, na.rm = TRUE)
    median_ffp <- median(data$`FFP Volume (ml)`, na.rm = TRUE)
    sd_ffp <- sd(data$`FFP Volume (ml)`, na.rm = TRUE)
    min_ffp <- min(data$`FFP Volume (ml)`, na.rm = TRUE)
    max_ffp <- max(data$`FFP Volume (ml)`, na.rm = TRUE)
    
    mean_cryo <- mean(data$`Cryo Volume (ml/unit)`, na.rm = TRUE)
    median_cryo <- median(data$`Cryo Volume (ml/unit)`, na.rm = TRUE)
    sd_cryo <- sd(data$`Cryo Volume (ml/unit)`, na.rm = TRUE)
    min_cryo <- min(data$`Cryo Volume (ml/unit)`, na.rm = TRUE)
    max_cryo <- max(data$`Cryo Volume (ml/unit)`, na.rm = TRUE)
    
    mean_fibrinogen <- mean(data$`Fibrinogen (mg)`, na.rm = TRUE)
    median_fibrinogen <- median(data$`Fibrinogen (mg)`, na.rm = TRUE)
    sd_fibrinogen <- sd(data$`Fibrinogen (mg)`, na.rm = TRUE)
    min_fibrinogen <- min(data$`Fibrinogen (mg)`, na.rm = TRUE)
    max_fibrinogen <- max(data$`Fibrinogen (mg)`, na.rm = TRUE)
    
    result_table <- data.frame(
      Variable = c('FFP Volume (ml)', 'Cryo Volume (ml)', 'Fibrinogen (mg)'),
      Mean = c(mean_ffp, mean_cryo, mean_fibrinogen),
      Median = c(median_ffp, median_cryo, median_fibrinogen),
      SD = c(sd_ffp, sd_cryo, sd_fibrinogen),
      Min = c(min_ffp, min_cryo, min_fibrinogen),
      Max = c(max_ffp, max_cryo, max_fibrinogen)
    )
    
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
            "p-value =", round(test_result$p.value, 4))
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
    power_result <- pwr::pwr.t.test(d = input$effect_size, 
                                    sig.level = input$significance_level, 
                                    power = input$power, 
                                    type = "two.sample")
    output$power_result <- renderText({
      paste("Sample Size needed:", ceiling(power_result$n))
    })
  })

  # แสดง Histogram ของ Time Intervals
  observeEvent(input$plot_histogram, {
    data <- data_clean()
    if (is.null(data)) return(NULL)
    
    output$histogram_plot <- renderPlot({
      ggplot(data, aes(x = time_interval)) +
        geom_histogram(stat = "count", fill = "skyblue", color = "black") +
        labs(title = "Histogram of Time Intervals",
             x = "Time Interval", y = "Count") +
        theme_minimal()
    })
  })
}
