# modules/plots.R
plots_module <- function(input, output, data_clean) {
  # Helper function สำหรับการตรวจสอบข้อมูล
  validate_plot_data <- function(data, required_cols) {
    if (is.null(data) || nrow(data) == 0) {
      return(FALSE)
    }
    all(required_cols %in% names(data))
  }

  # Helper function สำหรับ theme ของกราฟ
  custom_theme <- function() {
    theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95")
      )
  }

  # Q-Q plot
  output$qq_plot <- renderPlot({
    data <- data_clean()
    req(data)
    validate(need(validate_plot_data(data, "Cryo Volume (ml/unit)"),
                 "ไม่พบข้อมูลที่จำเป็นสำหรับการสร้าง Q-Q Plot"))
    
    # สร้าง Q-Q plot พร้อม confidence bands
    qq_data <- data$`Cryo Volume (ml/unit)`[!is.na(data$`Cryo Volume (ml/unit)`)]
    
    ggplot(data.frame(value = qq_data), aes(sample = value)) +
      stat_qq() +
      stat_qq_line(color = "red", linetype = "dashed") +
      labs(
        title = "Normal Q-Q Plot ของ Cryo Volume",
        subtitle = paste("n =", length(qq_data), "observations"),
        x = "Theoretical Quantiles",
        y = "Sample Quantiles"
      ) +
      custom_theme()
  })

  # Histogram by Time Interval
  observeEvent(input$plot_histogram, {
    data <- data_clean()
    req(data)
    validate(need(validate_plot_data(data, c("Cryo Volume (ml/unit)", "time_interval")),
                 "ไม่พบข้อมูลที่จำเป็นสำหรับการสร้าง Histogram"))
    
    output$histogram_plot <- renderPlot({
      # คำนวณค่าสถิติพื้นฐานสำหรับแต่ละช่วงเวลา
      stats <- data[, .(
        mean = mean(`Cryo Volume (ml/unit)`, na.rm = TRUE),
        median = median(`Cryo Volume (ml/unit)`, na.rm = TRUE)
      ), by = time_interval]
      
      ggplot(data, aes(x = `Cryo Volume (ml/unit)`)) +
        geom_histogram(aes(y = ..density..), 
                      binwidth = 1, 
                      fill = "skyblue", 
                      color = "black", 
                      alpha = 0.7) +
        geom_density(color = "red", size = 1) +
        geom_vline(data = stats, 
                  aes(xintercept = mean), 
                  color = "blue", 
                  linetype = "dashed") +
        geom_vline(data = stats, 
                  aes(xintercept = median), 
                  color = "green", 
                  linetype = "dashed") +
        facet_wrap(~ time_interval, 
                  ncol = 2, 
                  scales = "free_y") +
        labs(
          title = "การกระจายของ Cryo Volume ตามช่วงเวลา",
          subtitle = "เส้นประน้ำเงิน = ค่าเฉลี่ย, เส้นประเขียว = ค่ามัธยฐาน",
          x = "Cryo Volume (ml/unit)",
          y = "Density"
        ) +
        custom_theme()
    })
  })

  # Scatter Plot
  output$scatter_plot <- renderPlot({
    data <- data_clean()
    req(data)
    validate(need(validate_plot_data(data, c("FFP Volume (ml)", "Cryo Volume (ml/unit)", "time_interval")),
                 "ไม่พบข้อมูลที่จำเป็นสำหรับการสร้าง Scatter Plot"))
    
    # คำนวณ correlation สำหรับแต่ละช่วงเวลา
    cors <- data[, .(
      correlation = cor(`FFP Volume (ml)`, `Cryo Volume (ml/unit)`, 
                       use = "complete.obs")
    ), by = time_interval]
    
    ggplot(data, aes(x = `FFP Volume (ml)`, 
                     y = `Cryo Volume (ml/unit)`, 
                     color = time_interval)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = TRUE) +
      labs(
        title = "ความสัมพันธ์ระหว่าง FFP Volume และ Cryo Volume",
        subtitle = "แยกตามช่วงเวลาการเก็บรักษา",
        x = "FFP Volume (ml)",
        y = "Cryo Volume (ml/unit)",
        color = "ช่วงเวลา"
      ) +
      custom_theme() +
      facet_wrap(~ time_interval, scales = "free") +
      geom_text(data = cors, 
                aes(x = -Inf, y = Inf, 
                    label = paste("r =", round(correlation, 3))),
                hjust = -0.1, 
                vjust = 2,
                size = 4)
  })
}
