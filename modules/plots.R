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
        plot.title = element_text(size = 16, face = "bold", color = "#00BCD4"),
        plot.subtitle = element_text(size = 12, color = "#B0BEC5"),
        axis.title = element_text(size = 12, color = "#ECEFF1"),
        axis.text = element_text(size = 10, color = "#ECEFF1"),
        legend.title = element_text(size = 12, color = "#ECEFF1"),
        legend.text = element_text(size = 10, color = "#ECEFF1"),
        panel.background = element_rect(fill = "#1E1E1E"),
        plot.background = element_rect(fill = "#1E1E1E"),
        panel.grid.major = element_line(color = "#2A2A2A"),
        panel.grid.minor = element_line(color = "#2A2A2A"),
        strip.background = element_rect(fill = "#2A2A2A"),
        strip.text = element_text(color = "#ECEFF1")
      )
  }

  # Q-Q plot
  output$qq_plot <- renderPlot({
    data <- data_clean()
    req(data)
    validate(need(validate_plot_data(data, "Cryo Volume (ml/unit)"),
                 "ไม่พบข้อมูลที่จำเป็นสำหรับการสร้าง Q-Q Plot"))

    # กรองค่า NA
    qq_data <- data$`Cryo Volume (ml/unit)`[!is.na(data$`Cryo Volume (ml/unit)`)]

    # สร้าง Q-Q plot
    ggplot(data.frame(value = qq_data), aes(sample = value)) +
      stat_qq(color = "#00BCD4") +
      stat_qq_line(color = "#FF5252", linetype = "dashed") +
      labs(
        title = "Normal Q-Q Plot ของ Cryo Volume",
        subtitle = paste("n =", length(qq_data), "ตัวอย่าง"),
        x = "Theoretical Quantiles",
        y = "Sample Quantiles"
      ) +
      custom_theme()
  })

  # Histogram แยกตามช่วงเวลา
  observeEvent(input$plot_histogram, {
    data <- data_clean()
    req(data)
    validate(need(validate_plot_data(data, c("Cryo Volume (ml/unit)", "time_interval")),
                 "ไม่พบข้อมูลที่จำเป็นสำหรับการสร้าง Histogram"))

    output$histogram_plot <- renderPlot({
      # กรองค่า NA ออกจากข้อมูล
      plot_data <- data[!is.na(`Cryo Volume (ml/unit)`)]

      # คำนวณสถิติพื้นฐานสำหรับแต่ละช่วงเวลา
      stats <- plot_data[, .(
        mean = mean(`Cryo Volume (ml/unit)`, na.rm = TRUE),
        median = median(`Cryo Volume (ml/unit)`, na.rm = TRUE)
      ), by = time_interval]

      ggplot(plot_data, aes(x = `Cryo Volume (ml/unit)`)) +
        geom_histogram(aes(y = ..density..), 
                      binwidth = 1, 
                      fill = "#00BCD4", 
                      color = "#1E1E1E", 
                      alpha = 0.8) +
        geom_density(color = "#FFFFFF", size = 1) +
        geom_vline(data = stats, 
                  aes(xintercept = mean), 
                  color = "#FF5252", 
                  linetype = "dashed") +
        geom_vline(data = stats, 
                  aes(xintercept = median), 
                  color = "#8BC34A", 
                  linetype = "dashed") +
        facet_wrap(~ time_interval, 
                  ncol = 2, 
                  scales = "free_y") +
        labs(
          title = "การกระจายตัวของ Cryo Volume แยกตามระยะเวลาการเก็บรักษา",
          subtitle = "เส้นประสีแดง = ค่าเฉลี่ย, เส้นประสีเขียว = ค่ามัธยฐาน",
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
      scale_color_manual(values = c("#00BCD4", "#FF5252", "#8BC34A", "#FFC107", "#9C27B0", "#03A9F4")) +
      facet_wrap(~ time_interval, scales = "free") +
      geom_text(data = cors, 
                aes(x = Inf, y = -Inf, 
                    label = paste("r =", round(correlation, 3))),
                hjust = 1.1, 
                vjust = -0.5,
                color = "#ECEFF1",
                size = 4)
  })
}
