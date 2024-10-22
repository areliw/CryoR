# modules/correlation_analysis.R

correlation_analysis_module <- function(input, output, data_clean) {
  # เก็บค่าเดิม
  correlation_result <- reactiveVal(NULL)

  # ส่วนเดิม - วิเคราะห์ correlation ทั่วไป
  observeEvent(input$analyze_correlation, {
    data <- data_clean()
    req(data)

    result <- cor(data$`Cryo Volume (ml/unit)`, data$`FFP Volume (ml)`, use = "complete.obs")
    correlation_result(result)
  })

  # ส่วนเดิม - แสดงผล correlation ทั่วไป
  output$correlation_result <- renderText({
    result <- correlation_result()
    req(result)
    paste("Correlation between Cryo Volume and FFP Volume:", round(result, 4))
  })

  # เพิ่มส่วนใหม่ - วิเคราะห์ Cryo Volume 10-15 ml
  filtered_data <- reactive({
    req(data_clean())
    data_clean()[between(`Cryo Volume (ml/unit)`, 10, 15)]
  })

  # สร้างตารางสรุปสำหรับ Cryo 10-15 ml
  output$cryo_fibrinogen_table <- renderTable({
    data <- filtered_data()
    req(data)
    
    # คำนวณค่าสถิติ
    stats <- data[, .(
      "จำนวนตัวอย่าง" = .N,
      "Fibrinogen เฉลี่ย (mg)" = mean(`Fibrinogen (mg)`, na.rm = TRUE),
      "SD Fibrinogen" = sd(`Fibrinogen (mg)`, na.rm = TRUE),
      "Min Fibrinogen" = min(`Fibrinogen (mg)`, na.rm = TRUE),
      "Max Fibrinogen" = max(`Fibrinogen (mg)`, na.rm = TRUE),
      "จำนวนตัวอย่างที่ผ่านเกณฑ์" = sum(`Fibrinogen (mg)` >= 140, na.rm = TRUE),
      "ร้อยละที่ผ่านเกณฑ์" = 100 * mean(`Fibrinogen (mg)` >= 140, na.rm = TRUE)
    )]
    
    # จัดรูปแบบตัวเลข
    stats[] <- lapply(stats, function(x) if(is.numeric(x)) round(x, 2) else x)
    stats
  })

  # สร้างกราฟสำหรับ Cryo 10-15 ml
  output$cryo_fibrinogen_plot <- renderPlot({
    data <- filtered_data()
    req(data)
    
    ggplot(data, aes(x = `Cryo Volume (ml/unit)`, y = `Fibrinogen (mg)`)) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE) +
      geom_hline(yintercept = 140, color = "red", linetype = "dashed") +
      theme_minimal() +
      labs(
        title = "ความสัมพันธ์ระหว่าง Cryo Volume (10-15 ml) และ Fibrinogen",
        subtitle = "เส้นประแดงแสดงค่ามาตรฐาน Fibrinogen (140 mg)",
        x = "Cryo Volume (ml/unit)",
        y = "Fibrinogen (mg)"
      )
  })
}
