# modules/correlation_analysis.R

correlation_analysis_module <- function(input, output, data_clean) {
  # Reactive values สำหรับเก็บผลการวิเคราะห์
  correlation_result <- reactiveVal(NULL)
  detailed_results <- reactiveVal(NULL)

  # ฟังก์ชันสำหรับแปลผลค่าสหสัมพันธ์
  interpret_correlation <- function(r) {
    if (is.na(r)) return("ไม่สามารถคำนวณได้")
    if (abs(r) >= 0.8) return("มีความสัมพันธ์สูงมาก")
    if (abs(r) >= 0.6) return("มีความสัมพันธ์สูง")
    if (abs(r) >= 0.4) return("มีความสัมพันธ์ปานกลาง")
    if (abs(r) >= 0.2) return("มีความสัมพันธ์ต่ำ")
    return("มีความสัมพันธ์ต่ำมาก")
  }

  # ฟังก์ชันสำหรับคำนวณ correlation ระหว่างตัวแปรทั้งหมด
  calculate_all_correlations <- function(data) {
    vars <- c("FFP Volume (ml)", "Cryo Volume (ml/unit)", "Fibrinogen (mg)")
    
    # สร้างเมทริกซ์เปล่าสำหรับเก็บผลลัพธ์
    n_vars <- length(vars)
    results <- data.frame(
      Variable1 = character(),
      Variable2 = character(),
      Correlation = numeric(),
      P_Value = numeric(),
      N = numeric(),
      Interpretation = character(),
      stringsAsFactors = FALSE
    )
    
    # คำนวณ correlation สำหรับทุกคู่ตัวแปร
    for(i in 1:(n_vars-1)) {
      for(j in (i+1):n_vars) {
        tryCatch({
          # ลบค่า NA
          valid_data <- data[!is.na(get(vars[i])) & !is.na(get(vars[j])), 
                            c(vars[i], vars[j]), with=FALSE]
          
          if(nrow(valid_data) < 3) {
            next
          }
          
          # คำนวณ correlation
          test_result <- cor.test(valid_data[[vars[i]]], 
                                valid_data[[vars[j]]], 
                                method="pearson")
          
          # เพิ่มผลลัพธ์ลงในตาราง
          results <- rbind(results, data.frame(
            Variable1 = vars[i],
            Variable2 = vars[j],
            Correlation = test_result$estimate,
            P_Value = test_result$p.value,
            N = nrow(valid_data),
            Interpretation = interpret_correlation(test_result$estimate),
            stringsAsFactors = FALSE
          ))
        }, error = function(e) {
          warning(paste("Error calculating correlation between", 
                       vars[i], "and", vars[j], ":", e$message))
        })
      }
    }
    
    return(results)
  }

  # ส่วนเดิม - วิเคราะห์ correlation ทั่วไป
  observeEvent(input$analyze_correlation, {
    data <- data_clean()
    req(data)

    # คำนวณ correlation เดิม
    result <- cor(data$`Cryo Volume (ml/unit)`, 
                 data$`FFP Volume (ml)`, 
                 use = "complete.obs")
    correlation_result(result)

    # คำนวณ correlation ทั้งหมด
    detailed_results(calculate_all_correlations(data))
  })

  # ส่วนเดิม - แสดงผล correlation ทั่วไป
  output$correlation_result <- renderText({
    result <- correlation_result()
    detailed <- detailed_results()
    req(result, detailed)
    
    # สร้างข้อความสรุปผล
    summary_text <- paste("ผลการวิเคราะห์ความสัมพันธ์:\n\n")
    
    # เพิ่มผลการวิเคราะห์แบบละเอียด
    for(i in 1:nrow(detailed)) {
      summary_text <- paste0(
        summary_text,
        "- ", detailed$Variable1[i], " กับ ", detailed$Variable2[i], ":\n",
        "  • ค่าสหสัมพันธ์ = ", round(detailed$Correlation[i], 4), "\n",
        "  • p-value = ", format.pval(detailed$P_Value[i], digits = 4), "\n",
        "  • จำนวนข้อมูล = ", detailed$N[i], "\n",
        "  • การแปลผล: ", detailed$Interpretation[i], "\n\n"
      )
    }
    
    summary_text
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
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = TRUE, color = "blue") +
      geom_hline(yintercept = 140, color = "red", linetype = "dashed") +
      labs(
        title = "ความสัมพันธ์ระหว่าง Cryo Volume (10-15 ml) และ Fibrinogen",
        subtitle = "เส้นประแดงแสดงค่ามาตรฐาน Fibrinogen (140 mg)",
        x = "Cryo Volume (ml/unit)",
        y = "Fibrinogen (mg)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
  })

  # Return invisible reactive values for testing
  return(invisible(list(
    correlation_result = correlation_result,
    detailed_results = detailed_results,
    filtered_data = filtered_data
  )))
}
