# modules/power_analysis.R
power_analysis_module <- function(input, output) {
  # Helper functions for power analysis
  calculate_sample_size <- function(effect_size, sig_level, power, type = "two.sample") {
    tryCatch({
      validate(
        need(effect_size > 0, "Effect size ต้องมากกว่า 0"),
        need(sig_level > 0 && sig_level < 1, "Significance level ต้องอยู่ระหว่าง 0 และ 1"),
        need(power > 0 && power < 1, "Power ต้องอยู่ระหว่าง 0 และ 1")
      )
      
      result <- pwr::pwr.t.test(
        d = effect_size,
        sig.level = sig_level,
        power = power,
        type = type
      )
      
      list(
        success = TRUE,
        n = ceiling(result$n),
        actual_power = result$power,
        effect_size = result$d
      )
    }, error = function(e) {
      list(
        success = FALSE,
        message = paste("เกิดข้อผิดพลาด:", e$message)
      )
    })
  }
  
  # Power analysis event handler
  observeEvent(input$power_analysis, {
    result <- calculate_sample_size(
      effect_size = input$effect_size,
      sig_level = input$significance_level,
      power = input$power
    )
    
    output$power_result <- renderText({
      if (result$success) {
        paste0(
          "ผลการวิเคราะห์ Power:\n\n",
          "• ขนาดตัวอย่างที่ต้องการต่อกลุ่ม: ", result$n, " ราย\n",
          "• ขนาดตัวอย่างรวม: ", result$n * 2, " ราย\n",
          "• Power ที่แท้จริง: ", round(result$actual_power * 100, 2), "%\n",
          "• Effect size: ", round(result$effect_size, 3), "\n\n",
          "หมายเหตุ: คำนวณสำหรับการเปรียบเทียบค่าเฉลี่ย 2 กลุ่ม (Independent t-test)"
        )
      } else {
        paste("เกิดข้อผิดพลาด:", result$message)
      }
    })
  })
}
