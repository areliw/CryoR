# modules/summary_table.R

summary_table_module <- function(input, output, data_clean) {
  observeEvent(input$show_summary_table, {
    data <- data_clean()
    req(data)
    
    # แปลงคอลัมน์เป็นตัวเลขและลบค่า NA
    data <- data[!is.na(data$`Cryo Volume (ml/unit)`), ]
    data$`Cryo Volume (ml/unit)` <- as.numeric(data$`Cryo Volume (ml/unit)`)
    
    # สร้างตารางสรุปจำนวน Cryo Units ในแต่ละระดับของ Cryo Volume แยกตาม Time Interval
    summary_table <- data[, .N, by = .(`Cryo Volume (ml/unit)`, time_interval)]
    setnames(summary_table, "N", "Count")
    
    # จัดเรียงตารางสรุป
    summary_table <- summary_table[order(time_interval, `Cryo Volume (ml/unit)`)]
    
    # แสดงตาราง
    output$summary_table <- renderTable({
      summary_table
    })
  })
}
