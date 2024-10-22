# modules/summary_table.R

summary_table_module <- function(input, output, data_clean) {
  observeEvent(input$show_summary_table, {
    data <- data_clean()
    req(data)
    
    if (nrow(data) == 0) {
      showNotification("ไม่มีข้อมูลเพียงพอในการแสดงตารางสรุป", type = "warning")
      return(NULL)
    }
    
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
