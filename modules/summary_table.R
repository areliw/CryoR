# modules/summary_table.R

summary_table_module <- function(input, output, data_clean) {
  # Helper function สำหรับการจัดรูปแบบตัวเลข
  format_numbers <- function(x, digits = 2) {
    if (is.numeric(x)) {
      return(round(x, digits))
    }
    return(x)
  }

  # Helper function สำหรับการคำนวณ percentile
  calculate_percentiles <- function(x, probs = c(0.25, 0.5, 0.75)) {
    if (length(x[!is.na(x)]) == 0) return(rep(NA, length(probs)))
    quantile(x, probs = probs, na.rm = TRUE)
  }

  # Helper function สำหรับการตรวจสอบข้อมูลนอกเกณฑ์
  check_outliers <- function(x) {
    if (length(x[!is.na(x)]) < 4) return(rep(NA, 2))
    q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    lower <- q[1] - 1.5 * iqr
    upper <- q[2] + 1.5 * iqr
    c(sum(x < lower, na.rm = TRUE), sum(x > upper, na.rm = TRUE))
  }

  # ฟังก์ชันหลักสำหรับการคำนวณสถิติสรุป
  calculate_detailed_summary <- function(data) {
    tryCatch({
      req(data)
      if (nrow(data) == 0) {
        return(NULL)
      }

      # กำหนดตัวแปรที่ต้องการวิเคราะห์
      analysis_vars <- c(
        "Cryo Volume (ml/unit)",
        "FFP Volume (ml)",
        "Fibrinogen (mg)"
      )

      # คำนวณสถิติสำหรับแต่ละช่วงเวลา
      summary_stats <- data[, {
        # รายการสถิติที่จะคำนวณสำหรับแต่ละตัวแปร
        var_stats <- lapply(analysis_vars, function(var) {
          x <- get(var)
          q <- calculate_percentiles(x)
          outliers <- check_outliers(x)
          
          list(
            # ข้อมูลทั่วไป
            N = sum(!is.na(x)),
            Missing = sum(is.na(x)),
            
            # สถิติพื้นฐาน
            Mean = mean(x, na.rm = TRUE),
            SD = sd(x, na.rm = TRUE),
            Median = q[2],
            Q1 = q[1],
            Q3 = q[3],
            Min = min(x, na.rm = TRUE),
            Max = max(x, na.rm = TRUE),
            
            # ข้อมูลเพิ่มเติม
            CV = sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100,
            Lower_outliers = outliers[1],
            Upper_outliers = outliers[2]
          )
        })
        names(var_stats) <- analysis_vars
        
        # สร้างรายการผลลัพธ์
        list(
          # ข้อมูล Cryo Volume
          "จำนวนตัวอย่าง" = var_stats[["Cryo Volume (ml/unit)"]]$N,
          "Missing Cryo" = var_stats[["Cryo Volume (ml/unit)"]]$Missing,
          "Cryo Volume เฉลี่ย" = var_stats[["Cryo Volume (ml/unit)"]]$Mean,
          "SD Cryo Volume" = var_stats[["Cryo Volume (ml/unit)"]]$SD,
          "CV% Cryo Volume" = var_stats[["Cryo Volume (ml/unit)"]]$CV,
          "Median Cryo Volume" = var_stats[["Cryo Volume (ml/unit)"]]$Median,
          "Q1 Cryo Volume" = var_stats[["Cryo Volume (ml/unit)"]]$Q1,
          "Q3 Cryo Volume" = var_stats[["Cryo Volume (ml/unit)"]]$Q3,
          "Min Cryo Volume" = var_stats[["Cryo Volume (ml/unit)"]]$Min,
          "Max Cryo Volume" = var_stats[["Cryo Volume (ml/unit)"]]$Max,
          "Outliers Cryo (ต่ำ/สูง)" = paste(
            var_stats[["Cryo Volume (ml/unit)"]]$Lower_outliers,
            var_stats[["Cryo Volume (ml/unit)"]]$Upper_outliers,
            sep = "/"
          ),

          # ข้อมูล FFP Volume
          "FFP Volume เฉลี่ย" = var_stats[["FFP Volume (ml)"]]$Mean,
          "SD FFP Volume" = var_stats[["FFP Volume (ml)"]]$SD,
          "CV% FFP Volume" = var_stats[["FFP Volume (ml)"]]$CV,
          "Median FFP Volume" = var_stats[["FFP Volume (ml)"]]$Median,
          
          # ข้อมูล Fibrinogen
          "Fibrinogen เฉลี่ย" = var_stats[["Fibrinogen (mg)"]]$Mean,
          "SD Fibrinogen" = var_stats[["Fibrinogen (mg)"]]$SD,
          "CV% Fibrinogen" = var_stats[["Fibrinogen (mg)"]]$CV,
          "Median Fibrinogen" = var_stats[["Fibrinogen (mg)"]]$Median,
          
          # การผ่านเกณฑ์ Fibrinogen
          "จำนวนที่ผ่านเกณฑ์ Fibrinogen" = sum(`Fibrinogen (mg)` >= 140, na.rm = TRUE),
          "ร้อยละที่ผ่านเกณฑ์" = 100 * mean(`Fibrinogen (mg)` >= 140, na.rm = TRUE)
        )
      }, by = time_interval]

      return(summary_stats)
    }, error = function(e) {
      warning(paste("Error in calculate_detailed_summary:", e$message))
      return(NULL)
    })
  }

  # Event handler สำหรับปุ่มแสดงตารางสรุป
  observeEvent(input$show_summary_table, {
    data <- data_clean()
    req(data)
    
    # คำนวณสถิติสรุป
    summary_data <- calculate_detailed_summary(data)
    
    if (is.null(summary_data)) {
      showNotification("ไม่สามารถคำนวณข้อมูลสรุปได้", type = "error")
      return(NULL)
    }
    
    # จัดรูปแบบตัวเลข
    numeric_cols <- names(summary_data)[sapply(summary_data, is.numeric)]
    summary_data[, (numeric_cols) := lapply(.SD, format_numbers), .SDcols = numeric_cols]
    
    # แสดงตารางผ่าน DT
    output$summary_table <- renderDT({
      datatable(
        summary_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE,
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center; color: black; font-size: 16px;',
          'ตารางสรุปผลการวิเคราะห์แยกตามช่วงเวลา'
        ),
        filter = 'top'
      ) %>%
        formatStyle(
          'ร้อยละที่ผ่านเกณฑ์',
          backgroundColor = styleInterval(
            c(50, 80),
            c('pink', 'yellow', 'lightgreen')
          )
        ) %>%
        formatStyle(
          names(summary_data),
          backgroundColor = styleEqual(
            c(NA, NaN, Inf, -Inf),
            rep('pink', 4)
          )
        )
    })
  })
}
