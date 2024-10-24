# modules/data_loading.R
library(memoise)
library(data.table)
library(googlesheets4)

# สร้างฟังก์ชัน memoised สำหรับ read_sheet
memoised_read_sheet <- memoise::memoise(read_sheet)

# ฟังก์ชันสำหรับตรวจสอบคุณภาพข้อมูล
validate_data <- function(data) {
  validation_results <- list(
    valid = TRUE,
    messages = character(0)
  )
  
  # ตรวจสอบว่ามีข้อมูลหรือไม่
  if (nrow(data) == 0) {
    validation_results$valid <- FALSE
    validation_results$messages <- c(validation_results$messages, 
                                   "ไม่พบข้อมูลในชีท")
  }
  
  # ตรวจสอบคอลัมน์ที่จำเป็น
  required_cols <- c("Cryo Storage Duration (Days)", 
                    "FFP Volume (ml)", 
                    "Cryo Volume (ml/unit)", 
                    "Fibrinogen (mg)")
  
  missing_cols <- required_cols[!required_cols %in% names(data)]
  if (length(missing_cols) > 0) {
    validation_results$valid <- FALSE
    validation_results$messages <- c(validation_results$messages,
                                   paste("ไม่พบคอลัมน์:", 
                                         paste(missing_cols, collapse = ", ")))
  }
  
  return(validation_results)
}

load_data_module <- function(sheet_url) {
  tryCatch({
    # โหลดข้อมูลจากชีตชื่อ "CryoR"
    data <- memoised_read_sheet(sheet_url, sheet = "CryoR", col_types = "c")
    
    # ตรวจสอบข้อมูล
    validation <- validate_data(data)
    if (!validation$valid) {
      stop(paste(validation$messages, collapse = "\n"))
    }
    
    setDT(data)  # แปลงเป็น data.table
    
    # กำหนดคอลัมน์ที่ต้องการแปลงเป็นตัวเลข
    num_cols <- c("Cryo Storage Duration (Days)", 
                  "FFP Volume (ml)", 
                  "Cryo Volume (ml/unit)", 
                  "Fibrinogen (mg)")
    
    # แปลงคอลัมน์เป็นตัวเลขภายใน data.table
    data[, (num_cols) := lapply(.SD, function(x) {
      as.numeric(gsub("[^0-9.-]", "", as.character(x)))
    }), .SDcols = num_cols]
    
    # ตรวจสอบและจัดการค่า NA
    na_counts <- sapply(data[, ..num_cols], function(x) sum(is.na(x)))
    if (any(na_counts > 0)) {
      warning(paste("พบค่า NA ในคอลัมน์:", 
                   paste(names(na_counts[na_counts > 0]), 
                         collapse = ", ")))
    }
    
    # ลบแถวที่มีค่า NA ใน Cryo Storage Duration
    original_rows <- nrow(data)
    data <- data[!is.na(`Cryo Storage Duration (Days)`)]
    rows_removed <- original_rows - nrow(data)
    if (rows_removed > 0) {
      warning(paste("ลบ", rows_removed, 
                   "แถวที่มีค่า NA ใน Cryo Storage Duration"))
    }
    
    # สร้าง time_interval
    data[, time_interval := cut(
      `Cryo Storage Duration (Days)`,
      breaks = c(-Inf, 1, 15, 30, 90, 180, Inf),
      labels = c("1 Day", "2-15 days", "16-30 days", 
                "1-3 months", "3-6 months", "6+ months"),
      right = FALSE
    )]
    
    # ตรวจสอบค่าที่ไม่ถูกต้อง
    data[, `:=`(
      valid_cryo = between(`Cryo Volume (ml/unit)`, 0, 100),
      valid_ffp = between(`FFP Volume (ml)`, 0, 1000),
      valid_fibrinogen = between(`Fibrinogen (mg)`, 0, 1000)
    )]
    
    invalid_rows <- data[!valid_cryo | !valid_ffp | !valid_fibrinogen, .N]
    if (invalid_rows > 0) {
      warning(paste("พบ", invalid_rows, "แถวที่มีค่าไม่อยู่ในช่วงที่กำหนด"))
    }
    
    # สร้างรายงานสรุป
    summary_stats <- data[, .(
      total_rows = .N,
      missing_cryo = sum(is.na(`Cryo Volume (ml/unit)`)),
      missing_ffp = sum(is.na(`FFP Volume (ml)`)),
      missing_fibrinogen = sum(is.na(`Fibrinogen (mg)`)),
      invalid_values = invalid_rows
    )]
    
    attr(data, "summary_stats") <- summary_stats
    
    return(data)
    
  }, error = function(e) {
    stop(paste("เกิดข้อผิดพลาดในการโหลดข้อมูล:", e$message))
  }, warning = function(w) {
    warning(paste("คำเตือน:", w$message))
  })
}
