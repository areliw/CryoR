# modules/data_loading.R

library(memoise)

# สร้างฟังก์ชัน memoised สำหรับ read_sheet
memoised_read_sheet <- memoise::memoise(read_sheet)

load_data_module <- function(sheet_url) {
  data <- memoised_read_sheet(sheet_url, sheet = "CryoR", col_types = "c")
  setDT(data)  # แปลงเป็น data.table

  # กำหนดคอลัมน์ที่ต้องการแปลงเป็นตัวเลข
  num_cols <- c("Cryo Storage Duration (Days)", "FFP Volume (ml)", "Cryo Volume (ml/unit)", "Fibrinogen (mg)")
  
  # แปลงคอลัมน์เป็นตัวเลขภายใน data.table
  data[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

  # สร้าง time_interval ภายใน data.table
  data[, time_interval := cut(
    `Cryo Storage Duration (Days)`,
    breaks = c(0, 1, 15, 30, 90, 180, Inf),
    labels = c("1 Day", "2-15 days", "16-30 days", "1-3 months", "3-6 months", "6+ months"),
    right = FALSE
  )]

  data <- na.omit(data)
  return(data)
}
