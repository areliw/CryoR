# modules/data_loading.R

load_data_module <- function(sheet_url) {
  data <- read_sheet(sheet_url, sheet = "CryoR")
  data$time_interval <- cut(
    as.numeric(data$`Cryo Storage Duration (Days)`),
    breaks = c(0, 1, 15, 30, 90, 180, Inf),
    labels = c("1 Day", "2-15 days", "16-30 days", "1-3 months", "3-6 months", "6+ months"),
    right = FALSE
  )
  data <- na.omit(data)
  setDT(data)  # แปลงเป็น data.table
  return(data)
}
