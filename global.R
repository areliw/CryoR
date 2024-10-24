# global.R
library(shiny)
library(shinydashboard)
library(googlesheets4)
library(ggplot2)
library(data.table)
library(pwr)
library(Rcpp)
library(shinyjs)
library(DT)

# Initialize error handling
options(shiny.error = function() {
  stop.function <- function() {
    tags$div(
      class = "shiny-output-error-validation",
      "เกิดข้อผิดพลาด กรุณาลองใหม่อีกครั้งหรือติดต่อผู้ดูแลระบบ"
    )
  }
  stop.function
})

ensure_plot_device <- function() {
  if (!dev.cur()) {
    pdf(NULL)
  }
}

safe_box <- function(...) {
  tryCatch({
    ensure_plot_device()
    shinydashboard::box(...)
  }, error = function(e) {
    div(
      class = "alert alert-warning",
      "ไม่สามารถแสดงผลได้ กรุณารีเฟรชหน้าเว็บ"
    )
  })
}

# ใช้การเข้าถึงแบบไม่ต้องลงชื่อเข้าใช้
gs4_deauth()

# ลิงก์ของ Google Sheets
sheet_url <- "https://docs.google.com/spreadsheets/d/13oJHWG48vVJrh5WwkFzI0T0j9niPxvx3Gz4rwFX8js4/edit#gid=1940769691"

# C++ function สำหรับการคำนวณสถิติ
cppFunction('
NumericVector compute_stats(NumericVector x) {
  int n = x.size();
  double sum = 0, sum_sq = 0, min = x[0], max = x[0];
  for(int i = 0; i < n; ++i) {
    double val = x[i];
    sum += val;
    sum_sq += val * val;
    if(val < min) min = val;
    if(val > max) max = val;
  }
  double mean = sum / n;
  double variance = (sum_sq / n) - (mean * mean);
  double sd = sqrt(variance);
  NumericVector result = NumericVector::create(mean, sd, min, max);
  return result;
}
')
