# global.R

library(googlesheets4)
library(ggplot2)
library(shiny)
library(pwr)
library(data.table)
library(Rcpp)

# ใช้การเข้าถึงแบบไม่ต้องลงชื่อเข้าใช้ (Deauthorize)
gs4_deauth()

# ลิงก์ของ Google Sheets ที่คุณต้องการดึงข้อมูล
sheet_url <- "https://docs.google.com/spreadsheets/d/13oJHWG48vVJrh5WwkFzI0T0j9niPxvx3Gz4rwFX8js4/edit#gid=1940769691"

# สร้างฟังก์ชัน C++ สำหรับการคำนวณสถิติ
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
