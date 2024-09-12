library(googlesheets4)
library(ggplot2)
library(shiny)
library(pwr)

# ใช้การเข้าถึงแบบไม่ต้องลงชื่อเข้าใช้ (Deauthorize)
gs4_deauth()

# ลิงก์ของ Google Sheets ที่คุณต้องการดึงข้อมูล
sheet_url <- "https://docs.google.com/spreadsheets/d/13oJHWG48vVJrh5WwkFzI0T0j9niPxvx3Gz4rwFX8js4/edit?gid=1940769691#gid=1940769691"
