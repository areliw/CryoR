# server.R

# โหลดโมดูล
source("modules/data_loading.R")
source("modules/descriptive_stats.R")
source("modules/normality_test.R")
source("modules/correlation_analysis.R")
source("modules/power_analysis.R")
source("modules/plots.R")
source("modules/summary_table.R")  # โหลดโมดูลเพิ่มเติม

server <- function(input, output) {
  # ดึงข้อมูลจาก Google Sheets เมื่อกดปุ่ม
  data_clean <- eventReactive(input$load_data, {
    tryCatch({
      load_data_module(sheet_url)
    }, error = function(e) {
      showNotification("Error loading Google Sheets data.", type = "error")
      return(NULL)
    })
  })
  
  # เรียกใช้โมดูลต่าง ๆ พร้อมการตรวจสอบข้อมูล
  descriptive_stats_module(input, output, data_clean)
  normality_test_module(input, output, data_clean)
  correlation_analysis_module(input, output, data_clean)
  power_analysis_module(input, output)
  plots_module(input, output, data_clean)
  summary_table_module(input, output, data_clean)  # เรียกใช้โมดูลตารางสรุป
}

