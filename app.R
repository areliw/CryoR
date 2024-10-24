# app.R
source("global.R")

# อัพเดท initialize_app จากเวอร์ชันเดิม
initialize_app <- function() {
  # ตรวจสอบระบบและติดตั้งแพ็คเกจ
  check_system_requirements()
  check_and_install_packages()
  
  # ตั้งค่าระบบ
  setup_error_handling()
  setup_caching()
  
  # โหลดไฟล์ที่จำเป็น
  load_required_files()
  
  # สร้างและคืนค่า Shiny app
  shinyApp(ui = ui, server = server)
}
