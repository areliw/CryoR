library(shiny)
source("ui.R")     # โหลดไฟล์ UI
source("server.R") # โหลดไฟล์ Server
source("global.R") # โหลดค่าตัวแปรและแพ็กเกจร่วม

# รันแอป Shiny
shinyApp(ui = ui, server = server)
