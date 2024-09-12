library(shiny)
source("ui.R")     # โหลดไฟล์ UI
source("server.R") # โหลดไฟล์ Server

# รันแอป Shiny
shinyApp(ui = ui, server = server)
