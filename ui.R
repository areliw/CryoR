# ui.R
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(shinyBS)  # เพิ่มการเรียกใช้แพ็กเกจนี้

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "แอปพลิเคชันวิเคราะห์ข้อมูลทางคลินิก",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("ข้อมูล", tabName = "data", icon = icon("database"),
        menuSubItem("โหลดข้อมูล", tabName = "load_data", icon = icon("cloud-download")),
        menuSubItem("ดูข้อมูล", tabName = "view_data", icon = icon("table"))
      ),
      menuItem("การวิเคราะห์", tabName = "analysis", icon = icon("chart-line"),
        menuSubItem("สถิติเชิงพรรณนา", tabName = "descriptive", icon = icon("calculator")),
        menuSubItem("การทดสอบการแจกแจง", tabName = "normality", icon = icon("chart-bar")),
        menuSubItem("การวิเคราะห์สหสัมพันธ์", tabName = "correlation", icon = icon("project-diagram")),
        menuSubItem("Power Analysis", tabName = "power", icon = icon("tachometer"))
      ),
      menuItem("กราฟ", tabName = "plots", icon = icon("chart-area"),
        menuSubItem("Q-Q Plot", tabName = "qq_plot", icon = icon("line-chart")),
        menuSubItem("Histogram", tabName = "histogram", icon = icon("bar-chart")),
        menuSubItem("Scatter Plot", tabName = "scatter", icon = icon("chart-scatter"))
      ),
      menuItem("สรุปผล", tabName = "summary", icon = icon("file-alt"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Roboto:300,400,500,700&display=swap")
    ),
    
    tabItems(
      # Tab ข้อมูล
      tabItem(
        tabName = "load_data",
        fluidRow(
          box(
            width = 12,
            title = "โหลดข้อมูลจาก Google Sheets",
            status = "primary",
            solidHeader = TRUE,
            div(
              class = "text-center",
              actionButton(
                "load_data",
                "โหลดข้อมูล",
                icon = icon("cloud-download"),
                class = "btn-primary btn-lg",
                style = "margin: 20px;"
              )
            ),
            bsTooltip("load_data", "คลิกเพื่อโหลดข้อมูลจาก Google Sheets", placement = "right", options = list(container = "body")),
            hr(),
            DTOutput("data_preview")
          )
        )
      ),
      
      # Tab ดูข้อมูล
      tabItem(
        tabName = "view_data",
        fluidRow(
          box(
            width = 12,
            title = "ข้อมูลทั้งหมด",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("full_data_table")
          )
        )
      ),
      
      # Tab สถิติเชิงพรรณนา
      tabItem(
        tabName = "descriptive",
        fluidRow(
          box(
            width = 12,
            title = "สถิติเชิงพรรณนา",
            status = "info",
            solidHeader = TRUE,
            DTOutput("result_table")
          )
        )
      ),
      
      # Tab การทดสอบการแจกแจง
      tabItem(
        tabName = "normality",
        fluidRow(
          box(
            width = 6,
            title = "การทดสอบการแจกแจงแบบปกติ",
            status = "info",
            solidHeader = TRUE,
            actionButton(
              "test_normality",
              "ทดสอบการแจกแจง",
              icon = icon("check"),
              class = "btn-info"
            ),
            bsTooltip("test_normality", "คลิกเพื่อทดสอบการแจกแจงแบบปกติของข้อมูล", placement = "right", options = list(container = "body")),
            hr(),
            verbatimTextOutput("normality_result")
          ),
          box(
            width = 6,
            title = "Q-Q Plot",
            status = "info",
            solidHeader = TRUE,
            plotOutput("qq_plot_normality")
          )
        )
      ),
      
      # Tab การวิเคราะห์สหสัมพันธ์
      tabItem(
        tabName = "correlation",
        fluidRow(
          box(
            width = 6,
            title = "การวิเคราะห์สหสัมพันธ์",
            status = "info",
            solidHeader = TRUE,
            actionButton(
              "analyze_correlation",
              "วิเคราะห์สหสัมพันธ์",
              icon = icon("calculator"),
              class = "btn-info"
            ),
            bsTooltip("analyze_correlation", "คลิกเพื่อวิเคราะห์สหสัมพันธ์ระหว่างตัวแปร", placement = "right", options = list(container = "body")),
            hr(),
            verbatimTextOutput("correlation_result")
          ),
          box(
            width = 6,
            title = "Scatter Plot",
            status = "info",
            solidHeader = TRUE,
            plotOutput("correlation_plot")
          )
        )
      ),
      
      # Tab Power Analysis
      tabItem(
        tabName = "power",
        fluidRow(
          box(
            width = 6,
            title = "Power Analysis",
            status = "success",
            solidHeader = TRUE,
            numericInput(
              "effect_size",
              "Effect Size:",
              value = 0.5,
              min = 0,
              step = 0.1
            ),
            bsTooltip("effect_size", "ใส่ขนาดผลที่คาดหวัง (เช่น 0.5 สำหรับขนาดปานกลาง)", placement = "right", options = list(container = "body")),
            numericInput(
              "significance_level",
              "ระดับนัยสำคัญ (Alpha):",
              value = 0.05,
              min = 0,
              max = 1,
              step = 0.01
            ),
            bsTooltip("significance_level", "ใส่ระดับนัยสำคัญที่ต้องการ (เช่น 0.05)", placement = "right", options = list(container = "body")),
            numericInput(
              "power",
              "Power (1 - Beta):",
              value = 0.8,
              min = 0,
              max = 1,
              step = 0.01
            ),
            bsTooltip("power", "ใส่ค่า Power ที่ต้องการ (เช่น 0.8)", placement = "right", options = list(container = "body")),
            actionButton(
              "power_analysis",
              "วิเคราะห์ Power",
              icon = icon("calculator"),
              class = "btn-success"
            ),
            bsTooltip("power_analysis", "คลิกเพื่อคำนวณขนาดตัวอย่างที่ต้องการ", placement = "right", options = list(container = "body")),
            hr(),
            verbatimTextOutput("power_result")
          ),
          box(
            width = 6,
            title = "คำอธิบาย Power Analysis",
            status = "success",
            solidHeader = TRUE,
            tags$div(
              class = "well",
              tags$h4("คำอธิบาย Power Analysis"),
              tags$p("Power Analysis คือการวิเคราะห์เพื่อกำหนดขนาดตัวอย่างที่เหมาะสม 
                     โดยพิจารณาจากปัจจัยต่างๆ ดังนี้:"),
              tags$ul(
                tags$li(HTML("<strong>Effect Size:</strong> ขนาดของความแตกต่างที่ต้องการตรวจพบ
                            <ul>
                              <li>0.2: ผลขนาดเล็ก</li>
                              <li>0.5: ผลขนาดปานกลาง</li>
                              <li>0.8: ผลขนาดใหญ่</li>
                            </ul>")),
                tags$li(HTML("<strong>Alpha (ระดับนัยสำคัญ):</strong> โอกาสในการเกิด Type I Error 
                            (การปฏิเสธ H0 ทั้งที่ H0 เป็นจริง)")),
                tags$li(HTML("<strong>Power:</strong> โอกาสในการตรวจพบความแตกต่างที่มีอยู่จริง 
                            (1 - โอกาสในการเกิด Type II Error)"))
              ),
              tags$p("ค่าที่นิยมใช้:", 
                    tags$br(),
                    "Alpha = 0.05", 
                    tags$br(),
                    "Power = 0.80")
            )
          )
        )
      ),
      
      # Tab Q-Q Plot
      tabItem(
        tabName = "qq_plot",
        fluidRow(
          box(
            width = 12,
            title = "Q-Q Plot",
            status = "warning",
            solidHeader = TRUE,
            plotOutput("qq_plot", height = "600px")
          )
        )
      ),
      
      # Tab Histogram
      tabItem(
        tabName = "histogram",
        fluidRow(
          box(
            width = 12,
            title = "Histogram",
            status = "warning",
            solidHeader = TRUE,
            actionButton(
              "plot_histogram",
              "แสดง Histogram",
              icon = icon("chart-bar"),
              class = "btn-warning"
            ),
            bsTooltip("plot_histogram", "คลิกเพื่อแสดง Histogram ของ Cryo Volume", placement = "right", options = list(container = "body")),
            hr(),
            plotOutput("histogram_plot", height = "600px")
          )
        )
      ),
      
      # Tab Scatter Plot
      tabItem(
        tabName = "scatter",
        fluidRow(
          box(
            width = 12,
            title = "Scatter Plot",
            status = "warning",
            solidHeader = TRUE,
            plotOutput("scatter_plot", height = "600px")
          )
        )
      ),
      
      # Tab สรุปผล
      tabItem(
        tabName = "summary",
        fluidRow(
          box(
            width = 12,
            title = "ตารางสรุปผลการวิเคราะห์",
            status = "primary",
            solidHeader = TRUE,
            div(
              style = "margin-bottom: 20px;",
              actionButton(
                "show_summary_table",
                "แสดงตารางสรุป",
                icon = icon("table"),
                class = "btn-primary"
              ),
              bsTooltip("show_summary_table", "คลิกเพื่อแสดงตารางสรุปผลการวิเคราะห์", placement = "right", options = list(container = "body")),
              downloadButton(
                "download_summary",
                "ดาวน์โหลดรายงาน",
                class = "btn-success"
              )
            ),
            hr(),
            DTOutput("summary_table")
          )
        )
      )
    )
  )
)
