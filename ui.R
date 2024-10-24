# ui.R
ui <- dashboardPage(
  dashboardHeader(title = "วิเคราะห์ข้อมูลทางคลินิก"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("ข้อมูล", tabName = "data", icon = icon("database")),
      menuItem("การวิเคราะห์", tabName = "analysis", icon = icon("chart-line")),
      menuItem("กราฟ", tabName = "plots", icon = icon("chart-bar")),
      menuItem("สรุป", tabName = "summary", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .content-wrapper { overflow: auto; }
        .box { margin-bottom: 15px; }
        .shiny-notification { position: fixed; top: 50%; left: 50%; }
      "))
    ),
    
    tabItems(
      # แท็บข้อมูล
      tabItem(tabName = "data",
        fluidRow(
          safe_box(
            title = "โหลดและวิเคราะห์ข้อมูล",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            actionButton("load_data", "โหลดข้อมูลจาก Google Sheets", 
                        class = "btn-primary",
                        icon = icon("cloud-download")),
            DTOutput("data_preview")
          )
        )
      ),
      
      # แท็บวิเคราะห์
      tabItem(tabName = "analysis",
        fluidRow(
          safe_box(
            title = "การทดสอบการแจกแจงแบบปกติ",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            actionButton("test_normality", "ทดสอบการกระจายแบบปกติ", 
                        class = "btn-info"),
            verbatimTextOutput("normality_result")
          ),
          
          safe_box(
            title = "การวิเคราะห์สหสัมพันธ์",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            actionButton("analyze_correlation", "วิเคราะห์ความสัมพันธ์", 
                        class = "btn-info"),
            verbatimTextOutput("correlation_result")
          )
        ),
        
        fluidRow(
          safe_box(
            title = "Power Analysis",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            numericInput("effect_size", "Effect Size:", 
                        value = 0.5, min = 0),
            numericInput("significance_level", "ระดับนัยสำคัญ (Alpha):", 
                        value = 0.05, min = 0, max = 1, step = 0.01),
            numericInput("power", "Power (1 - Beta):", 
                        value = 0.8, min = 0, max = 1, step = 0.01),
            actionButton("power_analysis", "วิเคราะห์ Power", 
                        class = "btn-success"),
            verbatimTextOutput("power_result")
          )
        )
      ),
      
      # แท็บกราฟ
      tabItem(tabName = "plots",
        fluidRow(
          safe_box(
            title = "การแสดงผลกราฟ",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            tabsetPanel(
              tabPanel("Q-Q Plot", plotOutput("qq_plot")),
              tabPanel("Histogram", 
                      actionButton("plot_histogram", "แสดง Histogram", 
                                  class = "btn-warning"),
                      plotOutput("histogram_plot")),
              tabPanel("Scatter Plot", plotOutput("scatter_plot")),
              tabPanel("Cryo-Fibrinogen", plotOutput("cryo_fibrinogen_plot"))
            )
          )
        )
      ),
      
      # แท็บสรุป
      tabItem(tabName = "summary",
        fluidRow(
          safe_box(
            title = "ตารางสรุป",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            actionButton("show_summary_table", "แสดงตารางสรุป", 
                        class = "btn-secondary"),
            DTOutput("summary_table")
          )
        )
      )
    )
  )
)
