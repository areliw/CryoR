# ui.R

ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = "วิเคราะห์ข้อมูลทางคลินิก",
    titleWidth = 350
  ),

  # Sidebar
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      id = "sidebar",
      # ข้อมูล
      menuItem(
        text = "ข้อมูล",
        tabName = "data",
        icon = icon("database"),
        startExpanded = TRUE,
        menuSubItem(
          text = "โหลดข้อมูล",
          tabName = "load_data",
          icon = icon("cloud-download")
        ),
        menuSubItem(
          text = "ดูข้อมูล",
          tabName = "view_data",
          icon = icon("table")
        )
      ),
      
      # การวิเคราะห์
      menuItem(
        text = "การวิเคราะห์",
        tabName = "analysis",
        icon = icon("chart-line"),
        startExpanded = TRUE,
        menuSubItem(
          text = "สถิติเชิงพรรณนา",
          tabName = "descriptive",
          icon = icon("calculator")
        ),
        menuSubItem(
          text = "การทดสอบการแจกแจง",
          tabName = "normality",
          icon = icon("chart-bar")
        ),
        menuSubItem(
          text = "การวิเคราะห์สหสัมพันธ์",
          tabName = "correlation",
          icon = icon("project-diagram")
        ),
        menuSubItem(
          text = "Power Analysis",
          tabName = "power",
          icon = icon("tachometer-alt")
        )
      ),
      
      # กราฟ
      menuItem(
        text = "การแสดงผลกราฟ",
        tabName = "plots",
        icon = icon("chart-area"),
        startExpanded = TRUE,
        menuSubItem(
          text = "Q-Q Plot",
          tabName = "qq_plot",
          icon = icon("chart-line")
        ),
        menuSubItem(
          text = "Histogram",
          tabName = "histogram",
          icon = icon("chart-bar")
        ),
        menuSubItem(
          text = "Scatter Plot",
          tabName = "scatter",
          icon = icon("chart-scatter")
        )
      ),
      
      # สรุปผล
      menuItem(
        text = "สรุปผล",
        tabName = "summary",
        icon = icon("file-alt")
      )
    )
  ),
  
  # Body
  dashboardBody(
    # Load custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$style(HTML("
        .content-wrapper { padding-top: 20px; }
        .box { margin-bottom: 20px; }
        .loading-spinner { margin: 20px; }
      "))
    ),
    
    useShinyjs(),
    
    # Custom notifications
    div(id = "notification-area", 
        style = "position: fixed; top: 20px; right: 20px; z-index: 9999;"),
    
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
            div(
              id = "loading-spinner",
              class = "loading-spinner",
              style = "display: none;",
              tags$i(class = "fa fa-spinner fa-spin fa-3x")
            ),
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
            numericInput(
              "significance_level",
              "ระดับนัยสำคัญ (Alpha):",
              value = 0.05,
              min = 0,
              max = 1,
              step = 0.01
            ),
            numericInput(
              "power",
              "Power (1 - Beta):",
              value = 0.8,
              min = 0,
              max = 1,
              step = 0.01
            ),
            actionButton(
              "power_analysis",
              "วิเคราะห์ Power",
              icon = icon("calculator"),
              class = "btn-success"
            ),
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
