ui <- fluidPage(
  titlePanel("Shiny App for Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Load and Analyze Data"),
      actionButton("load_data", "โหลดข้อมูลจาก Google Sheets", class = "btn-primary"),
      br(), br(),
      
      h3("Normality Test"),
      actionButton("test_normality", "ทดสอบการกระจายแบบปกติ (Shapiro-Wilk Test)", class = "btn-info"),
      textOutput("normality_result"),
      br(),
      
      h3("Correlation Analysis"),
      actionButton("analyze_correlation", "วิเคราะห์ความสัมพันธ์ (เลือกแบบอัตโนมัติ)", class = "btn-info"),
      br(), br(),
      
      h3("Power Analysis"),
      numericInput("effect_size", "Effect Size:", value = 0.5, min = 0),
      numericInput("significance_level", "Significance Level (Alpha):", value = 0.05, min = 0, max = 1, step = 0.01),
      numericInput("power", "Power (1 - Beta):", value = 0.8, min = 0, max = 1, step = 0.01),
      actionButton("power_analysis", "วิเคราะห์ Power", class = "btn-success"),
      textOutput("power_result"),
      br(),
      
      h3("Time Interval Histogram"),
      actionButton("plot_histogram", "แสดง Histogram สำหรับ Time Intervals", class = "btn-warning")
    ),
    
    mainPanel(
      fluidRow(
        column(6,
               h3("สถิติเชิงพรรณนา"),
               tableOutput("result_table")
        ),
        column(6,
               h3("Q-Q Plot for Cryo Volume"),
               plotOutput("qq_plot")
        )
      ),
      
      fluidRow(
        column(6,
               h3("ผลการวิเคราะห์ความสัมพันธ์"),
               textOutput("correlation_result")
        ),
        column(6,
               h3("Histogram ของ Time Intervals"),
               plotOutput("histogram_plot")
        )
      ),
      
      fluidRow(
        column(12,
               h3("การกระจายตัว (Scatter Plot)"),
               plotOutput("scatter_plot")
        )
      )
    )
  )
)
