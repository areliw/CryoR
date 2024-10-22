ui <- fluidPage(
  titlePanel("Shiny App for Data Analysis"),
  
  sidebarLayout(
    # Sidebar Panel
    sidebarPanel(
      h3("Load and Analyze Data"),
      actionButton("load_data", "โหลดข้อมูลจาก Google Sheets", class = "btn-primary"),
      br(), br(),
      
      h3("Normality Test"),
      actionButton("test_normality", "ทดสอบการกระจายแบบปกติ (Shapiro-Wilk Test)", 
                   class = "btn-info"),
      textOutput("normality_result"),
      br(),
      
      h3("Correlation Analysis"),
      actionButton("analyze_correlation", "วิเคราะห์ความสัมพันธ์", 
                   class = "btn-info"),
      textOutput("correlation_result"),
      br(), br(),
      
      h3("Power Analysis"),
      numericInput("effect_size", "Effect Size:", 
                   value = 0.5, min = 0),
      numericInput("significance_level", "Significance Level (Alpha):", 
                   value = 0.05, min = 0, max = 1, step = 0.01),
      numericInput("power", "Power (1 - Beta):", 
                   value = 0.8, min = 0, max = 1, step = 0.01),
      actionButton("power_analysis", "วิเคราะห์ Power", 
                   class = "btn-success"),
      textOutput("power_result"),
      br(),
      
      h3("Time Interval Histogram"),
      actionButton("plot_histogram", "แสดง Histogram สำหรับ Time Intervals", 
                   class = "btn-warning"),
      br(), br(),
      
      h3("Summary Table"),
      actionButton("show_summary_table", "แสดงตารางสรุป", 
                   class = "btn-secondary")
    ),
    
    # Main Panel
    mainPanel(
      tabsetPanel(
        # แท็บเดิม
        tabPanel("สถิติเชิงพรรณนา",
                 tableOutput("result_table")
        ),
        tabPanel("Q-Q Plot",
                 plotOutput("qq_plot")
        ),
        tabPanel("Histogram",
                 plotOutput("histogram_plot")
        ),
        tabPanel("Scatter Plot",
                 plotOutput("scatter_plot")
        ),
        tabPanel("Summary Table",
                 tableOutput("summary_table")
        ),
        
        # แท็บใหม่สำหรับ Cryo-Fibrinogen Analysis
        tabPanel("Cryo-Fibrinogen Analysis",
                 fluidRow(
                   column(12,
                          box(width = 12,
                              title = "การวิเคราะห์ Cryo Volume (10-15 ml) กับ Fibrinogen",
                              status = "primary",
                              solidHeader = TRUE,
                              
                              # ตารางสรุปผล
                              h4("ตารางสรุปผลการวิเคราะห์"),
                              tableOutput("cryo_fibrinogen_table"),
                              
                              # กราฟแสดงความสัมพันธ์
                              hr(),
                              h4("กราฟแสดงความสัมพันธ์"),
                              plotOutput("cryo_fibrinogen_plot", height = "500px")
                          )
                   )
                 )
        )
      )
    )
  )
)
