ui <- fluidPage(
  titlePanel("Shiny App for Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("load_data", "โหลดข้อมูลจาก Google Sheets"),
      h4("Normality Test"),
      actionButton("test_normality", "ทดสอบการกระจายแบบปกติ (Shapiro-Wilk Test)"),
      textOutput("normality_result"),
      h4("Correlation Analysis"),
      actionButton("analyze_correlation", "วิเคราะห์ความสัมพันธ์ (เลือกแบบอัตโนมัติ)"),
      
      h4("Power Analysis"),
      numericInput("effect_size", "Effect Size:", value = 0.5, min = 0),
      numericInput("significance_level", "Significance Level (Alpha):", value = 0.05, min = 0, max = 1, step = 0.01),
      numericInput("power", "Power (1 - Beta):", value = 0.8, min = 0, max = 1, step = 0.01),
      actionButton("power_analysis", "วิเคราะห์ Power"),
      textOutput("power_result"),
      
      h4("Time Interval Histogram"),
      actionButton("plot_histogram", "แสดง Histogram สำหรับ Time Intervals")
    ),
    
    mainPanel(
      h4("สถิติเชิงพรรณนา"),
      tableOutput("result_table"),
      
      h4("การกระจายตัว (Scatter Plot)"),
      plotOutput("scatter_plot"),
      
      h4("ผลการวิเคราะห์ความสัมพันธ์"),
      textOutput("correlation_result"),
      
      h4("Histogram ของ Time Intervals"),
      plotOutput("histogram_plot")
    )
  )
)
