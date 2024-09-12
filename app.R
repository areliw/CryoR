# โหลดแพ็กเกจที่จำเป็น
library(shiny)
library(googlesheets4)
library(ggplot2)
library(pwr)

# ใช้การเข้าถึงแบบไม่ต้องลงชื่อเข้าใช้ (Deauthorize)
gs4_deauth()

# ลิงก์ของ Google Sheets ที่คุณต้องการดึงข้อมูล (ตั้งค่าสิทธิ์เป็น "Anyone with the link")
sheet_url <- "https://docs.google.com/spreadsheets/d/13oJHWG48vVJrh5WwkFzI0T0j9niPxvx3Gz4rwFX8js4/edit?gid=1940769691#gid=1940769691"

# ส่วนของ UI
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

# ส่วนของ Server
server <- function(input, output) {
  # ดึงข้อมูลจาก Google Sheets เมื่อกดปุ่ม
  data_clean <- eventReactive(input$load_data, {
    tryCatch({
      data <- read_sheet(sheet_url, sheet = "CryoR")  # ดึงข้อมูลจากแผ่น CryoR
      # สร้างคอลัมน์ time_interval ที่แยกช่วงเวลา
      data$time_interval <- cut(
        as.numeric(data$`Cryo Storage Duration (Days)`),
        breaks = c(0, 1, 15, 30, 90, 180, Inf),
        labels = c("1 Days","2-15 days", "16-30 days", "1-3 months", "3-6 months", "6+ months")
      )
      na.omit(data)  # ลบข้อมูลที่มีค่า NA ออก
    }, error = function(e) {
      showNotification("Error loading Google Sheets data.", type = "error")
      return(NULL)
    })
  })
  
  # แสดงตารางสถิติเชิงพรรณนา
  output$result_table <- renderTable({
    data <- data_clean()
    if (is.null(data)) return(NULL)
    
    mean_ffp <- mean(data$`FFP Volume (ml)`, na.rm = TRUE)
    median_ffp <- median(data$`FFP Volume (ml)`, na.rm = TRUE)
    sd_ffp <- sd(data$`FFP Volume (ml)`, na.rm = TRUE)
    min_ffp <- min(data$`FFP Volume (ml)`, na.rm = TRUE)
    max_ffp <- max(data$`FFP Volume (ml)`, na.rm = TRUE)
    
    mean_cryo <- mean(data$`Cryo Volume (ml/unit)`, na.rm = TRUE)
    median_cryo <- median(data$`Cryo Volume (ml/unit)`, na.rm = TRUE)
    sd_cryo <- sd(data$`Cryo Volume (ml/unit)`, na.rm = TRUE)
    min_cryo <- min(data$`Cryo Volume (ml/unit)`, na.rm = TRUE)
    max_cryo <- max(data$`Cryo Volume (ml/unit)`, na.rm = TRUE)
    
    mean_fibrinogen <- mean(data$`Fibrinogen (mg)`, na.rm = TRUE)
    median_fibrinogen <- median(data$`Fibrinogen (mg)`, na.rm = TRUE)
    sd_fibrinogen <- sd(data$`Fibrinogen (mg)`, na.rm = TRUE)
    min_fibrinogen <- min(data$`Fibrinogen (mg)`, na.rm = TRUE)
    max_fibrinogen <- max(data$`Fibrinogen (mg)`, na.rm = TRUE)
    
    result_table <- data.frame(
      Variable = c('FFP Volume (ml)', 'Cryo Volume (ml)', 'Fibrinogen (mg)'),
      Mean = c(mean_ffp, mean_cryo, mean_fibrinogen),
      Median = c(median_ffp, median_cryo, median_fibrinogen),
      SD = c(sd_ffp, sd_cryo, sd_fibrinogen),
      Min = c(min_ffp, min_cryo, min_fibrinogen),
      Max = c(max_ffp, max_cryo, max_fibrinogen)
    )
    
    return(result_table)
  })
  
  # ทดสอบการกระจายแบบปกติ (Shapiro-Wilk Test)
  output$normality_result <- renderText({
    req(input$test_normality)
    data <- data_clean()
    if (is.null(data)) return(NULL)
    
    shapiro_test_ffp <- shapiro.test(data$`FFP Volume (ml)`)
    shapiro_test_cryo <- shapiro.test(data$`Cryo Volume (ml/unit)`)
    
    if (shapiro_test_ffp$p.value > 0.05 && shapiro_test_cryo$p.value > 0.05) {
      return("Both FFP Volume and Cryo Volume are normally distributed (p > 0.05)")
    } else {
      return("At least one variable is not normally distributed (p <= 0.05)")
    }
  })
  
  # แสดงการกระจายตัว (Scatter Plot)
  output$scatter_plot <- renderPlot({
    data <- data_clean()
    if (is.null(data)) return(NULL)
    
    ggplot(data, aes(x = `FFP Volume (ml)`, y = `Cryo Volume (ml/unit)`)) +
      geom_point(color = "blue", size = 3) +
      labs(title = "Scatter Plot: FFP Volume vs Cryo Volume",
           x = "FFP Volume (ml)", y = "Cryo Volume (ml/unit)") +
      theme_minimal()
  })
  
  # วิเคราะห์ความสัมพันธ์
  output$correlation_result <- renderText({
    req(input$analyze_correlation)
    data <- data_clean()
    if (is.null(data)) return(NULL)
    
    shapiro_test_ffp <- shapiro.test(data$`FFP Volume (ml)`)
    shapiro_test_cryo <- shapiro.test(data$`Cryo Volume (ml/unit)`)
    
    method <- ifelse(shapiro_test_ffp$p.value > 0.05 && shapiro_test_cryo$p.value > 0.05, "pearson", "spearman")
    correlation_result <- cor(data$`FFP Volume (ml)`, data$`Cryo Volume (ml/unit)`, method = method, use = "complete.obs")
    
    paste("Correlation (", method, ") between FFP Volume and Cryo Volume: ", round(correlation_result, 3))
  })
  
  # Power Analysis (Sample Size Estimation) - Updated for user input
  output$power_result <- renderText({
    req(input$power_analysis)
    data <- data_clean()
    if (is.null(data)) return(NULL)
    
    effect_size <- input$effect_size
    alpha <- input$significance_level
    power <- input$power
    
    # Conduct power analysis for t-test with user input
    power_analysis_result <- pwr.t.test(d = effect_size, sig.level = alpha, power = power, type = "two.sample")
    
    paste("Required sample size for power analysis (effect size = ", effect_size, "): ", round(power_analysis_result$n, 2))
  })
  
  # แสดง Histogram สำหรับ Time Intervals
  output$histogram_plot <- renderPlot({
    req(input$plot_histogram)
    data <- data_clean()
    if (is.null(data)) return(NULL)
    
    ggplot(data, aes(x = `Cryo Volume (ml/unit)`)) +
      geom_histogram(binwidth = 1, fill = "blue", color = "black") +
      facet_wrap(~ time_interval) +
      labs(title = "Distribution of Cryo Volume by Time Interval",
           x = "Cryo Volume (ml/unit)",         y = "Frequency") +
      theme_minimal()
  })
}

# รันแอป Shiny
shinyApp(ui = ui, server = server)

           