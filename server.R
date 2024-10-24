# server.R
library(shiny)
library(shinyjs)
library(googlesheets4)
library(DT)
library(shinydashboard)
library(shinyBS)

server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    data = NULL,
    error = NULL,
    loading = FALSE
  )

  # Data loading
  observeEvent(input$load_data, {
    rv$loading <- TRUE
    showModal(modalDialog("กำลังโหลดข้อมูล...", footer = NULL))

    withProgress(
      message = 'กำลังโหลดข้อมูล...',
      value = 0,
      {
        tryCatch({
          sheet_url <- "https://docs.google.com/spreadsheets/d/13oJHWG48vVJrh5WwkFzI0T0j9niPxvx3Gz4rwFX8js4/edit?gid=1940769691#gid=1940769691"  # ใส่ URL ของ Google Sheets ที่คุณให้มา
          data <- load_data_module(sheet_url)
          rv$data <- data
          rv$loading <- FALSE
          removeModal()
          showNotification("โหลดข้อมูลสำเร็จ!", type = "message")
        }, error = function(e) {
          rv$error <- e$message
          rv$loading <- FALSE
          removeModal()
          showNotification(
            paste("เกิดข้อผิดพลาด:", e$message), 
            type = "error"
          )
        })
      }
    )
  })

  # แสดงข้อมูลที่โหลดมาในตาราง
  output$data_preview <- renderDT({
    req(rv$data)
    datatable(rv$data, options = list(pageLength = 5))
  })

  # แสดงข้อมูลทั้งหมด
  output$full_data_table <- renderDT({
    req(rv$data)
    datatable(rv$data, options = list(pageLength = 10, scrollX = TRUE))
  })

  # Enable/disable UI elements
  observe({
    if (is.null(rv$data)) {
      disable("test_normality")
      disable("analyze_correlation")
      disable("power_analysis")
      disable("plot_histogram")
      disable("show_summary_table")
    } else {
      enable("test_normality")
      enable("analyze_correlation")
      enable("power_analysis")
      enable("plot_histogram")
      enable("show_summary_table")
    }
  })

  # Call modules
  descriptive_stats_module(input, output, reactive(rv$data))
  normality_test_module(input, output, reactive(rv$data))
  correlation_analysis_module(input, output, reactive(rv$data))
  power_analysis_module(input, output)
  plots_module(input, output, reactive(rv$data))
  summary_table_module(input, output, reactive(rv$data))

  # ปรับปรุงการแจ้งเตือนข้อผิดพลาด
  observeEvent(rv$error, {
    if (!is.null(rv$error)) {
      showNotification(rv$error, type = "error", duration = 5)
      rv$error <- NULL
    }
  })
}
