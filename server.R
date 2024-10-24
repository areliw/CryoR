# server.R
server <- function(input, output, session) {
  rv <- reactiveValues(
    data = NULL,
    error = NULL,
    loading = FALSE
  )
  
  # Data loading
  observeEvent(input$load_data, {
    rv$loading <- TRUE
    withProgress(
      message = 'กำลังโหลดข้อมูล...',
      value = 0,
      {
        tryCatch({
          data <- load_data_module(sheet_url)
          rv$data <- data
          rv$loading <- FALSE
          showNotification("โหลดข้อมูลสำเร็จ!", type = "success")
          
          # Preview data
          output$data_preview <- renderDT({
            datatable(rv$data,
                     options = list(pageLength = 10,
                                  scrollX = TRUE),
                     rownames = FALSE)
          })
          
        }, error = function(e) {
          rv$error <- e$message
          rv$loading <- FALSE
          showNotification(paste("เกิดข้อผิดพลาด:", e$message), type = "error")
        })
      }
    )
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
}
