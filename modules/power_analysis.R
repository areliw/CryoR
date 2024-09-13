# modules/power_analysis.R

power_analysis_module <- function(input, output) {
  observeEvent(input$power_analysis, {
    tryCatch({
      power_result <- pwr::pwr.t.test(d = input$effect_size, 
                                      sig.level = input$significance_level, 
                                      power = input$power, 
                                      type = "two.sample")
      output$power_result <- renderText({
        paste("Sample Size needed:", ceiling(power_result$n))
      })
    }, error = function(e) {
      output$power_result <- renderText("Error in power analysis: ตรวจสอบอินพุตของคุณ")
    })
  })
}
