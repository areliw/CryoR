# modules/normality_test.R

normality_test_module <- function(input, output, data_clean) {
    observeEvent(input$test_normality, {
        data <- data_clean()
        req(data)

        # แปลงคอลัมน์เป็นตัวเลขและลบค่า NA
        test_data <- as.numeric(data$`Cryo Volume (ml/unit)`)
        test_data <- test_data[!is.na(test_data)]

        # ตรวจสอบว่ามีข้อมูลเพียงพอหรือไม่
        if (length(test_data) < 3) {
            output$normality_result <- renderText("ไม่สามารถทำการทดสอบได้เนื่องจากจำนวนข้อมูลน้อยเกินไป")
        } else {
            test_result <- shapiro.test(test_data)
            output$normality_result <- renderText({
                paste("Shapiro-Wilk Test: W =", round(test_result$statistic, 4), 
                      "p-value =", format.pval(test_result$p.value, digits = 4))
            })
        }
    })
}
