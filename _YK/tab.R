library(shiny)
library(DT)
shinyApp(
  
  ui = fluidPage(
    
    sidebarLayout(
      
      sidebarPanel(
        numericInput(
          inputId = "random_val",
          label = "pick random value",
          value = 1
        )
      ),
      
      mainPanel(
        tabsetPanel(
          id = "tabset",
          selected = "test_render",
          tabPanel(
            title = "some_other_tab",
            "Some other stuff"
          ),
          tabPanel(
            title = "test_render",
            textOutput("echo_test"),
            DTOutput("dt_test")
          )
        )
      )
    )
  ),
  
  server = function(input, output, session) {
    
    output$echo_test <- renderText({
      cat("renderText called \n")
      input$random_val
    })
    outputOptions(output, "echo_test", suspendWhenHidden = FALSE)
    output$dt_test <- renderDT({
      cat("renderDT called \n")
      df <- data.frame(
        a = 1:10^6,
        b = rep(1, 10^6)
      )
      datatable(df)
    }, server = TRUE)
    observeEvent(input$random_val, {
      df <- data.frame(
        a = 1:10^6,
        b = rep(input$random_val, 10^6)
      )
      dt_test_proxy <- dataTableProxy("dt_test", session = shiny::getDefaultReactiveDomain(),
                                      deferUntilFlush = TRUE)
      replaceData(dt_test_proxy, df)
      cat("table updated \n")
    })
    updateTabsetPanel(session, "tabset", selected = "some_other_tab")
  }
)