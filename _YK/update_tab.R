output$testEditTableBAU <- renderRHandsontable({
  testEditTableBAU <- read_excel("data/test.xlsx")
  rhandsontable(testEditTableBAU)
})

output$plotBAU <- renderPlotly({
  plotBAU<- plot_ly(economics, x = ~date, y = ~pop)
  plotBAU
})

output$testTableBAU1 <- renderDataTable({
  testTableBAU2 <- read_excel("data/test.xlsx")
  testTableBAU2 
})

output$testTableBAU2 <- renderDataTable({
  testTableBAU <- read_excel("data/test.xlsx")
  testTableBAU 
})

#############################
observeEvent(input$runButton1, {
  removeUI("#testTableBAU1")
  removeUI("#saveBttn-bau1")
  insertUI(selector='#placeholderTable1',
           where='afterEnd',
           ui= tagList(dataTableOutput("testTableBAU1"),
                       actionBttn(inputId = "saveBttn-bau1",
                                  label = "Simpan",
                                  style = "fill",
                                  color = "success",
                                  icon = icon("check"), size = "sm"))
  )
})

observeEvent(input$runButton2, {
  removeUI("#testEditTableBAU")
  removeUI("#saveBttn-bau2")
  insertUI(selector='#placeholderTable2',
           where='afterEnd',
           ui= tagList(rHandsontableOutput("testEditTableBAU"),
                       actionBttn(inputId = "saveBttn-bau2",
                                  label = "Simpan",
                                  style = "fill",
                                  color = "success",
                                  icon = icon("check"), size = "sm"))
  )
})

observeEvent(input$runButton3, {
  removeUI("#analysisTitle")
  removeUI("#plotBAU")
  removeUI("#testTableBAU2")
  removeUI("#saveBttn-bau3")
  insertUI(selector='#placeholderTable3',
           where='afterEnd',
           ui= tagList(h3("Plot", id="analysisTitle"),
                       plotlyOutput("plotBAU"),
                       dataTableOutput("testTableBAU2"),
                       actionBttn(inputId = "saveBttn-bau3",
                                  label = "Simpan",
                                  style = "fill",
                                  color = "success",
                                  icon = icon("check"), size = "sm"))
  )
})
