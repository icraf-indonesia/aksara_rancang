output$testEditTableBAU <- renderRHandsontable({
  testEditTableBAU <- read_excel("data/test.xlsx")
  rhandsontable(testEditTableBAU)
})


output$plotBAU <- renderPlotly({
  plotBAU<- plot_ly(economics, x = ~date, y = ~pop)
  plotBAU
})

output$testTableBAU <- renderDataTable({
  testTableBAU <- read_excel("data/test.xlsx")
  testTableBAU 
})

output$testTableBAU2 <- renderDataTable({
  testTableBAU2 <- read_excel("data/test.xlsx")
  testTableBAU2 
})

#############################
observeEvent(input$runButton1, {
  removeUI("tab2")
  removeUI("tab3")
  insertUI(selector='#placeholderTable',
           where='afterEnd',
           ui= tagList(rHandsontableOutput("testEditTableBAU"),
                       actionBttn(inputId = "saveBttn-bau1",
                                  label = "Simpan",
                                  style = "fill",
                                  color = "success",
                                  icon = icon("check"), size = "sm"))
  )
})

observeEvent(input$runButton2, {
  removeUI("tab1")
  removeUI("tab3")
  insertUI(selector='#placeholderTable2',
           where='afterEnd',
           ui= tagList(plotlyOutput("plotBAU"),
                       dataTableOutput("testTableBAU"),
                       actionBttn(inputId = "saveBttn-bau2",
                                  label = "Simpan",
                                  style = "fill",
                                  color = "success",
                                  icon = icon("check"), size = "sm"))
  )
})

observeEvent(input$runButton3, {
  removeUI("tab1")
  removeUI("tab2")
  insertUI(selector='#placeholderTable3',
           where='afterEnd',
           ui= tagList(dataTableOutput("testTableBAU2"),
                       actionBttn(inputId = "saveBttn-bau3",
                                  label = "Simpan",
                                  style = "fill",
                                  color = "success",
                                  icon = icon("check"), size = "sm"))
  )
})
# output$defUIManual<- renderUI({
#   tagLis(rHandsontableOutput(ns('editDefine')),
#   )
# })