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

#############################
observeEvent(input$runButton1, {
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

# output$defUIManual<- renderUI({
#   tagLis(rHandsontableOutput(ns('editDefine')),
#   )
# })