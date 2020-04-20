updateTabsetPanel(session, 'partidos_gerar_visualizacoes2', 'tab3')

output$testEditTableBAU <- renderRHandsontable({
  testEditTableBAU <- read_excel("data/test.xlsx")
  rhandsontable(testEditTableBAU)
})

output$testTableBAU <- renderDataTable({
  testTableBAU <- read_excel("data/test.xlsx")
  testTableBAU
})

plotBAU<- plot_ly(economics, x = ~date, y = ~pop)

output$plotBAU <- renderPlotly({
  plotBAU
})