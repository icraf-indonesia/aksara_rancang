# updateTabsetPanel(session, 'partidos_gerar_visualizacoes2', 'tab3')

output$testEditTableBAU <- renderRHandsontable({
  testEditTableBAU <- read_excel("data/test.xlsx")
  rhandsontable(testEditTableBAU)
})

observeEvent(input$partidos_gerar_visualizacoes2, {
  output$testTableBAU <- renderDataTable({
    testTableBAU <- read_excel("data/test.xlsx")
    testTableBAU
  })
  
  output$plotBAU <- renderPlotly({
    plotBAU<- plot_ly(economics, x = ~date, y = ~pop)
    plotBAU
  })
  updateTabItems(session, "tabPanelBAU", selected = "tab3")
  removeUI('#tab2')
})

  
  
  
