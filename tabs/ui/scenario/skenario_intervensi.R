intScenario <- tabPanel(
  title = "Skenario Intervensi", 
  value = "intScenario", 
  hr(),
  actionButton("showInterventionTable", "Tampilkan"),
  tabBox(id="tabPanelIntervensi", width = 12,
    tabPanel("Energi & Transportasi", tags$br(), buttonUI("forEnergy")),
    tabPanel("Limbah", tags$br(), buttonUI("forWaste")),
    tabPanel("Pertanian", tags$br(), buttonUI("forAgri")),
    tabPanel("Lahan", tags$br(), buttonUI("forLand"))
  )
                   
)