intScenario <- tabPanel(title = "Skenario Intervensi", 
                        value = "intScenario", 
                   tabBox(id="tabPanelIntervensi", width = 12,
                          tabPanel("Energi & Transportasi",
                                   tags$br(),
                                   buttonUI("forEnergy")
                                   ),
                          tabPanel("Limbah",
                                   tags$br(),
                                   buttonUI("forWaste") 
                                   ),
                          tabPanel("Pertanian",
                                   tags$br(),
                                   buttonUI("forAgri")
                                   ),
                          tabPanel("Lahan",
                                   tags$br(),
                                   buttonUI("forLand")
                                   )
                          )
)