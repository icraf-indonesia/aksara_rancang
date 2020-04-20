intScenario <- tabPanel(title = "Skenario Intervensi", 
                   value = "intScenario",
                   tabBox(id="tabPanelIntervensi", width = 12, 
                          tabPanel("Sektor Energi & Transportasi", id="energi_transport_int",
                                   div(style="overflow-x: scroll; padding-top: 20px;", dataTableOutput('tabelEnergiFinal')),
                                   br(),
                                   # uiOutput("test")
                                   actionBttn(inputId = "defineScenarioEnergi", 
                                                        label = "Deskripsi Skenario", 
                                                        style = "fill", 
                                                        color = "success", 
                                                        # icon = icon("data"), 
                                                        size = "sm")
                          ),
                          tabPanel("Sektor Lahan", id="lahan_int",
                                   br(),
                                   # tableOutput("perfil_candidato_gerar_visualizacoes")
                                   actionBttn(inputId = "defineScenarioLahan",
                                              label = "Deskripsi Skenario",
                                              style = "fill",
                                              color = "success",
                                              # icon = icon("data"),
                                              size = "sm")
                          ),
                          tabPanel("Sektor Limbah", id="limbah_int",
                                   br(),
                                   actionBttn(inputId = "defineScenarioLimbah", 
                                              label = "Deskripsi Skenario", 
                                              style = "fill", 
                                              color = "success", 
                                              # icon = icon("data"), 
                                              size = "sm")
                          ),
                          tabPanel("Sektor Pertanian", id="pertanian_int",
                                   br(),
                                   actionBttn(inputId = "defineScenarioPertanian", 
                                              label = "Deskripsi Skenario", 
                                              style = "fill", 
                                              color = "success", 
                                              # icon = icon("data"), 
                                              size = "sm")
                          ))
                   ##-- Outputs ----
)