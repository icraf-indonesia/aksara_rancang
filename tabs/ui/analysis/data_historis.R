eleicoes_brasil <- tabPanel(title = "Data Historis", 
                            value = "brasil",
                            # br(), hr(),
                            column(width = 2,
                                   pickerInput(inputId = "categoryProvince", 
                                               label = "Provinsi", 
                                               choices = c("Aceh", "Bali", "Bangka Belitung", "Banten", "Bengkulu", "Daerah Istimewa Yogyakarta", "DKI Jakarta", "Gorontalo", "Jambi", "Jawa Barat", "Jawa Tengah",
                                                           "Jawa Timur", "Kalimantan Barat","Kalimantan Selatan", "Kalimantah Tengah", "Kalimantan Timur", "Kalimantan Utara", "Kepulauan Riau", "Lampung", "Maluku", 
                                                           "Maluku Utara", "Nusa Tenggara Barat", "Nusa Tenggara Timur", "Papua", "Papua Barat", "Riau", "Sulawesi Barat", "Sulawesi Selatan", "Sulawesi Tengah", 
                                                           "Sulawesi Tenggara", "Sulawesi Utara", "Sumatera Barat", "Sumatera Selatan", "Sumatera Utara"), 
                                               selected = NULL, 
                                               options = NULL)
                            ),
                            column(width = 2, style = "padding-top: 57px;",
                                   actionBttn(inputId = "eleicoes_gerar_visualizacoes_br", 
                                              label = "Jalankan", 
                                              style = "fill", 
                                              color = "success", 
                                              icon = icon("check"), size = "sm") 
                            ),
                            ##-- Outputs ----
                            conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_br > 0",
                                             tabBox(id="tabPanelHistori", width = 12, 
                                                    tabPanel("Tabel Input-Output", id="boxIO",
                                                             div(style="overflow-x: scroll; padding-top: 20px;", dataTableOutput('tableIO'))
                                                    ),
                                                    tabPanel("Tabel Tenaga Kerja", id="boxLabour", 
                                                             div(style="overflow-x: scroll; padding-top: 20px;", dataTableOutput('SatelitTenagaKerja'))
                                                    ))
                            )
                                             
)