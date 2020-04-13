eleicoes_uf <- tabPanel(title = "Hasil Analasis", 
                        value = "uf",
                        # br(), hr(),
                        ##-- Botões ----
                        column(width = 10,
                               ##-- + Ano ----
                               column(width = 2,
                                      pickerInput(inputId = "eleicoes_ano_uf", 
                                                  label = "Kategori", 
                                                  choices = c("Ekonomi", "Energi", "Lahan", "Limbah"), 
                                                  selected = NULL, 
                                                  options = list(`live-search` = TRUE))
                               ),
                               ##-- + Cargo ----
                               column(width = 2,
                                      pickerInput(inputId = "eleicoes_cargo_uf", 
                                                  label = "Output yang Ditampilkan", 
                                                  choices = c("PDRB", "Backward Linkage", "Forward Linkage", "Angka Pengganda Pendapatan Rumah Tangga", "Angka Pengganda Tenaga Kerja", "Angka Pengganda Output",
                                                              "Upah gaji", "Rasio Upah gaji per Surplus Usaha", "Pendapatan per kapita", "Perbandingan Angka Pengganda"
                                                  ),
                                                  selected = "PRESIDENT",
                                                  options = list(`live-search` = TRUE))
                               )
                               # ##-- + Turno ----
                               # column(width = 2,
                               #        pickerInput(inputId = "eleicoes_turno_uf", 
                               #                    label = "Round", 
                               #                    choices = c("1º round", "2º round"), 
                               #                    selected = "1º round",
                               #                    options = list(`live-search` = TRUE))
                               # ),
                               # ##-- + Estado ----
                               # column(width = 2,
                               #        pickerInput(inputId = "eleicoes_estado_uf", 
                               #                    label = "State", 
                               #                    choices = levels(factor(x = estados,
                               #                                            levels = estados)), 
                               #                    selected = "AC",
                               #                    options = list(`live-search` = TRUE,
                               #                                   `none-selected-text` = "None selected"))
                               # )
                        ), 
                        ##-- Visualizar ----
                        column(width = 2, style = "padding-top: 50px;",
                               actionBttn(inputId = "eleicoes_gerar_visualizacoes_uf", 
                                          label = "Select", 
                                          style = "fill", 
                                          color = "success", 
                                          icon = icon("check"), size = "sm") 
                        ),
                        # conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_uf < 1",
                        #                  column(width = 12,
                        #                         br(), 
                        #                         wellPanel(
                        #                           HTML("<h1>Elections: State level<h1>")
                        #                         )
                        #                  )
                        # ),
                        ##-- Outputs ----
                        # column(width = 12,
                        #        ##-- + Mapa do Brasil ----
                        #        column(width = 4,
                        #               conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_uf > 0",
                        #                                HTML("<center><h1>TOP VOTED BY STATE</h1></center>"),
                        #                                br(),
                        #                                withSpinner(leafletOutput("mapa_uf_geral_uf"), type = 6)
                        #               )
                        #        ),
                        #        ##-- + Mapa dos municípios ----
                        #        column(width = 4,
                        #               conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_uf > 0",
                        #                                HTML("<center><h1>ELECTION BY CITY</h1></center>"),
                        #                                br(),
                        #                                withSpinner(leafletOutput("mapa_mun_geral_uf"), type = 6)
                        #                                
                        #               )
                        #        ),
                        #        ##-- + Gráfico de barras ----
                        #        column(width = 4,
                        #               conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_uf > 0",
                        #                                HTML("<center><h1>VOTES BARPLOT (%)</h1></center>"),
                        #                                br(),
                        #                                withSpinner(plotlyOutput("barras_geral_uf"), type = 6)
                        #                                
                        #               )
                        #        )
                        # )
)