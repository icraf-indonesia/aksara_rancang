eleicoes_brasil <- tabPanel(title = "Data Historis", 
                            value = "brasil",
                            # br(), hr(),
                            ##-- Botões ----
                            column(width = 2,
                                   pickerInput(inputId = "eleicoes_ano_br", 
                                               label = "Provinsi", 
                                               choices = c("Aceh", "Bali", "Bangka Belitung", "Banten", "Bengkulu", "Daerah Istimewa Yogyakarta", "DKI Jakarta", "Gorontalo", "Jambi", "Jawa Barat", "Jawa Tengah",
                                                           "Jawa Timur", "Kalimantan Barat","Kalimantan Selatan", "Kalimantah Tengah", "Kalimantan Timur", "Kalimantan Utara", "Kepulauan Riau", "Lampung", "Maluku", 
                                                           "Maluku Utara", "Nusa Tenggara Barat", "Nusa Tenggara Timur", "Papua", "Papua Barat", "Riau", "Sulawesi Barat", "Sulawesi Selatan", "Sulawesi Tengah", 
                                                           "Sulawesi Tenggara", "Sulawesi Utara", "Sumatera Barat", "Sumatera Selatan", "Sumatera Utara"), 
                                               selected = "Jawa Barat", 
                                               options = NULL)
                            ),
                                   # ##-- + Cargo ----
                                   # column(width = 2,
                                   #        pickerInput(inputId = "eleicoes_cargo_br", 
                                   #                    label = "Position", 
                                   #                    choices = list("PRESIDENT" = 1), 
                                   #                    selected = 1,
                                   #                    options = list(`live-search` = TRUE))
                                   # ),
                                   ##-- + Turno ----
                                   # column(width = 2,
                                   #        pickerInput(inputId = "eleicoes_turno_br", ##Ada di server skenario intervensi
                                   #                    label = "Lainnya", 
                                   #                    choices = c("1º round", "2º round"), 
                                   #                    selected = "1º round",
                                   #                    options = list(`live-search` = TRUE))
                                   # )
                                   # ##-- + Estado ----
                                   # column(width = 2,
                                   #        pickerInput(inputId = "eleicoes_estado_br", 
                                   #                    label = "State", 
                                   #                    choices = levels(factor(x = sort(unique(chaves$UF)),
                                   #                                            levels = sort(unique(chaves$UF)))), 
                                   #                    selected = "AC",
                                   #                    options = list(`live-search` = TRUE,
                                   #                                   `none-selected-text` = "None selected"))
                                   # )
                            ##-- Visualizar ----
                            column(width = 2, style = "padding-top: 57px;",
                                   actionBttn(inputId = "eleicoes_gerar_visualizacoes_br", 
                                              label = "Jalankan", 
                                              style = "fill", 
                                              color = "success", 
                                              icon = icon("check"), size = "sm") 
                            ),
                            tabBox(id="tabPanelHistori", width = 12, 
                                   tabPanel("Table Input-Output", id="boxIO",
                                            div(style="overflow-x: scroll", dataTableOutput('tableIO'))
                                   ),
                                   tabPanel("Tabel Tenaga Kerja", id="boxLabour", 
                                            div(style="overflow-x: scroll", dataTableOutput('SatelitTenagaKerja'))
                                   )),
                            # conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_br < 1",
                            #                  column(width = 12,
                            #                         br(), 
                            #                         wellPanel(
                            #                           HTML("<h1>Elections: Federal level<h1>"),
                            #                           HTML("<h4>In this tab, the user can explore the President candidate who received the votes majority in each state/city. 
                            #                                It is interesting to see each party hot areas.
                            #                                .<h4>")
                            #                         )
                            #                  )
                            # ),
                            ##-- Outputs ----
                            # column(width = 12,
                            #        ##-- + Mapa do Brasil ----
                            #        column(width = 4,
                            #               conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_br > 0",
                            #                                HTML("<center><h1>ELECTION BY STATE</h1></center>"),
                            #                                br(),
                            #                                withSpinner(leafletOutput("mapa_uf_geral_br"), type = 6)
                            #               )
                            #        ),
                            #        ##-- + Mapa dos municípios ----
                            #        column(width = 4,
                            #               conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_br > 0",
                            #                                HTML("<center><h1>ELECTION BY CITY</h1></center>"),
                            #                                br(),
                            #                                withSpinner(leafletOutput("mapa_mun_geral_br"), type = 6)
                            #                                
                            #               )
                            #        ),
                            #        ##-- + Gráfico de barras ----
                            #        column(width = 4,
                            #               conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_br > 0",
                            #                                HTML("<center><h1>VOTES BARPLOT (%)</h1></center>"),
                            #                                br(),
                            #                                withSpinner(plotlyOutput("barras_geral_br"), type = 6)
                            #                                
                            #               )
                            #        )
                            # )
)