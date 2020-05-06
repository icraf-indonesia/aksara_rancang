bauScenario <- tabPanel(title = "Skenario BAU", 
                              value = "bauScenario",
                              # br(), hr(),
                              tabBox(id="tabPanelBAU", width = 12, 
                                     tabPanel("Proyeksi", id="tab1",
                                              column(width = 12,
                                                            column(width = 2,
                                                                   pickerInput(inputId = "partido_ano",
                                                                               label = "Tipe Proyeksi",
                                                                               choices = c("Proyeksi BAU berdasarkan pertumbuhan ekonomi", "Proyeksi BAU berdasarkan perubahan tutupan lahan"),
                                                                               selected = NULL,
                                                                               options = list(`live-search` = TRUE))
                                                            ),
                                                            column(width = 1, style = "padding-top: 57px;",
                                                                   actionBttn(inputId = "runButton3",
                                                                              label = "Jalankan",
                                                                              style = "fill",
                                                                              color = "success",
                                                                              icon = icon("check"), size = "sm")
                                                            )
                                                     ),
                                              tags$div(id="placeholderTable3")
                                              ),
                                     tabPanel("Rate Table", id="tab2",
                                              column(width = 12,
                                                            column(width = 2,
                                                                   pickerInput(inputId = "partido_ano",
                                                                               label = "Tipe Intervensi",
                                                                               choices = c("Tipe 1", "Tipe 2"),
                                                                               selected = NULL,
                                                                               options = list(`live-search` = TRUE))
                                                            ),
                                                            column(width = 2,
                                                                   pickerInput(inputId = "dateFrom",
                                                                               label = "Tahnun Awal",
                                                                               choices = 2010:2030,
                                                                               selected = 2015,
                                                                               options = list(`live-search` = TRUE))
                                                            ),
                                                            column(width = 2,
                                                                   pickerInput(inputId = "dateTo",
                                                                               label = "Tahun Akhir",
                                                                               choices = 2010:2030,
                                                                               selected = 2030,
                                                                               options = list(`live-search` = TRUE))
                                                            ),
                                                            column(width = 2, style = "padding-top: 57px;",
                                                                   actionBttn(inputId = "runButton1",
                                                                              label = "Buat Tabel",
                                                                              style = "fill",
                                                                              color = "success",
                                                                              icon = icon("check"), size = "sm")
                                                                   )
                                                     ),
                                              tags$div(id="placeholderTable")
                                              ),
                                     tabPanel("Hasil Analisis", id="tab3", 
                                              column(width = 12,
                                                     column(width = 2,
                                                            pickerInput(inputId = "bauResults",
                                                                        label = "Output yang Ditampilkan",
                                                                        choices = c("Proyeksi PDRB", "Proyeksi Upah per Kapita", "Proyeksi Upah Gaji", "Proyeksi Tenaga Kerja",
                                                                                    "Proyeksi Konsumsi Energi", "Proyeksi Emisi Terkait Konsumsi Energi", "Proyeksi Buangan Limbah",
                                                                                    "Proyeksi Emisi Terkait Buangan Limbah", "Proyeksi Total Emisi", "Proyeksi Intensitas Emisi"),
                                                                        selected = "Proyeksi PDRB")
                                                     ),
                                                     column(width = 1, style = "padding-top: 57px;",
                                                            actionBttn(inputId = "runButton2",
                                                                       label = "Jalankan",
                                                                       style = "fill",
                                                                       color = "success",
                                                                       icon = icon("check"), size = "sm")
                                                     )
                                              ),
                                              tags$div(id="placeholderTable2")
                                     )
                              )
                              ### Output ####
                              # conditionalPanel(condition = "input.partidos_gerar_visualizacoes > 0",
                              #                  column(width = 12,
                              #                         withSpinner(rHandsontableOutput("testEditTableBAU"), type = 6),
                              #                         style = "padding-bottom: 10px;")
                              #                  # column(width = 12,
                              #                  #        withSpinner(dataTableOutput("testTableBAU"), type = 6),
                              #                  #        style = "padding-bottom: 10px;"
                              #                  # )
                              # ),
                              # conditionalPanel(condition = "input.partidos_gerar_visualizacoes > 0 ",
                              #                  actionBttn(inputId = "saveBttn-bau",
                              #                             label = "Simpan",
                              #                             style = "fill",
                              #                             color = "success",
                              #                             icon = icon("check"), size = "sm")
                              # ),
                              # conditionalPanel(condition = "input.partidos_gerar_visualizacoes2 > 0",
                              #                  HTML("<center><h1>PLOT</h1></center>"),
                              #                  column(width = 12,
                              #                         withSpinner(plotlyOutput("plotBAU"), type = 6)
                              #                  )           
                              # ),
                              # conditionalPanel(condition = "input.partidos_gerar_visualizacoes2 > 0",
                              #                  HTML("<center><h1>TABLE</h1></center>"),
                              #                  column(width = 12,
                              #                         withSpinner(dataTableOutput("testTableBAU"), type = 6)
                              #                  )           
                              # )
)