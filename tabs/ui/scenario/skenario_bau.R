perfil_eleitorado <- tabPanel(title = "Skenario BAU", 
                              value = "candidatos_perfil_eleitorado",
                              # br(), hr(),
                              tabBox(id="tabPanelBAU", width = 12, 
                                     tabPanel("Proyeksi", id="proyeksiBAU",
                                              column(width = 12,
                                                            column(width = 2,
                                                                   pickerInput(inputId = "partido_ano",
                                                                               label = "Tipe Proyeksi",
                                                                               choices = c("Proyeksi BAU berdasarkan pertumbuhan ekonomi", "Proyeksi BAU berdasarkan perubahan tutupan lahan"),
                                                                               selected = NULL,
                                                                               options = list(`live-search` = TRUE))
                                                            ),
                                                            column(width = 1, style = "padding-top: 57px;",
                                                                   actionBttn(inputId = "partidos_gerar_visualizacoes1",
                                                                              label = "Jalankan",
                                                                              style = "fill",
                                                                              color = "success",
                                                                              icon = icon("check"), size = "sm")
                                                            )
                                              )
                                     ),
                                     tabPanel("Rate Table", id="ratetableBAU",
                                              column(width = 12,
                                                            column(width = 2,
                                                                   pickerInput(inputId = "partido_ano",
                                                                               label = "Tipe Intervensi",
                                                                               choices = c("Tipe 1", "Tipe 2"),
                                                                               selected = NULL,
                                                                               options = list(`live-search` = TRUE))
                                                            ),
                                                            column(width = 2,
                                                                   pickerInput(inputId = "partido_cargo",
                                                                               label = "Tahnun Awal",
                                                                               choices = 2010:2030,
                                                                               selected = 2015,
                                                                               options = list(`live-search` = TRUE))
                                                            ),
                                                            column(width = 2,
                                                                   pickerInput(inputId = "partido_cargo",
                                                                               label = "Tahnun Akhir",
                                                                               choices = 2010:2030,
                                                                               selected = 2030,
                                                                               options = list(`live-search` = TRUE))
                                                            ),
                                                            column(width = 2, style = "padding-top: 57px;",
                                                                   actionBttn(inputId = "partidos_gerar_visualizacoes",
                                                                              label = "Buat Tabel",
                                                                              style = "fill",
                                                                              color = "success",
                                                                              icon = icon("check"), size = "sm")
                                                            )
                                                     )
                                              ),
                                     tabPanel("Hasil Analisis", id="analisisBAU", 
                                              column(width = 12,
                                                     column(width = 2,
                                                            pickerInput(inputId = "partido_ano",
                                                                        label = "Output yang Ditampilkan",
                                                                        choices = c("Proyeksi PDRB", "Proyeksi Upah per Kapita", "Proyeksi Upah Gaji", "Proyeksi Tenaga Kerja",
                                                                                    "Proyeksi Konsumsi Energi", "Proyeksi Emisi Terkait Konsumsi Energi", "Proyeksi Buangan Limbah",
                                                                                    "Proyeksi Emisi Terkait Buangan Limbah", "Proyeksi Total Emisi", "Proyeksi Intensitas Emisi"),
                                                                        selected = "Proyeksi PDRB",
                                                                        options = list(`live-search` = TRUE))
                                                     ),
                                                     column(width = 1, style = "padding-top: 57px;",
                                                            actionBttn(inputId = "partidos_gerar_visualizacoes2",
                                                                       label = "Jalankan",
                                                                       style = "fill",
                                                                       color = "success",
                                                                       icon = icon("check"), size = "sm")
                                                     )
                                              )
                                     )
                              ),
                              ### Output ####
                              conditionalPanel(condition = "input.partidos_gerar_visualizacoes > 0",
                                               column(width = 12,
                                                      withSpinner(rHandsontableOutput("testEditTableBAU"), type = 6),
                                                      style = "padding-bottom: 10px;")
                                               # column(width = 12,
                                               #        withSpinner(dataTableOutput("testTableBAU"), type = 6),
                                               #        style = "padding-bottom: 10px;"
                                               # )
                              ),
                              conditionalPanel(condition = "input.partidos_gerar_visualizacoes > 0 ",
                                               actionBttn(inputId = "saveBttn-bau",
                                                          label = "Simpan",
                                                          style = "fill",
                                                          color = "success",
                                                          icon = icon("check"), size = "sm")
                              ),
                              conditionalPanel(condition = "input.partidos_gerar_visualizacoes2 > 0",
                                               HTML("<center><h1>PLOT</h1></center>"),
                                               column(width = 12,
                                                      withSpinner(plotlyOutput("plotBAU"), type = 6)
                                               )           
                              ),
                              conditionalPanel(condition = "input.partidos_gerar_visualizacoes2 > 0",
                                               HTML("<center><h1>TABLE</h1></center>"),
                                               column(width = 12,
                                                      withSpinner(dataTableOutput("testTableBAU"), type = 6)
                                               )           
                              )
                              # column(width = 12,
                              #        column(width = 2,
                              #               pickerInput(inputId = "partido_ano",
                              #                           label = "Tipe Proyeksi",
                              #                           choices = c("Proyeksi BAU berdasarkan pertumbuhan ekonomi", "Proyeksi BAU berdasarkan perubahan tutupan lahan"),
                              #                           selected = NULL,
                              #                           options = list(`live-search` = TRUE))
                              #        ),
                              #        column(width = 1, style = "padding-top: 57px;",
                              #               actionBttn(inputId = "partidos_gerar_visualizacoes1",
                              #                          label = "Jalankan",
                              #                          style = "fill",
                              #                          color = "success",
                              #                          icon = icon("check"), size = "sm")
                              #        )
                              # ),
                              # column(width = 12,
                              #        column(width = 2,
                              #               pickerInput(inputId = "partido_ano",
                              #                           label = "Tipe Intervensi",
                              #                           choices = c("Tipe 1", "Tipe 2"),
                              #                           selected = NULL,
                              #                           options = list(`live-search` = TRUE))
                              #        ),
                              #        column(width = 2,
                              #               pickerInput(inputId = "partido_cargo",
                              #                           label = "Tahnun Awal",
                              #                           choices = 2010:2030,
                              #                           selected = 2015,
                              #                           options = list(`live-search` = TRUE))
                              #        ),
                              #        column(width = 2,
                              #               pickerInput(inputId = "partido_cargo",
                              #                           label = "Tahnun Akhir",
                              #                           choices = 2010:2030,
                              #                           selected = 2030,
                              #                           options = list(`live-search` = TRUE))
                              #        ),
                              #        column(width = 2, style = "padding-top: 57px;",
                              #               actionBttn(inputId = "partidos_gerar_visualizacoes",
                              #                          label = "Buat Tabel",
                              #                          style = "fill",
                              #                          color = "success",
                              #                          icon = icon("check"), size = "sm")
                              #        )
                              # ),
                              # column(width = 12,
                              #        column(width = 2,
                              #               pickerInput(inputId = "partido_ano",
                              #                           label = "Output yang Ditampilkan",
                              #                           choices = c("Proyeksi PDRB", "Proyeksi Upah per Kapita", "Proyeksi Upah Gaji", "Proyeksi Tenaga Kerja",
                              #                                       "Proyeksi Konsumsi Energi", "Proyeksi Emisi Terkait Konsumsi Energi", "Proyeksi Buangan Limbah",
                              #                                       "Proyeksi Emisi Terkait Buangan Limbah", "Proyeksi Total Emisi", "Proyeksi Intensitas Emisi"),
                              #                           selected = "Proyeksi PDRB",
                              #                           options = list(`live-search` = TRUE))
                              #        ),
                              #        column(width = 1, style = "padding-top: 57px;",
                              #               actionBttn(inputId = "partidos_gerar_visualizacoes2",
                              #                          label = "Jalankan",
                              #                          style = "fill",
                              #                          color = "success",
                              #                          icon = icon("check"), size = "sm")
                              #        )
                              # )
 
)