perfil_eleitorado <- tabPanel(title = "Skenario BAU", 
                              value = "candidatos_perfil_eleitorado",
                              # br(), hr(),
                              column(width = 12,
                                     column(width = 2,
                                            pickerInput(inputId = "partido_ano",
                                                        label = "Tipe Proyeksi",
                                                        choices = c("Proyeksi BAU berdasarkan pertumbuhan ekonomi", "Proyeksi BAU berdasarkan perubahan tutupan lahan"),
                                                        selected = NULL,
                                                        options = list(`live-search` = TRUE))
                                     ),
                                     column(width = 1, style = "padding-top: 57px;",
                                            actionBttn(inputId = "partidos_gerar_visualizacoes",
                                                       label = "Select",
                                                       style = "fill",
                                                       color = "success",
                                                       icon = icon("check"), size = "sm")
                                     )
                              ),
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
                              ),
                              column(width = 12,
                                     column(width = 2,
                                            pickerInput(inputId = "partido_ano",
                                                        label = "Tipe Proyeksi",
                                                        choices = c("Proyeksi PDRB", "Proyeksi Upah per Kapita", "Proyeksi Upah Gaji", "Proyeksi Tenaga Kerja",
                                                                    "Proyeksi Konsumsi Energi", "Proyeksi Emisi Terkait Konsumsi Energi", "Proyeksi Buangan Limbah",
                                                                    "Proyeksi Emisi Terkait Buangan Limbah", "Proyeksi Total Emisi", "Proyeksi Intensitas Emisi"),
                                                        selected = "Proyeksi PDRB",
                                                        options = list(`live-search` = TRUE))
                                     ),
                                     column(width = 1, style = "padding-top: 57px;",
                                            actionBttn(inputId = "partidos_gerar_visualizacoes",
                                                       label = "Select",
                                                       style = "fill",
                                                       color = "success",
                                                       icon = icon("check"), size = "sm")
                                     )
                              )
)