eleicoes_uf <- tabPanel(title = "Hasil Analasis", 
                        value = "uf",
                        # br(), hr(),
                        column(width = 10,
                               column(width = 2,
                                      pickerInput(inputId = "eleicoes_ano_uf", 
                                                  label = "Kategori", 
                                                  choices = c("Ekonomi", "Energi", "Lahan", "Limbah"), 
                                                  selected = NULL, 
                                                  options = list(`live-search` = TRUE))
                               ),
                               column(width = 2,
                                      pickerInput(inputId = "eleicoes_cargo_uf", 
                                                  label = "Output yang Ditampilkan", 
                                                  choices = c("PDRB", "Backward Linkage", "Forward Linkage", "Angka Pengganda Pendapatan Rumah Tangga", "Angka Pengganda Tenaga Kerja", "Angka Pengganda Output",
                                                              "Upah gaji", "Rasio Upah gaji per Surplus Usaha", "Pendapatan per kapita", "Perbandingan Angka Pengganda"
                                                  ),
                                                  selected = "PRESIDENT",
                                                  options = list(`live-search` = TRUE))
                               ),
                               column(width = 2, style = "padding-top: 57px;",
                                      actionBttn(inputId = "eleicoes_gerar_visualizacoes_uf", 
                                                 label = "Jalankan", 
                                                 style = "fill", 
                                                 color = "success", 
                                                 icon = icon("check"), size = "sm") 
                               )
                        )
                        
)