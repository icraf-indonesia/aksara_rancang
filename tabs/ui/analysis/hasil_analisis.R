eleicoes_uf <- tabPanel(title = "Hasil Analasis", 
                        value = "uf",
                        # br(), hr(),
                        column(width = 12,
                               column(width = 2,
                                      pickerInput(inputId = "eleicoes_ano_uf", 
                                                  label = "Kategori", 
                                                  choices = c("Ekonomi", "Energi", "Lahan", "Limbah"), 
                                                  selected = NULL, 
                                                  options = list(`live-search` = TRUE))
                               ),
                               column(width = 2,
                                      conditionalPanel(condition = "input.eleicoes_ano_uf=='Ekonomi'",
                                                       pickerInput(inputId = "eleicoes_cargo_uf", 
                                                                   label = "Output yang Ditampilkan", 
                                                                   choices = c("PDRB", "Backward Linkage", "Forward Linkage", "Angka Pengganda Pendapatan Rumah Tangga", "Angka Pengganda Tenaga Kerja", "Angka Pengganda Output",
                                                                               "Upah gaji", "Rasio Upah gaji per Surplus Usaha", "Pendapatan per kapita", "Perbandingan Angka Pengganda"),
                                                                   selected = "PDRB",
                                                                   options = list(`live-search` = TRUE))
                                      ),
                                      conditionalPanel(condition = "input.eleicoes_ano_uf=='Energi'",
                                                       pickerInput(inputId = "eleicoes_cargo_uf", 
                                                                   label = "Output yang Ditampilkan", 
                                                                   choices = c("Angka Pengganda Energi", "Koefisien Intensitas Energi", "Emisi dari Penggunaan Energi"),
                                                                   selected = "Angka Pengganda Energi",
                                                                   options = list(`live-search` = TRUE))
                                      ),
                                      conditionalPanel(condition = "input.eleicoes_ano_uf=='Limbah'",
                                                       pickerInput(inputId = "eleicoes_cargo_uf", 
                                                                   label = "Output yang Ditampilkan", 
                                                                   choices = c("Angka Pengganda Buangan Limbah", "Koefisien Produk Limbah", "Emisi dari Limbah"),
                                                                   selected = "Angka Pengganda Buangan Limbah",
                                                                   options = list(`live-search` = TRUE))
                                      ),
                                      conditionalPanel(condition = "input.eleicoes_ano_uf=='Lahan'",
                                                       pickerInput(inputId = "eleicoes_cargo_uf", 
                                                                   label = "Output yang Ditampilkan", 
                                                                   choices = c("Matriks Distribusi Lahan", "Koefisien Kebutuhan Lahan", "Koefisien Produktivitas Lahan", "Permintaan Lahan"),
                                                                   selected = "Matriks Distribusi Lahan",
                                                                   options = list(`live-search` = TRUE))
                                      )
                               ),
                               column(width = 2, style = "padding-top: 57px;",
                                      actionBttn(inputId = "eleicoes_gerar_visualizacoes_uf", 
                                                 label = "Jalankan", 
                                                 style = "fill", 
                                                 color = "success", 
                                                 icon = icon("check"), size = "sm") 
                               ),
                               
                               ### Output ####
                               column(width = 12,
                                      conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_uf > 0",
                                                       HTML("<center><h1>PLOT</h1></center>"),
                                                       column(width = 12,
                                                              withSpinner(plotlyOutput("plotTest"), type = 6)
                                                       )           
                                      )
                               ),
                               column(width = 12,
                                      conditionalPanel(condition = "input.eleicoes_gerar_visualizacoes_uf > 0",
                                                       HTML("<center><h1>TABLE</h1></center>"),
                                                       column(width = 12,
                                                              withSpinner(dataTableOutput("testTable"), type = 6)
                                                       )           
                                      )
                               )
                        )
)