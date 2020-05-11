hasil_analisis <- tabPanel(title = "Hasil Analisis", 
                        value = "hasil_analisis",
                        # br(), hr(),
                        column(width = 12,
                               column(width = 2,
                                      pickerInput(inputId = "categorySector", 
                                                  label = "Kategori", 
                                                  choices = c("Ekonomi", "Energi", "Lahan", "Limbah"), 
                                                  selected = NULL, 
                                                  options = list(`live-search` = TRUE))
                               ),
                               column(width = 2,
                                      conditionalPanel(condition = "input.categorySector=='Ekonomi'",
                                                       pickerInput(inputId = "pprkResults", 
                                                                   label = "Output yang Ditampilkan", 
                                                                   choices = c("PDRB", "Backward Linkage", "Forward Linkage", "Angka Pengganda Pendapatan Rumah Tangga", "Angka Pengganda Tenaga Kerja", "Angka Pengganda Output",
                                                                               "Upah gaji", "Rasio Upah gaji per Surplus Usaha", "Pendapatan per kapita", "Perbandingan Angka Pengganda"),
                                                                   selected = "PDRB",
                                                                   options = list(`live-search` = TRUE))
                                      ),
                                      conditionalPanel(condition = "input.categorySector=='Energi'",
                                                       pickerInput(inputId = "pprkEnergy", 
                                                                   label = "Output yang Ditampilkan", 
                                                                   choices = c("Angka Pengganda Energi", "Koefisien Intensitas Energi", "Emisi dari Penggunaan Energi"),
                                                                   selected = "Angka Pengganda Energi",
                                                                   options = list(`live-search` = TRUE))
                                      ),
                                      conditionalPanel(condition = "input.categorySector=='Limbah'",
                                                       pickerInput(inputId = "pprkWaste", 
                                                                   label = "Output yang Ditampilkan", 
                                                                   choices = c("Angka Pengganda Buangan Limbah", "Koefisien Produk Limbah", "Emisi dari Limbah"),
                                                                   selected = "Angka Pengganda Buangan Limbah",
                                                                   options = list(`live-search` = TRUE))
                                      ),
                                      conditionalPanel(condition = "input.categorySector=='Lahan'",
                                                       pickerInput(inputId = "pprkLand", 
                                                                   label = "Output yang Ditampilkan", 
                                                                   choices = c("Matriks Distribusi Lahan", "Koefisien Kebutuhan Lahan", "Koefisien Produktivitas Lahan", "Permintaan Lahan"),
                                                                   selected = "Matriks Distribusi Lahan",
                                                                   options = list(`live-search` = TRUE))
                                      )
                               ),
                               column(width = 2, style = "padding-top: 57px;",
                                      actionBttn(inputId = "pageTwo", 
                                                 label = "Jalankan", 
                                                 style = "fill", 
                                                 color = "success", 
                                                 icon = icon("check"), size = "sm") 
                               ),
                               
                               ### Output ####
                               column(width = 12,
                                      conditionalPanel(condition = "input.pageTwo > 0",
                                                       fluidRow(
                                                         column(width=12,
                                                                box(width=NULL,
                                                                    h3(textOutput("tableDesc")),
                                                                    hr(),
                                                                    div(style="overflow-x: scroll", dataTableOutput('tableResults')),
                                                                    downloadButton('downloadTable', 'Download Table (.csv)')
                                                                )
                                                         )
                                                       )
                                      )
                               ),
                               column(width = 12,
                                      conditionalPanel(condition = "input.pageTwo > 0",
                                                       conditionalPanel(
                                                         condition="input.pprkResults=='Perbandingan Angka Pengganda'",
                                                         uiOutput("sectorSelection")
                                                       )
                                      )
                               ),
                               column(width = 12,
                                      conditionalPanel(condition = "input.pageTwo > 0",
                                                       conditionalPanel(
                                                         condition="input.pprkResults!='Pendapatan per kapita'",
                                                         plotlyOutput("plotlyResults")
                                                       )
                                      )
                               )
                               # column(width = 12,
                               #        conditionalPanel(condition = "input.pageTwo > 0",
                               #                         HTML("<center><h1>PLOT</h1></center>"),
                               #                         column(width = 12,
                               #                                withSpinner(plotlyOutput("plotTest"), type = 6)
                               #                         )           
                               #        )
                               # ),
                               # column(width = 12,
                               #        conditionalPanel(condition = "input.pageTwo > 0",
                               #                         HTML("<center><h1>TABLE</h1></center>"),
                               #                         column(width = 12,
                               #                                withSpinner(dataTableOutput("testTable"), type = 6)
                               #                         )           
                               #        )
                               # )
                        )
)