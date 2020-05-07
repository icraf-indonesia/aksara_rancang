bauScenario <- tabPanel(title = "Skenario BAU", 
                              value = "bauScenario",
                        br(),
                        pickerInput(inputId = "selectProjType",
                                    label = "Tipe Proyeksi",
                                    choices = c("Proyeksi BAU berdasarkan perubahan tutupan lahan", "Proyeksi BAU berdasarkan pertumbuhan ekonomi"),
                                    selected = NULL,
                                    options = list(`live-search` = TRUE)),
                        hr(),
                        conditionalPanel(condition = "input.selectProjType=='Proyeksi BAU berdasarkan pertumbuhan ekonomi'",
                                         tags$div(id="inputProjType"),
                                         div(uiOutput("projTypeEconomyUI"),
                                             hr(),
                                             tags$div(id="gdpRateUI",
                                                      conditionalPanel(
                                                        condition = "input.typeIntervention=='Tipe 1'",
                                                        h3("Tipe 1: Pertumbuhan ekonomi sama untuk setiap lapangan usaha, namun berbeda setiap tahun"),
                                                        selectInput("yearBAUInv", "Pilih tahun:", choices = 2010:2030, selected=2015),
                                                        sliderInput("gdpRate", "Laju pertumbuhan ekonomi:", min=0, max=10, post="%", value=5, step=0.01, width = "600px")
                                                      ),
                                                      conditionalPanel(
                                                        condition = "input.typeIntervention=='Tipe 2'",
                                                        h3("Tipe 2: Pertumbuhan ekonomi berbeda baik untuk setiap lapangan usaha, maupun setiap tahun proyeksi")
                                                      ),
                                                      br(),
                                                      actionButton("saveTableBauType", "Simpan Tabel"),
                                                      actionButton("buttonBAU", "Jalankan Simulasi"),
                                                      br(), br(),
                                                      rHandsontableOutput('tableBAUType'),
                                                      br())
                                             ),
                                         # uiOutput("resultUI"),
                                         conditionalPanel(condition = "input.buttonBAU>0",
                                                          h3("Hasil Analisis"),
                                                          uiOutput("resultUI"))
                                             # conditionalPanel(
                                             #   condition = "input.buttonBAU>0",
                                             #   removeUI("#tableBAUType"),
                                             #   uiOutput("resultUI"))
                                         ),
                        conditionalPanel(condition = "input.selectProjType=='Proyeksi BAU berdasarkan perubahan tutupan lahan'",
                                         tags$div(id="inputProjType"),
                                         div(br(),
                                             fluidRow(
                                               # h3(style="padding-left: 15px;", "Pilih Data Input BAU"), 
                                               box(width=12,
                                                   #        tabPanel("Tabel LDM Proporsi", id="TabPanelBAUDataLDMProp",
                                                   div(style="overflow-x: scroll", 
                                                       uiOutput("LDMFileOptions"),
                                                       dataTableOutput("LDMListTable"),
                                                       dataTableOutput("LDMTampil"),
                                                       uiOutput('LDMTableTampilUI'),
                                                       uiOutput('modalLDMUI'))
                                               )
                                             ),
                                             hr(),
                                             uiOutput("projTypeLandUI")
                                             )
                                         ))
                                             
                        #       tabBox(id="tabPanelBAU", width = 12, 
                        #              tabPanel("Proyeksi BAU berdasarkan Pertumbuhan Ekonomi", id="inputProjType",
                        #                       div(br(),
                        #                           uiOutput("projTypeEconomyUI"),
                        #                           hr(),
                        #                           conditionalPanel(
                        #                             condition = "input.typeIntervention=='Tipe 1'",
                        #                             h3("Tipe 1: Pertumbuhan ekonomi sama untuk setiap lapangan usaha, namun berbeda setiap tahun"),
                        #                             selectInput("yearBAUInv", "Pilih tahun:", choices = 2010:2030, selected=2015),
                        #                             sliderInput("gdpRate", "Laju pertumbuhan ekonomi:", min=0, max=10, post="%", value=5, step=0.01, width = "600px")
                        #                           ),
                        #                           conditionalPanel(
                        #                             condition = "input.typeIntervention=='Tipe 2'",
                        #                             h3("Tipe 2: Pertumbuhan ekonomi berbeda baik untuk setiap lapangan usaha, maupun setiap tahun proyeksi")
                        #                           ),
                        #                           actionButton("saveTableBauType", "Simpan Tabel"),
                        #                           actionButton("buttonBAU", "Jalankan Simulasi"),
                        #                           hr(),
                        #                           rHandsontableOutput('tableBAUType')
                        #                           )),
                        #              tabPanel("Proyeksi BAU berdasarkan Perubahan Tutupan Lahan", id="inputProjType",
                        #                       div(br(),
                        #                           fluidRow(
                        #                             # h3(style="padding-left: 15px;", "Pilih Data Input BAU"), 
                        #                             box(width=12,
                        #                             #        tabPanel("Tabel LDM Proporsi", id="TabPanelBAUDataLDMProp",
                        #                                             div(style="overflow-x: scroll", 
                        #                                                 uiOutput("LDMFileOptions"),
                        #                                                 dataTableOutput("LDMListTable"),
                        #                                                 dataTableOutput("LDMTampil"),
                        #                                                 uiOutput('LDMTableTampilUI'),
                        #                                                 uiOutput('modalLDMUI'))
                        #                                    )
                        #                             ),
                        #                           hr(),
                        #                           uiOutput("projTypeLandUI")
                        #                           ))
                        #              )
                        # )
                              #        tabPanel("Proyeksi", id="tab1",
                              #                 column(width = 12,
                              #                               column(width = 2,
                              #                                      pickerInput(inputId = "selectProjType",
                              #                                                  label = "Tipe Proyeksi",
                              #                                                  choices = c("Proyeksi BAU berdasarkan pertumbuhan ekonomi", "Proyeksi BAU berdasarkan perubahan tutupan lahan"),
                              #                                                  selected = NULL,
                              #                                                  options = list(`live-search` = TRUE))
                              #                               ),
                              #                               column(width = 1, style = "padding-top: 57px;",
                              #                                      actionBttn(inputId = "runButton1",
                              #                                                 label = "Jalankan",
                              #                                                 style = "fill",
                              #                                                 color = "success",
                              #                                                 icon = icon("check"), size = "sm")
                              #                               )
                              #                        ),
                              #                 tags$div(id="placeholderTable1")
                              #                 ),
                              #        tabPanel("Rate Table", id="tab2",
                              #                 tags$div(id='inputProjType'),
                              #                 conditionalPanel(
                              #                   condition = "input.typeIntervention=='Tipe 1'",
                              #                   h3("Tipe 1: Pertumbuhan ekonomi sama untuk setiap lapangan usaha, namun berbeda setiap tahun"),
                              #                   selectInput("yearBAUInv", "Pilih tahun:", choices = 2010:2030, selected=2015),
                              #                   sliderInput("gdpRate", "Laju pertumbuhan ekonomi:", min=0, max=10, post="%", value=5, step=0.01, width = "600px")
                              #                 ),
                              #                 conditionalPanel(
                              #                   condition = "input.typeIntervention=='Tipe 2'",
                              #                   h3("Tipe 2: Pertumbuhan ekonomi berbeda baik untuk setiap lapangan usaha, maupun setiap tahun proyeksi")
                              #                 ),
                              #                 actionButton("saveTableBauType", "Simpan Tabel"),
                              #                 actionButton("buttonBAU", "Jalankan Simulasi"),
                              #                 hr(),
                              #                 rHandsontableOutput('tableBAUType'),
                              #                 # column(width = 12,
                              #                 #               column(width = 2,
                              #                 #                      pickerInput(inputId = "partido_ano",
                              #                 #                                  label = "Tipe Intervensi",
                              #                 #                                  choices = c("Tipe 1", "Tipe 2"),
                              #                 #                                  selected = NULL,
                              #                 #                                  options = list(`live-search` = TRUE))
                              #                 #               ),
                              #                 #               column(width = 2,
                              #                 #                      pickerInput(inputId = "dateFrom",
                              #                 #                                  label = "Tahnun Awal",
                              #                 #                                  choices = 2010:2030,
                              #                 #                                  selected = 2015,
                              #                 #                                  options = list(`live-search` = TRUE))
                              #                 #               ),
                              #                 #               column(width = 2,
                              #                 #                      pickerInput(inputId = "dateTo",
                              #                 #                                  label = "Tahun Akhir",
                              #                 #                                  choices = 2010:2030,
                              #                 #                                  selected = 2030,
                              #                 #                                  options = list(`live-search` = TRUE))
                              #                 #               ),
                              #                 #               column(width = 2, style = "padding-top: 57px;",
                              #                 #                      actionBttn(inputId = "runButton2",
                              #                 #                                 label = "Buat Tabel",
                              #                 #                                 style = "fill",
                              #                 #                                 color = "success",
                              #                 #                                 icon = icon("check"), size = "sm")
                              #                 #                      )
                              #                 #        ),
                              #                 tags$div(id="placeholderTable2")
                              #                 ),
                              #        tabPanel("Hasil Analisis", id="tab3", 
                              #                 column(width = 12,
                              #                        column(width = 2,
                              #                               pickerInput(inputId = "bauResults",
                              #                                           label = "Output yang Ditampilkan",
                              #                                           choices = c("Proyeksi PDRB", "Proyeksi Upah per Kapita", "Proyeksi Upah Gaji", "Proyeksi Tenaga Kerja",
                              #                                                       "Proyeksi Konsumsi Energi", "Proyeksi Emisi Terkait Konsumsi Energi", "Proyeksi Buangan Limbah",
                              #                                                       "Proyeksi Emisi Terkait Buangan Limbah", "Proyeksi Total Emisi", "Proyeksi Intensitas Emisi"),
                              #                                           selected = "Proyeksi PDRB")
                              #                        ),
                              #                        column(width = 1, style = "padding-top: 57px;",
                              #                               actionBttn(inputId = "runButton3",
                              #                                          label = "Jalankan",
                              #                                          style = "fill",
                              #                                          color = "success",
                              #                                          icon = icon("check"), size = "sm")
                              #                        )
                              #                 ),
                              #                 tags$div(id="placeholderTable3")
                              #        )
                              # )
                              # ### Output ####
                              # # conditionalPanel(condition = "input.partidos_gerar_visualizacoes > 0",
                              # #                  column(width = 12,
                              # #                         withSpinner(rHandsontableOutput("testEditTableBAU"), type = 6),
                              # #                         style = "padding-bottom: 10px;")
                              # #                  # column(width = 12,
                              # #                  #        withSpinner(dataTableOutput("testTableBAU"), type = 6),
                              # #                  #        style = "padding-bottom: 10px;"
                              # #                  # )
                              # # ),
                              # # conditionalPanel(condition = "input.partidos_gerar_visualizacoes > 0 ",
                              # #                  actionBttn(inputId = "saveBttn-bau",
                              # #                             label = "Simpan",
                              # #                             style = "fill",
                              # #                             color = "success",
                              # #                             icon = icon("check"), size = "sm")
                              # # ),
                              # # conditionalPanel(condition = "input.partidos_gerar_visualizacoes2 > 0",
                              # #                  HTML("<center><h1>PLOT</h1></center>"),
                              # #                  column(width = 12,
                              # #                         withSpinner(plotlyOutput("plotBAU"), type = 6)
                              # #                  )           
                              # # ),
                              # # conditionalPanel(condition = "input.partidos_gerar_visualizacoes2 > 0",
                              # #                  HTML("<center><h1>TABLE</h1></center>"),
                              # #                  column(width = 12,
                              # #                         withSpinner(dataTableOutput("testTableBAU"), type = 6)
                              # #                  )           
                              # # )
# )