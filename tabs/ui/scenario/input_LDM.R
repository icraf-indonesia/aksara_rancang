inputLDM <- tabPanel(title = "LDM Input", 
                        value = "inputLDM",
                     div(fluidRow(
                           h3(style="padding-left: 15px;", "Pilih Data Input BAU"),
                           br(),
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
                         # hr(),
                         # uiOutput("projTypeLandUI")
                     ))
                         