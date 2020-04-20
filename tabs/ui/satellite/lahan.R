land <- tabPanel(title = "Lahan", 
                          value = "land",
                          column(width = 12,
                                 br(),
                                 wellPanel(
                                   HTML("<h1>Akun Satelit: Lahan<h1>"),
                                 )
                          ),
                          ##-- Outputs ----
                          column(width = 12,
                                 withSpinner(rHandsontableOutput("SatelitLahan"), type = 6),
                                 style = "padding-bottom: 10px;"
                          ),
                          # column(width = 12,
                          #        withSpinner(dataTableOutput("testEditTable"), type = 6),
                          #        style = "padding-bottom: 10px;"
                          # )
                          actionBttn(inputId = "saveBttn",
                                     label = "Simpan",
                                     style = "fill",
                                     color = "success",
                                     icon = icon("check"), size = "sm")
)