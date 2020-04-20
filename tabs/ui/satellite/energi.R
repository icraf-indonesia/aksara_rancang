energy <- tabPanel(title = "Energi & Transportasi", 
                          value = "energy",
                          column(width = 12,
                                 br(),
                                 wellPanel(
                                   HTML("<h1>Akun Satelit: Energi & Transportasi<h1>"),
                                 )
                          ),
                          ##-- Outputs ----
                          column(width = 12,
                                 withSpinner(rHandsontableOutput("SatelitEnergi"), type = 6),
                                 style = "padding-bottom: 10px;"
                          ),
                          # column(width = 12,
                          #        withSpinner(dataTableOutput("SatelitEnergi"), type = 6),
                          #        style = "padding-bottom: 10px;"
                          # ),
                          actionBttn(inputId = "saveBttn",
                                     label = "Simpan",
                                     style = "fill",
                                     color = "success",
                                     icon = icon("check"), size = "sm")
)