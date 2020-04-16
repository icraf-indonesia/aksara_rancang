partido_geral <- tabPanel(title = "Energi & Transportasi", 
                          value = "partidos_geral",
                          column(width = 12,
                                 br(),
                                 wellPanel(
                                   HTML("<h1>Akun Satelit: Energi & Transportasi<h1>"),
                                 )
                          ),
                          ##-- Outputs ----
                          column(width = 12,
                                 withSpinner(rHandsontableOutput("testEditTable"), type = 6),
                                 style = "padding-bottom: 10px;"
                          ),
                          # column(width = 12,
                          #        withSpinner(dataTableOutput("testEditTable"), type = 6),
                          #        style = "padding-bottom: 10px;"
                          # )
                          actionBttn(inputId = "eleicoes_gerar_visualizacoes_br",
                                     label = "Simpan",
                                     style = "fill",
                                     color = "success",
                                     icon = icon("check"), size = "sm")
)