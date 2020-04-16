partido_analise <- tabPanel(title = "Lahan", 
                          value = "partido_analise",
                          column(width = 12,
                                 br(),
                                 wellPanel(
                                   HTML("<h1>Akun Satelit: Lahan<h1>"),
                                 )
                          ),
                          ##-- Outputs ----
                          column(width = 12,
                                 withSpinner(rHandsontableOutput("testEditTableLahan"), type = 6),
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