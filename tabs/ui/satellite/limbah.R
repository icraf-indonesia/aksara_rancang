partido_analise2 <- tabPanel(title = "Limbah", 
                          value = "partido_analise2",
                          column(width = 12,
                                 br(),
                                 wellPanel(
                                   HTML("<h1>Akun Satelit: Limbah<h1>"),
                                 )
                          ),
                          ##-- Outputs ----
                          column(width = 12,
                                 withSpinner(rHandsontableOutput("testEditTableLimbah"), type = 6),
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