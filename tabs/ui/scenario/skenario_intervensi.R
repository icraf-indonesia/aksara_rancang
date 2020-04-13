perfil <- tabPanel(title = "Skenario Intervensi", 
                   value = "candidatos_perfil",
                   # br(), hr(),
                   # column(width = 10,
                   #        column(width = 2,
                   #               pickerInput(inputId = "perfil_candidato_ano", 
                   #                           label = "Year", 
                   #                           choices = anos, 
                   #                           selected = 2014, 
                   #                           options = list(`live-search` = TRUE))
                   #        ),
                   #        column(width = 2,
                   #               pickerInput(inputId = "perfil_candidato_cargo", 
                   #                           label = "Position", 
                   #                           choices = cargos, 
                   #                           selected = "PRESIDENT",
                   #                           options = list(`live-search` = TRUE))
                   #        ),
                   #        column(width = 2,
                   #               pickerInput(inputId = "perfil_candidato_turno", 
                   #                           label = "Round", 
                   #                           choices = c("1º round", "2º round"), 
                   #                           selected = "1º",
                   #                           options = list(`live-search` = TRUE))
                   #        ),
                   #        column(width = 2,
                   #               pickerInput(inputId = "perfil_candidato_partido", 
                   #                           label = "Party", 
                   #                           choices = levels(factor(x = c("All parties", partidos),
                   #                                                   levels = c("All parties", partidos))), 
                   #                           selected = "All parties",
                   #                           options = list(`live-search` = TRUE,
                   #                                          `none-selected-text` = "None selected"))
                   #        ),
                   #        column(width = 2,
                   #               pickerInput(inputId = "perfil_candidato_estado", 
                   #                           label = "State", 
                   #                           choices = levels(factor(x = c("All states", estados),
                   #                                                   levels = c("All states", estados))), 
                   #                           selected = "Todos os estados",
                   #                           options = list(`live-search` = TRUE,
                   #                                          `none-selected-text` = "None selected"))
                   #        ),
                   #        column(width = 2,
                   #               pickerInput(inputId = "perfil_candidato_cpf", 
                   #                           label = "Candidate", 
                   #                           choices = NULL, 
                   #                           selected = NULL,
                   #                           options = list(`live-search` = TRUE,
                   #                                          `none-selected-text` = "None selected"))
                   #        )
                   # ), 
                   tabBox(id="tabPanelIntervensi", width = 12, 
                          tabPanel("Sektor Energi & Transportasi", id="energi-transport_int",
                                   br(),
                                   # uiOutput("test")
                                   actionBttn(inputId = "perfil_candidato_gerar_visualizacoes", 
                                                        label = "Deskripsi Skenario", 
                                                        style = "fill", 
                                                        color = "success", 
                                                        # icon = icon("data"), 
                                                        size = "sm")
                          ),
                          tabPanel("Sektor Lahan", id="lahan_int",
                                   br(),
                                   # tableOutput("perfil_candidato_gerar_visualizacoes")
                                   actionBttn(inputId = "perfil_candidato_gerar_visualizacoes",
                                              label = "Deskripsi Skenario",
                                              style = "fill",
                                              color = "success",
                                              # icon = icon("data"),
                                              size = "sm")
                          ),
                          tabPanel("Sektor Limbah", id="limbah_int",
                                   br(),
                                   actionBttn(inputId = "perfil_candidato_gerar_visualizacoes", 
                                              label = "Deskripsi Skenario", 
                                              style = "fill", 
                                              color = "success", 
                                              # icon = icon("data"), 
                                              size = "sm")
                          ),
                          tabPanel("Sektor Pertanian", id="pertanian_int",
                                   br(),
                                   actionBttn(inputId = "perfil_candidato_gerar_visualizacoes", 
                                              label = "Deskripsi Skenario", 
                                              style = "fill", 
                                              color = "success", 
                                              # icon = icon("data"), 
                                              size = "sm")
                          )),
                   ##-- Outputs ----
                   column(width = 12,
                          column(width = 6,
                                 conditionalPanel(condition = "input.perfil_candidato_gerar_visualizacoes > 0",
                                                  # HTML("<center><h1>PROPORTION OF VOTES</h1></center>"),
                                                  br(), 
                                                  column(width = 12,
                                                         withSpinner(leafletOutput("perfil_candidatos_mapa", height = "500px"), type = 6)
                                                  )
                                 )
                          ),
                          column(width = 6,
                                 conditionalPanel(condition = "input.perfil_candidato_gerar_visualizacoes > 0",
                                                  # HTML("<center><h1>CANDIDATE PROFILE</h1></center>"),
                                                  column(width = 12,
                                                         withSpinner(uiOutput("perfil_candidato"), type = 6)
                                                  )           
                                 )
                          )
                   )
)

# perfil <- tabPanel(title = "Skenario BAU", 
#                    value = "candidatos_perfil",
#                    br(), hr(),
#                    column(width = 12,
#                           column(width = 2,
#                                  pickerInput(inputId = "partido_ano",
#                                              label = "Tipe Proyeksi",
#                                              choices = c("Proyeksi BAU berdasarkan pertumbuhan ekonomi", "Proyeksi BAU berdasarkan perubahan tutupan lahan"),
#                                              selected = NULL,
#                                              options = list(`live-search` = TRUE))
#                           ),
#                           column(width = 1, style = "padding-top: 57px;",
#                                  actionBttn(inputId = "partidos_gerar_visualizacoes",
#                                             label = "Select",
#                                             style = "fill",
#                                             color = "success",
#                                             icon = icon("check"), size = "sm")
#                           )
#                    ),
#                    column(width = 12,
#                           column(width = 2,
#                                  pickerInput(inputId = "partido_ano",
#                                              label = "Tipe Intervensi",
#                                              choices = c("Tipe 1", "Tipe 2"),
#                                              selected = NULL,
#                                              options = list(`live-search` = TRUE))
#                           ),
#                           column(width = 2,
#                                  pickerInput(inputId = "partido_cargo",
#                                              label = "Tahnun Awal",
#                                              choices = 2010:2030,
#                                              selected = 2015,
#                                              options = list(`live-search` = TRUE))
#                           ),
#                           column(width = 2,
#                                  pickerInput(inputId = "partido_cargo",
#                                              label = "Tahnun Akhir",
#                                              choices = 2010:2030,
#                                              selected = 2030,
#                                              options = list(`live-search` = TRUE))
#                           ),
#                           column(width = 2, style = "padding-top: 57px;",
#                                  actionBttn(inputId = "partidos_gerar_visualizacoes",
#                                             label = "Buat Tabel",
#                                             style = "fill",
#                                             color = "success",
#                                             icon = icon("check"), size = "sm")
#                           )
#                    ),
#                    column(width = 12,
#                           column(width = 2,
#                                  pickerInput(inputId = "partido_ano",
#                                              label = "Tipe Proyeksi",
#                                              choices = c("Proyeksi PDRB", "Proyeksi Upah per Kapita", "Proyeksi Upah Gaji", "Proyeksi Tenaga Kerja",
#                                                          "Proyeksi Konsumsi Energi", "Proyeksi Emisi Terkait Konsumsi Energi", "Proyeksi Buangan Limbah",
#                                                          "Proyeksi Emisi Terkait Buangan Limbah", "Proyeksi Total Emisi", "Proyeksi Intensitas Emisi"),
#                                              selected = "Proyeksi PDRB",
#                                              options = list(`live-search` = TRUE))
#                           ),
#                           column(width = 1, style = "padding-top: 57px;",
#                                  actionBttn(inputId = "partidos_gerar_visualizacoes",
#                                             label = "Select",
#                                             style = "fill",
#                                             color = "success",
#                                             icon = icon("check"), size = "sm")
#                           )
#                    )
# )