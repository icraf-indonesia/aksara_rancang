data_historis_debug <- tabPanel(title = "Data Historis Debug",
                          value = "data_historis_debug",
                          column(width = 2,
                                 pickerInput(inputId = "categoryProvince2", 
                                             label = "Provinsi", 
                                             list(`Regional Barat` = list("Aceh" = "Aceh", "Bangka Belitung"="BaBel", "Bengkulu"="Bengkulu", "Jambi"="Jambi", "Kepulauan Riau"="KepRi",
                                                                          "Lampung"="Lampung", "Riau"="Riau", "Sumatera Barat"="SumBar", "Sumatera Selatan"="SumSel", "Sumatera Utara"="SumUt"),
                                                  `Regional Tengah` = list("Bali"="Bali","Banten"="Banten", "DKI Jakarta"="DKIJakarta", "D.I. Yogyakarta"="DIY", "Jawa Barat"="JaBar",
                                                                           "Jawa Tengah"="JaTeng", "Jawa Timur"="JaTim", "Kalimantan Barat"="KalBar",
                                                                           "Kalimantan Selatan"="KalSel", "Kalimantan Tengah"="KalTeng", "Kalimantan Utara"="KalTara", "Kalimantan Timur"="KalTim", 
                                                                           "Nusa Tenggara Barat"="NTB", "Nusa Tenggara Timur"="NTT"),
                                                  `Regional Timur` = list("Gorontalo"="Gorontalo", "Maluku"="Maluku", "Maluku Utara"="Maluku_Utara",
                                                                          "Papua"="Papua", "Papua Barat"="Papua_Barat", "Sulawesi Selatan"="Sulawesi_Selatan", "Sulawesi Tengah"="Sulawesi_Tengah",
                                                                          "Sulawesi Tenggara"="Sulawesi_Tenggara", "Sulawesi Barat"="Sulawesi_Barat", "Sulawesi Utara"="Sulawesi_Utara")),
                                             selected = "'Jawa Barat'='JaBar'", 
                                             options = NULL)
                          ),
                          column(width = 2, style = "padding-top: 57px;",
                                 actionBttn(inputId = "inputLogin2", 
                                            label = "Jalankan", 
                                            style = "fill", 
                                            color = "success", 
                                            icon = icon("check"), size = "sm") 
                          ),
                          ##-- Outputs ----
                          conditionalPanel(condition = "input.inputLogin2 > 0",
                                           tabBox(id="tabPanelHistori", width = 12, 
                                                  tabPanel("Tabel Input-Output", id="boxIO",
                                                           div(style="overflow-x: scroll; padding-top: 20px;", dataTableOutput('tableIO2'))
                                                  ),
                                                  tabPanel("Tabel Tenaga Kerja", id="boxLabour", 
                                                           div(style="overflow-x: scroll; padding-top: 20px;", dataTableOutput('SatelitTenagaKerja2'))
                                                  ))
                          )
)