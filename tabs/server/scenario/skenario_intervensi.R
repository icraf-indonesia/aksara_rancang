callModule(buttonModule, "forEnergy", energyData, type="energy")
callModule(buttonModule, "forWaste", wasteData, type="waste")
callModule(buttonModule, "forAgri", agriData, type="agriculture")
callModule(buttonModule, "forLand", landData, type="land")


# observeEvent(input$defineScenarioEnergi, {
#   showModal(modalDialog(sidebarLayout(sidebarPanel(
#           fluidRow(
#             textInput("intervensiDef",
#                       label = "Nama Skenario"
#             ),
#             selectInput("tahunAwal",
#                         label = "Tahun Awal",
#                         choices = c(2010:2030)),
#             selectInput("tahunAkhir",
#                         label = "Tahun Akhir",
#                         choices = c(2010:2030),
#                         selected = 2030),
#             textAreaInput("deskripsi",
#                           label = "Deskripsi Skenario",
#                           width = "330px")
#           ),
#           tags$br(),
#           actionButton("defHit","Tampilkan"),
#           width=5
#         ),
#         mainPanel(
#           tags$div(id = 'defPlaceholder'),
#           width = 7
#         )),
#         title = "Deskripsi Skenario",
#         footer = tagList(
#           actionButton("closeModalDef", "Simpan"),
#           actionButton("cancelModalDef", "Batal")
#         ),
#         size = "l",
#         easyClose = FALSE
#         ))
# })
# 
# observeEvent(input$defineScenarioLahan, {
#   showModal(modalDialog(sidebarLayout(sidebarPanel(
#     fluidRow(
#       textInput("intervensiDef",
#                 label = "Nama Skenario"
#       ),
#       selectInput("tahunAwal",
#                   label = "Tahun Awal",
#                   choices = c(2010:2030)),
#       selectInput("tahunAkhir",
#                   label = "Tahun Akhir",
#                   choices = c(2010:2030),
#                   selected = 2030),
#       textAreaInput("deskripsi",
#                     label = "Deskripsi Skenario",
#                     width = "330px")
#     ),
#     tags$br(),
#     actionButton("defHit","Tampilkan"),
#     width=5
#   ),
#   mainPanel(
#     tags$div(id = 'defPlaceholder'),
#     width = 7
#   )),
#   title = "Deskripsi Skenario",
#   footer = tagList(
#     actionButton("closeModalDef", "Simpan"),
#     actionButton("cancelModalDef", "Batal")
#   ),
#   size = "l",
#   easyClose = FALSE
#   ))
# })
# 
# observeEvent(input$defineScenarioLimbah, {
#   showModal(modalDialog(sidebarLayout(sidebarPanel(
#     fluidRow(
#       textInput("intervensiDef",
#                 label = "Nama Skenario"
#       ),
#       selectInput("tahunAwal",
#                   label = "Tahun Awal",
#                   choices = c(2010:2030)),
#       selectInput("tahunAkhir",
#                   label = "Tahun Akhir",
#                   choices = c(2010:2030),
#                   selected = 2030),
#       textAreaInput("deskripsi",
#                     label = "Deskripsi Skenario",
#                     width = "330px")
#     ),
#     tags$br(),
#     actionButton("defHit","Tampilkan"),
#     width=5
#   ),
#   mainPanel(
#     tags$div(id = 'defPlaceholder'),
#     width = 7
#   )),
#   title = "Deskripsi Skenario",
#   footer = tagList(
#     actionButton("closeModalDef", "Simpan"),
#     actionButton("cancelModalDef", "Batal")
#   ),
#   size = "l",
#   easyClose = FALSE
#   ))
# })
# 
# observeEvent(input$defineScenarioPertanian, {
#   showModal(modalDialog(sidebarLayout(sidebarPanel(
#     fluidRow(
#       textInput("intervensiDef",
#                 label = "Nama Skenario"
#       ),
#       selectInput("tahunAwal",
#                   label = "Tahun Awal",
#                   choices = c(2010:2030)),
#       selectInput("tahunAkhir",
#                   label = "Tahun Akhir",
#                   choices = c(2010:2030),
#                   selected = 2030),
#       textAreaInput("deskripsi",
#                     label = "Deskripsi Skenario",
#                     width = "330px")
#     ),
#     tags$br(),
#     actionButton("defHit","Tampilkan"),
#     width=5
#   ),
#   mainPanel(
#     tags$div(id = 'defPlaceholder'),
#     width = 7
#   )),
#   title = "Deskripsi Skenario",
#   footer = tagList(
#     actionButton("closeModalDef", "Simpan"),
#     actionButton("cancelModalDef", "Batal")
#   ),
#   size = "l",
#   easyClose = FALSE
#   ))
# })
# 
# observeEvent(input$closeModalDef,{
#   #browser()
#   removeModal()
# })
# 
# observeEvent(input$cancelModalDef,{
#   removeModal()
# })
# 
# observeEvent(input$defHit, {
#   insertUI(selector='#defPlaceholder',
#            where='afterEnd',
#            ui= uiOutput('defUIManual')
#   )
# })
# 
# output$defUIManual<- renderUI({
#   tagList(rHandsontableOutput('editDefine')
#   )
# })
# 
# ListButton_fun <- function(FUN, len, id, ...) {
#   inputs <- character(len)
#   for (i in seq_len(len)) {
#     inputs[i] <- as.character(FUN(paste0(id,i), ...))
#   }
#   inputs
# }
# 
# Nama_File = "file_uji_JaBar_2020-04-13_14-49-00"
# 
# tabelEnergi <- data.table(Nama_File,
#                           Sunting = ListButton_fun(
#                             FUN = actionButton,
#                             len = length(Nama_File),
#                             id = 'button_',
#                             label = "Edit Konstruksi Ekonomi dan Satelit Akun",
#                             onclick = sprintf('Shiny.onInputChange("%s",this.id)', "select_button")
#                             ),
#                           Jalankan = ListButton_fun(
#                             FUN = actionButton,
#                             len = length(Nama_File),
#                             id = 'tombol_',
#                             label = "Tampilkan Plot",
#                             onclick = sprintf('Shiny.onInputChange("%s",this.id)', "run_button")
#                             )
#                           )
# 
# output$tabelEnergiFinal <- renderDataTable({
#   DT = tabelEnergi
#   datatable(DT, escape = F)
# })
# 
# Sector <- c("Pertanian", "Perkebunan", "Hutan", "Perikanan")
# 
# observeEvent(input$closeModalFD,{
#   removeModal()
# })
# 
# observeEvent(input$select_button,{
#   #browser()
#   
#   showModal(
#     modalDialog( 
#       footer=tagList(
#         actionButton("closeModalFD", "Tutup")
#       ),
#       tabsetPanel(
#         tabPanel(
#           h2("Ekonomi"),
#           sidebarLayout(
#             sidebarPanel(
#               fluidRow(
#                 selectInput("intervensiEcon",
#                             label="Pilih Intervensi",
#                             choices=c("Final Demand","AV","Input-Output")),
#                 pickerInput("sektorEcon",
#                             label="Pilih Sektor", selected = Sector[1],
#                             choices=Sector,options = list(`actions-box` = TRUE),multiple = T)),
#               tags$br(),
#               actionButton("econHit","Tahun Intervensi"),
#               width=5
#             ),
#             mainPanel(
#               tags$div(id = 'FDPlaceholder'),
#               width=7)
#           ),
#           title="Sunting Intervensi Ekonomi"
#         ),
#         
#     ### BUTTON KONSTRUKSI SATELIT AKUN ###
#         tabPanel(
#           h2("Satelit akun"),
#           sidebarLayout(sidebarPanel(
#             fluidRow(
#               selectInput("intervensiSat",
#                           label="Pilih Intervensi",
#                           choices=c("Konsumsi Energi","Faktor Emisi")),
#               pickerInput("sektorSat",
#                           label="Pilih Sektor",selected = Sector[1],
#                           choices=Sector,options = list(`actions-box` = TRUE),multiple = T)),
#             tags$br(),
#             actionButton("satHit","Tahun Intervensi"),
#             width=5
#           ),
#           mainPanel(
#             tags$div(id = 'satPlaceholder'),
#             width=7)
#           ),
#           title="Sunting Intervensi Satelit Akun"
#         ))
#       ,
#       size="l",
#       easyClose = FALSE)
#   )
#   
# })
# 
# observeEvent(input$econHit, {
#   #browser()
#   insertUI(selector=paste0("#", "FDPlaceholder"),
#            where='afterEnd',
#            ui= uiOutput('FDUIManual')
#   )
# })  
# 
# output$FDUIManual<- renderUI({
#   tagList(pickerInput("pilihtahunFD",
#                       label="pilih tahun", selected = loadFileRDS()$tahunAwal,
#                       choices=c(loadFileRDS()$tahunAwal : loadFileRDS()$tahunAkhir),options = list(`actions-box` = TRUE),multiple = T),
#           tags$br(),
#           actionButton('showYearEco', 'tampilkan tabel'),
#           tags$br(),
#           tags$br(),
#           tags$div(id = 'SuntingPlaceHolder')
#   )
# })
# 
# observeEvent(input$showYearEco, {
#   insertUI(selector='#SuntingPlaceHolder',
#            where='afterEnd',
#            ui= uiOutput('SuntingUITable')
#   )
# }) 
# 
# output$SuntingUITable<- renderUI({
#   tagList(
#     tags$b('Sunting secara manual'),
#     tags$br(),
#     rHandsontableOutput('editFD'),
#     tags$br(),
#     actionButton('saveModalFD',' simpan tabel '),
#     tags$div(id = 'objDownloadFD')
#   )
# })
# 
# 
# 
# observeEvent(input$satHit, {
#   insertUI(selector= paste0("#", "satPlaceholder"),
#            where='afterEnd',
#            ui= uiOutput('satMUIManual')
#   )
# })
# 
# output$satMUIManual<- renderUI({
#   tagList(selectInput("pilihtahunSat",
#                       label="pilih tahun", selected = loadFileRDS()$tahunAwal,
#                       choices=c(loadFileRDS()$tahunAwal:loadFileRDS()$tahunAkhir)),
#           pickerInput("pilihBahanBakar",
#                       label="pilih faktor emisi",selected = data$faktorEmisi[1],
#                       choices=data$faktorEmisi,options = list(`actions-box` = TRUE),multiple = T),
#           tags$br(),
#           tags$br(),
#           tags$b('Sunting secara manual'),
#           tags$br(),
#           tags$br(),
#           rHandsontableOutput('editSat'),
#           tags$br(),
#           actionButton('saveModalSat', 'simpan tabel'),
#           tags$br(),
#           tags$br(),
#           tags$div(id='teksSatSave')
#   )
# })
# # observeEvent(input$select_button,{
# #   showModal(modalDialog(sidebarLayout(sidebarPanel(
# #     fluidRow(
# #       textInput("intervensiDef",
# #                 label = "Nama Skenario"
# #       ),
# #       selectInput("tahunAwal",
# #                   label = "Tahun Awal",
# #                   choices = c(2010:2030)),
# #       selectInput("tahunAkhir",
# #                   label = "Tahun Akhir",
# #                   choices = c(2010:2030),
# #                   selected = 2030),
# #       textAreaInput("deskripsi",
# #                     label = "Deskripsi Skenario",
# #                     width = "330px")
# #     ),
# #     tags$br(),
# #     actionButton("defHit","Tampilkan"),
# #     width=5
# #   ),
# #   mainPanel(
# #     tags$div(id = 'defPlaceholder'),
# #     width = 7
# #   )),
# #   title = "Deskripsi Skenario",
# #   footer = tagList(
# #     actionButton("closeModalDef", "Simpan"),
# #     actionButton("cancelModalDef", "Batal")
# #   ),
# #   size = "l",
# #   easyClose = FALSE
# #   ))
# # })