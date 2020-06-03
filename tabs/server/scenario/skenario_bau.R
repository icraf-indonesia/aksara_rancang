###*bau input####
generate_table<-function(table, first_year, second_year, value=0.05){
  n <- second_year-first_year
  eval(parse(text=(paste0("table$y", first_year, " <- value"))))
  for(i in 1:n){
    eval(parse(text=(paste0("table$y", first_year+i, " <- value"))))
  }
  table
}

observeEvent(input$generateBAUTable, {
  allDataProv$growthRate <- data.frame(Lapangan_usaha=as.character(allDataProv$ioSector[,1])) # reset table
  allDataProv$growthRate <- generate_table(allDataProv$growthRate, as.numeric(input$initialYear), as.numeric(input$finalYear))
  recordActivities(paste0("Membuat tabel proyeksi BAU tahun ", input$initialYear, "-", input$finalYear), "Berhasil", paste0(Sys.time()))
  notif_id <<- showNotification("Tabel berhasil dimuat", duration = 4, closeButton = TRUE, type = "warning")
})

output$tableBAUType <- renderRHandsontable({
  rhandsontable(allDataProv$growthRate, fixedColumnsLeft=1, height=640) %>% hot_cols(format="0%") # load table
})

observeEvent(input$saveTableBauType, {
  # if(input$typeIntervention=='Tipe 1'){
  #   allDataProv$growthRate <- generate_table(allDataProv$growthRate, as.numeric(input$initialYear), as.numeric(input$finalYear), value=as.numeric(input$gdpRate/100))
  # } 
  if(input$typeIntervention=='Tipe 1'){
    column_year <- paste0("y", input$yearBAUInv)
    growthRate <- allDataProv$growthRate
    eval(parse(text=(paste0("growthRate$", column_year, "<-as.numeric(input$gdpRate/100)"))))
    
    allDataProv$growthRate<-growthRate
  } else {
    allDataProv$growthRate <- hot_to_r(input$tableBAUType)
  }
  # print(allDataProv$growthRate)
  recordActivities(paste0("Menyimpan tabel proyeksi BAU tahun ", input$initialYear, "-", input$finalYear), "Berhasil", paste0(Sys.time()))
  notif_id <<- showNotification("Tabel berhasil disimpan", duration = 4, closeButton = TRUE, type = "warning")
  # 
  # print(allDataProv$growthRate)
  # 
})

### start LAHAN #####
ListLDMButton_fun <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

### buat tabel daftar nama file LDM reaktif ###
LDMListTableReact <- reactive({
  data.frame(
    Nama_File = c("LDM historis", ldmRV$LDMListFile),
    Lihat_File = ListLDMButton_fun(actionButton, length(ldmRV$LDMListFile)+1,
                                   'button_',
                                   label = "Tampilkan",
                                   onclick = paste0('Shiny.onInputChange( \"select_button\" , this.id)')
    )
  )
})

### buat list options ###
output$LDMFileOptions<- renderUI({
  LDMListTableReact <- LDMListTableReact()
  namaFile<-as.character(LDMListTableReact$Nama_File)
  selectInput("LDMPropUse", "Pilih tabel LDM proporsi yang akan digunakan dalam perhitungan:", choices=namaFile)
})

### tampilkan tabel list file LDM###
output$LDMListTable <- renderDataTable({
  LDMListTableReact()
}, escape = FALSE)

### pilh nama file dan file yang akan ditampilkan###
LDMTableTampil <- eventReactive(input$select_button,{
  sec<-blackBoxInputs()
  LDMProp_his<-sec$LDMProp_his
  selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
  
  if(selectedRow==1){
    fileName = "LDM historis"
    selectedFile = LDMProp_his
    list<-list(fileName=fileName,
               selectedFile=selectedFile)
    print("kondisi1")
  } else if(selectedRow != 1){
    fileName<-LDMListTableReact()[selectedRow,1]
    selectedFile<-readRDS(paste0("LDMData/Prov/",fileName))   # ganti mas alfa
    list<-list(fileName=fileName, selectedFile=selectedFile)
    print("kondisi2")
  }
  list
})

### tampilkan UI tabel LDM yang dipilih ###
output$LDMTableTampilUI<-renderUI({
  LDMTableTampil<-LDMTableTampil()
  selectedFile<-LDMTableTampil$selectedFile
  fileName<-LDMTableTampil$fileName
  
  if(identical(fileName, "LDM historis")){
    tagList(tags$h3(paste0(fileName)),
            tags$br(),
            actionButton('modalLDMbutton', 'Sunting Tabel'),
            tags$br(),
            tags$br(),
            datatable(selectedFile, extensions = "FixedColumns", options=list(pageLength=20, scrollX=TRUE, scrollY="500px", fixedColumns=list(leftColumns=1)), rownames=TRUE, width = "100%"))
  } else {
    tagList(tags$br(),
            tags$br(),
            tags$h3(paste0(fileName)),
            tags$br(),
            actionButton('modalLDMbutton', 'Sunting Tabel'),
            actionButton('deleteLDMTable','Hapus Tabel'),
            tags$br(),
            tags$br(),
            datatable(selectedFile))
  }
})

#### tampilkan modal dialog modalLDM ketika sunting tabel dipilih ####
observeEvent(input$modalLDMbutton,{
  sec<-blackBoxInputs()
  LDMProp_his<-sec$LDMProp_his
  sector<-sec$sector[,1]
  colnamesLDM<-colnames(LDMProp_his)
  showModal(modalDialog(sidebarLayout(sidebarPanel(
    fluidRow(
      selectInput("tupla", label="jenis tutupan lahan", choices=colnamesLDM),
      selectInput("sektor", label="sektor", choices=sector)
    ),
    rHandsontableOutput('editLDM'),
    tags$br(),
    uiOutput('LDMButton'),
    textOutput("sunting"),
    width=5
  ),
  mainPanel(
    tags$div(id = 'LDMPlaceholder'),
    width=7)
  ),
  title="sunting LDM",
  footer= tagList(
    actionButton("saveLDMTable", "simpan tabel"),
    actionButton("closeModalLDM", "tutup")
  ),
  size="l",
  easyClose = FALSE
  ))
})

### tutup modal dialog ###
observeEvent(input$closeModalLDM,{
  removeModal()
})

### hapus file ###
observeEvent(input$deleteLDMTable, {
  LDMTableTampil<-LDMTableTampil()
  fileName<-LDMTableTampil$fileName
  file.remove(paste0("LDMData/Prov/", fileName))   # ganti mas alfa
  
})
###### modal dialog######

# hapus tampilan kolom jika memasukkan 
observeEvent(c(input$modalLDMbutton, input$tupla, input$sektor), {
  LDMTableTampil<-LDMTableTampil()
  selectedFile<-LDMTableTampil$selectedFile
  fileName<-LDMTableTampil$fileName
  
  removeUI(selector='#LDMUIManual')
  removeUI(selector='#LDMUINormal')
  
  if(is.null(LDMProp_new$tablo) & is.null(LDMProp_new$coba)){
    LDMProp_new$tablo <- selectedFile
  } else if(!is.null(LDMProp_new$tablo) & is.null(LDMProp_new$coba)){
    LDMProp_new$tablo<- selectedFile
  } else if (!is.null(LDMProp_new$coba)){
    LDMProp_new$tablo<-LDMProp_new$coba
  }
  
})

teksLDM<-reactiveValues(unedited=" ", edited="Total nilai kolom tutupan lahan tidak sama dengan 1")

valLDM<- reactive({
  table_show <- as.matrix(subset(LDMProp_new$tablo, rownames(LDMProp_new$tablo) %in% input$sektor, colnames(LDMProp_new$tablo) %in% input$tupla))
  table_show
})

output$editLDM <- renderRHandsontable({
  rhandsontable(valLDM(), rowHeaderWidth = 160,) %>% hot_cols(format=3)
})

output$LDMButton<-renderUI({
  if (is.null(valLDM())){
    return(NULL)
  } else if (!is.null(valLDM)){
    tagList(actionButton('LDMButtonEdit','simpan hasil sunting'),
            tags$div(id='NormOrMan')
    )
  }
})

#### masukkan nilai sel baru ke dalam kolom tupla 
LDM_fun<-eventReactive(input$LDMButtonEdit,{
  tablo = LDMProp_new$tablo
  LDM_sel<-as.data.frame(hot_to_r(input$editLDM))
  LDM_sel<-as.numeric(LDM_sel[1,1])
  # inputSektor<-as.character(input$sektor)
  # inputTupla<-as.character(input$tupla)
  inputSektor<-input$sektor
  inputTupla<-input$tupla
  tablo[inputSektor, inputTupla]<-LDM_sel
  tablo<-as.data.frame(tablo)
  totLDMProp<-as.data.frame(colSums(tablo))
  totLDMProp_sel<-totLDMProp[inputTupla,]
  LDM_list<-list(LDM_sel=LDM_sel,
                 totLDMProp_sel=totLDMProp_sel,
                 totLDMProp=totLDMProp,
                 tablo = tablo,
                 inputTupla=inputTupla
  )
  LDM_list
})

observe({ 
  tes <- LDM_fun()
  inputTupla<-tes$inputTupla
  
  if(tes$totLDMProp_sel==1) {
    insertUI(selector='#NormOrMan',
             where= 'afterEnd',
             ui= tags$div(id='pesanHitungLDMNo', "tidak ada perubahan")
    )
  } else {
    removeUI('#pesanHitungLDMNo')
    insertUI(selector='#NormOrMan',
             where = 'afterEnd',
             ui= tags$div(id='pesanHitungLDMYes',
                          tagList(tags$br(),
                                  paste0("Jumlah total kolom ", inputTupla," tidak sama dengan 1"),
                                  tags$br(),
                                  tags$br(),
                                  radioButtons('LDMhit',
                                               'Pilih perhitungan yang akan dilakukan',
                                               choiceNames = c('normalisasi','hitung manual'),
                                               choiceValues = c('normal','manual'),
                                               selected = character(0))
                          )
                          
             )
    )
  }
})

### visualisasi kolom baru yang sudah diisi nilai sel yang diganti 
LDMPropKol_0 <- reactive({
  tes<-LDM_fun()
  inputTupla<-tes$inputTupla
  tablo<-tes$tablo
  # tablo<-LDMProp_new$tablo
  LDMProp_kol<-as.matrix(tablo[,inputTupla])
  kolsum<-as.matrix(colSums(LDMProp_kol))
  LDMProp_kol<-rbind(LDMProp_kol,kolsum)
  row.names(LDMProp_kol)<-c(row.names(tablo),"total")
  colnames(LDMProp_kol)<-inputTupla
  LDMProp_kol
})

observeEvent(input$LDMButtonEdit, {
  tabel$manualSave<-LDMPropKol_0()
  removeUI(selector='#LDMUIManual')
  removeUI(selector='#LDMUINormal')
})

observeEvent(input$LDMhit, {
  if(input$LDMhit=='normal'){
    removeUI(selector='#LDMUIManual')
    insertUI(selector='#LDMPlaceholder',
             where='afterEnd',
             ui= uiOutput('LDMUINormal')
    )
  }
  else if (input$LDMhit=='manual'){
    removeUI(selector='#LDMUINormal')
    insertUI(selector='#LDMPlaceholder',
             where='afterEnd',
             ui= uiOutput('LDMUIManual')
    )
  }
})

##### untuk edit manual satu kolom tupla #####
output$LDMUIManual<- renderUI({
  tagList(tags$b('Sunting secara manual'),
          tags$br(),
          tags$br(),
          rHandsontableOutput('LDMKolManualTable'),
          tags$br(),
          actionButton('saveLDMPropManual', 'Simpan kolom'),
          tags$br(),
          tags$div(id='teksLDMManual')
  )
})

observeEvent(input$LDMKolManualTable$changes$changes,{
  tes<-LDM_fun()
  inputTupla<-tes$inputTupla
  tablo<-tes$tablo
  
  LDMPropKol_1 <- as.data.frame(hot_to_r(input$LDMKolManualTable))
  row.names(LDMPropKol_1)<-c(row.names(tablo),"total")
  colnames(LDMPropKol_1)<-inputTupla
  LDMPropKol_1[nrow(LDMPropKol_1),1]<- sum(LDMPropKol_1[1:nrow(LDMPropKol_1)-1,1])
  tabel$manualSave<-LDMPropKol_1
  
  removeUI(selector='#teksManual')
  
})

output$LDMKolManualTable<-renderRHandsontable({
  rhandsontable(tabel$manualSave,
                rowHeaderWidth = 220,
                height=420,
                fixedRowsBottom=1
  )
})

observeEvent(input$saveLDMPropManual,{
  tes<-LDM_fun()
  tablo = tes$tablo
  inputTupla = tes$inputTupla
  kolom<-as.data.frame(hot_to_r(input$LDMKolManualTable))
  total = kolom[nrow(kolom),1]
  
  if(total != 1){
    insertUI(selector='#teksLDMManual', 
             where = 'afterEnd',
             ui = tags$div (id ='teksManual', "Kolom tidak dapat disimpan. Nilai total tidak sama dengan 1."))
  } else {
    kolomMinSum <- kolom[1:nrow(kolom)-1,]
    tablo[,inputTupla]<-kolomMinSum
    LDMProp_new$coba<-tablo
    insertUI(selector='#teksLDMManual',
             where='afterEnd',
             ui= tags$div (id='teksManual',"Kolom berhasil disimpan. Silakan melanjutkan penyuntingan kolom lain.")
    )
    removeUI(selector='#pesanHitungLDMNo')
    removeUI(selector='#pesanHitungLDMYes')
  }
})



##### untuk isi satu kolom dengan normalisasi #####
output$LDMUINormal<- renderUI({
  tagList(tags$b ('Hasil perhitungan normalisasi'),
          tags$br(),
          tags$br(),
          rHandsontableOutput('LDMKolNormalTable'),
          tags$br(),
          actionButton('saveLDMPropNormal', 'Simpan kolom'),
          tags$br(),
          tags$div(id='teksLDMNormal')
  )
})

normal_fun<-reactive({
  LDMPropKol_0<-LDMPropKol_0()
  before<-as.matrix(LDMPropKol_0[1:nrow(LDMPropKol_0)-1,])
  sum<-matrix(data=LDMPropKol_0[nrow(LDMPropKol_0),], nrow=nrow(before), ncol=1)
  normal<- before/sum
  normal_sum<-sum(normal)
  normal<-rbind(normal,normal_sum)
  rownames(normal)<-rownames(LDMPropKol_0)
  colnames(normal)<-colnames(LDMPropKol_0)
  normal
  
})

output$LDMKolNormalTable<-renderRHandsontable({
  rhandsontable(normal_fun(), 
                rowHeaderWidth = 220,
                height=420, 
                readOnly = TRUE, 
                fixedRowsBottom=1
  )
})

observeEvent(input$saveLDMPropNormal,{
  tes<-LDM_fun()
  tablo = tes$tablo
  inputTupla = tes$inputTupla
  kolom <- as.data.frame(hot_to_r(input$LDMKolNormalTable))
  kolomMinSum <- kolom[1:nrow(kolom)-1,]
  tablo[,inputTupla]<-kolomMinSum
  LDMProp_new$coba<-tablo
  insertUI(selector='#teksLDMNormal',
           where='afterEnd',
           ui= tags$div (id='teksNormal',"Kolom berhasil disimpan. Silakan melanjutkan penyuntingan kolom lain.")
  )
  removeUI(selector='#pesanHitungLDMNo')
  removeUI(selector='#pesanHitungLDMYes')
  
})


##### simpan tabel LDM ke dalam folder ####
observeEvent(input$saveLDMTable,{
  waktuLDM<-Sys.time()
  simpanLDM<-gsub(" ","_",waktuLDM,fixed = TRUE)
  simpanLDM<-gsub(":","-",simpanLDM,fixed = TRUE)
  tanggalLDM<-Sys.Date()
  namafileLDM<-paste0("username","_","Prov","_",simpanLDM) # ganti mas alfa
  saveRDS(LDMProp_new$coba, file = paste0('LDMData/Prov/',namafileLDM)) #ganti mas alfa
  ldmRV$LDMListFile<-list.files(paste0("LDMData/Prov")) #ganti mas alfa
  ldmRV$LDMTotFile<-length(list.files("LDMData/Prov")) # ganti mas alfa
  removeUI(selector='#pesanHitungLDMNo')
  removeUI(selector='#pesanHitungLDMYes')
  removeUI(selector='#LDMUIManual')
  removeUI(selector='#LDMUINormal')
})

###### select tipe proyeksi BAU yang diinginkan 
observeEvent(input$selectProjType,{
  if(input$selectProjType=="Proyeksi BAU berdasarkan pertumbuhan ekonomi"){
    removeUI(selector='#projTypeLandUI')
    insertUI(selector='#inputProjType',
             where='afterEnd',
             ui= uiOutput('projTypeEconomyUI'))
  } else {
    removeUI(selector='#projTypeEconomyUI')
    insertUI(selector='#inputProjType',
             where = 'afterEnd',
             ui=uiOutput('projTypeLandUI'))
    
  }
})

observeEvent(input$buttonBAU,{
  # removeUI(selector='#gdpRateUI')
  insertUI(selector='#inputProjType',
           where='afterEnd',
           ui= uiOutput('resultUI'))
})

output$projTypeEconomyUI<-renderUI(
  tagList(
    fluidRow(
      column(2, 
        selectInput("typeIntervention", "Tipe Intervensi", choices = c("Tipe 1", "Tipe 2"))
      ),
      column(2, 
        selectInput("initialYear", "Tahun awal:", choices = 1990:2100, selected=2016)
      ),
      column(2, 
        selectInput("finalYear", "Tahun akhir:", choices = 1990:2100, selected=2030)
      ),
      column(6, 
        style = "padding-top: 55px;",
        actionButton("generateBAUTable", "Buat Tabel")
      )
    )
    # fileInput("populationTable", "Tabel Populasi per Tahun", buttonLabel="Browse...", placeholder="No file selected"),
    # fileInput("emissionSectorRADTable", "Tabel Emisi Sumber Lain", buttonLabel="Browse...", placeholder="No file selected"),
    # hr()
    # actionButton("showResult", "Tampilkan")
  )
)

# observeEvent(input$showResult, {
#   tagList(uiOutput("resultUI"))
# })

output$resultUI<-renderUI(
  tagList(
    h3("Hasil Analisis"),
    fluidRow(
      column(2,
        selectInput("bauResults",
          label="Pilih output yang ingin ditampilkan",
          choices=c("Proyeksi PDRB",
                  "Proyeksi Upah per Kapita",
                  "Proyeksi Upah Gaji",
                  "Proyeksi Tenaga Kerja",
                  "Proyeksi Konsumsi Energi",
                  "Proyeksi Emisi Terkait Konsumsi Energi",
                  "Proyeksi Buangan Limbah",
                  "Proyeksi Emisi Terkait Buangan Limbah",
                  "Proyeksi Total Emisi",
                  "Proyeksi Intensitas Emisi",
                  "Proyeksi Tutupan Lahan",
                  "Proyeksi Emisi Terkait Tutupan Lahan"
          )
        )     
      ),
      column(2,
        conditionalPanel(
          condition="input.bauResults!='Proyeksi Upah per Kapita' & input.bauResults!='Proyeksi Total Emisi'",
          uiOutput("yearSelection")
        )
      )
    ),
    br(),
    tags$div(id='bauplaceholder'),
    plotlyOutput("plotlyResultsBAU"),
    br(),
    fluidRow(
      column(width=12,
        box(width=NULL,
          dataTableOutput('tableResultsBAU'),
          downloadButton('downloadTableBAU', 'Download Table (.csv)')
        )
      )
    )
  )
)

output$projTypeLandUI<-renderUI(
  tagList(selectInput("lahanResults",
                      label="Pilih output sektor lahan yang ingin ditampilkan",
                      choices=c("Proyeksi Output",
                                "Proyeksi PDRB",
                                "Proyeksi Income",
                                "Proyeksi Profit",
                                "Proyeksi Pajak",
                                "Proyeksi Impor",
                                "Proyeksi Ekspor",
                                "Proyeksi Belanja Pemerintah",
                                "Proyeksi Belanja Rumah Tangga",
                                "Proyeksi Tenaga Kerja",
                                "Proyeksi Neraca Perdagangan"
                      )
  )
  )
)
### end LAHAN ####


observeEvent(input$buttonBAU, {
  if(debugMode){
    sec <- blackBoxInputs()
    baselineEmission <- sec$baselineEmission
    populationProjection <- sec$populationProjection
  } else {
    sec <- allInputs()
    inPopTable <- input$populationTable
    if(is.null(inPopTable))
      return(NULL)
    
    inEmOtherTable <- input$emissionSectorRADTable
    if(is.null(inEmOtherTable))
      return(NULL)
    
    populationProjection <- read.table(inPopTable$datapath, header=TRUE, sep=",")
    baselineEmission <- read.table(inEmOtherTable$datapath, header=TRUE, sep=",")
    
  }
  ioSector <- sec$ioSector
  ioIntermediateDemand <- sec$ioIntermediateDemand
  ioFinalDemand <- sec$ioFinalDemand
  ioAddedValue <- sec$ioAddedValue
  ioFinalDemandComponent <- sec$ioFinalDemandComponent
  ioAddedValueComponent <- sec$ioAddedValueComponent
  satelliteLabour <- sec$satelliteLabour
  satelliteEnergy <- sec$satelliteEnergy
  emissionFactorEnergy <- sec$emissionFactorEnergy
  satelliteWaste <-sec$satelliteWaste
  emissionFactorWaste <- sec$emissionFactorWaste
  satelliteAgriculture <- sec$satelliteAgriculture
  emissionFactorAgriculture <- sec$emissionFactorAgriculture
  growthRate <- allDataProv$growthRate
  
  LU_tahun<-sec$LU_tahun
  GDPAll<-sec$GDPAll
  tahun<-sec$tahun
  
  # tin cek dulu LDMProp-nya !
  ###### gunakan LDMProp yang ditentukan di menu sebelumnya #####
  if (input$LDMPropUse=="LDM historis"){
    LDMProp=sec$LDMProp_his
  } else {
    LDMProp = readRDS(paste0("LDMData/Prov/",input$LDMPropUse))  #ganti mas alfa
  }
  
  ###BEGIN: BAU projection####
  
  # Series of GPD & Output projection
  # output$sectorSelection <- renderUI({
  #   if(debugMode){
  #     sec <- blackBoxInputs()
  #   } else {
  #     sec <- allInputs()
  #   }
  #   analysisResult <- sec$result
  #   selectInput("selectedSector", "Sektor", "Pilih sektor", choices=as.character(analysisResult$Sektor))
  # })
  # browser()
  
  ###BEGIN : Define function ####
  sec <- blackBoxInputs()
  
  #1 Function for ...
  functionSatelliteImpact <- function(type = "energy", satellite = data.frame(), matrix_output = matrix(), emission_factor = data.frame()) { 
    impact <- list()
    
    # impact$consumption
    impact$consumption <- satellite
    
    # calculate the proportion
    if(type != "labour"){
      proportionConsumption <- impact$consumption[, 4:ncol(impact$consumption)] / impact$consumption[, 3]
      impact$consumption[, 4:ncol(impact$consumption)] <- proportionConsumption
    }
    
    # calculate the coefficient & the new total satellite consumption 
    coefficientConsumption <- as.matrix(impact$consumption[,3]) / ioTotalOutput
    impact$consumption[,3] <- coefficientConsumption * matrix_output
    
    # calculate emission
    if(type != "labour"){
      
      colnames(impact$consumption)[3] <- "Tconsumption"
      
      # get the new satellite consumption for each sector
      # total consumption * proportion
      impact$consumption[,4:ncol(impact$consumption)] <- impact$consumption[,4:ncol(impact$consumption)] * impact$consumption[, 3]
      
      # checking the order of factor emission 
      orderEnergyType <- names(impact$consumption)[4:ncol(impact$consumption)]
      emissionFactor <- numeric()
      for(m in 1:length(orderEnergyType)){
        emissionFactor <- c(emissionFactor, emission_factor[which(emission_factor[,1]==orderEnergyType[m]), 2])
      }
      emissionFactor <- diag(emissionFactor, nrow = length(emissionFactor), ncol = length(emissionFactor))
      
      # impact$emission
      impact$emission <- impact$consumption
      impact$emission[,4:ncol(impact$emission)] <- as.matrix(impact$consumption[,4:ncol(impact$consumption)]) %*% emissionFactor
      impact$emission[,3] <- rowSums(impact$emission[,4: ncol(impact$emission)])
      impact$emission[is.na(impact$emission)] <- 0
      colnames(impact$emission)[3] <- "Temission"
    } 
    
    impact$consumption[is.na(impact$consumption)] <- 0
    return(impact)
    
  }
  
  #2 Function for calculating Land Requirement Coefficient, Land Requirement, & land Cover
  functionSatelliteLand1<-function(type=NULL, 
                                   matrix_output=NULL, 
                                   advanceMode = FALSE,
                                   currYear= NULL,
                                   runNum = NULL, # input for advanceMode = FALSE, runNUm 1 to 2
                                   LRCRate= NULL){ # input for advanceMode = TRUE, LRCRate sebagai reactive value yang by default diisi LRC historis 
    
    impact<-list()
    if(type=="historis"){
      impact$LRC <- sec$analysisLRC ###panggil dari blackbox input
      impact$landReq <- sec$landReq_his
      impact$landCover <- sec$landCover_his
    } else{
      # browser()
      if(advanceMode== TRUE){
        impact$LRC<-sec$analysisLRC*LRCRate^(currYear-sec$ioPeriod)
      } else{
        if (runNum == 1 ){
          impact$LRC<-sec$analysisLRC*(sec$LRCRate_his^(currYear-sec$ioPeriod))
        } else if (runNum ==2 ){
          impact$LRC<-sec$analysisLRC*(sec$LRCRate_2^(currYear-sec$ioPeriod))
        }
      }
      # Land Requirement
      impact$landReq<-diag(impact$LRC[,1]) %*% rbind(as.matrix(matrix_output[,1]),0)
      impact$landReq[nrow(as.matrix(impact$landReq)),]<-sum(sec$landCover_his[,1])-sum(as.matrix(impact$landReq[1:nrow(as.matrix(impact$landReq))-1,]))
      # Land Cover
      impact$landCover<-sec$LDMProp_sektor %*% as.matrix(impact$landReq)
      rownames(impact$landCover)<-colnames(sec$LDMProp_his)
      
    }
    
    # Rapikan
    impact$landReq <- data.frame(c(rownames(sec$ioSector), nrow(sec$ioSector)+1),
                                 c(as.character(sec$ioSector[,1]), "lainnya (tidak menghasilkan output)"),
                                 impact$landReq, stringsAsFactors = FALSE)
    
    colnames(impact$landReq)<-c("id.sector", "sector", "land.requirement")
    
    
    impact$landCover <- data.frame(as.character(1:23),
                                   colnames(sec$LDMProp_his),
                                   impact$landCover[,1],stringsAsFactors=FALSE)
    colnames(impact$landCover)<-c("id.land.use", "land.use", "luas.land.use")
    
    
    return(impact)
  }
  
  #3 Function for calculating LUTM
  # Land cover yang dihasilkan pada fungsi ini adalah land cover proyeksi + input land cover skenario
  functionSatelliteLand2<- function(type=NULL,
                                    landCoverProjection = NULL,  #proyeksi land cover BAU atau skenario
                                    landCoverProjectionMin=NULL,
                                    inputLandCover=NULL,  #perubahan land cover skenario aksi
                                    LUTMTemplate=NULL,
                                    advanceMode=FALSE, 
                                    percentage=NULL, # input parameter jika advanceMode=TRUE 
                                    runNum=NULL, # input parameter jika advanceMode=FALSE
                                    additionalG=NULL, 
                                    additionalH=NULL,
                                    additionalE=NULL, 
                                    additionalF=NULL,
                                    LUTMChange=NULL,
                                    GDP=NULL,
                                    carbonStock=carbonStock_his){
    
    impact<- list()
    
    if (type=="historis"){
      impact$landCover<-sec$landCover_his
      # impact$matrixE<-NULL
      # impact$matrixF<-NULL
      # impact$matrixG<-NULL
      # impact$matrixH<-NULL
      impact$LUTM<-sec$LUTM_his
      
    } else{
      
      # set multiiplier for making matrix H
      if(advanceMode==TRUE){
        multiplier <- matrix(percentage, nrow=ncol(sec$TPM), ncol=1)
      } else {
        if(runNum==1){ multiplier = 0.8
        } else if (runNum ==2) {multiplier <- 0.5
        } else if (runNum == 3) {multiplier <- 0.3
        } else if (runNum == 4) {multiplier <- 0.1
        } else if (runNum==5) {multiplier <- 0
        } else if (runNum==6) {
          multiplier <- 0.1
          LUTMTemplate <- matrix(NA, nrow=nrow(sec$LUTMTemplate_his),ncol=ncol(sec$LUTMTemplate_his))
          rownames(LUTMTemplate)<-rownames(sec$LUTMTemplate_his)
          colnames(LUTMTemplate)<-colnames(sec$LUTMTemplate_his)
          for (i in 1:nrow(sec$landCover_his)){
            if (sum(sec$landCover_his[i,])==0){
              LUTMTemplate[i,]<-matrix(0,ncol=ncol(sec$LUTMTemplate_his)) #LUTMTemplate bisa diedit di interface
              LUTMTemplate[,i]<-matrix(0,nrow=ncol(sec$LUTMTemplate_his))
            } else {}
          }
          # LUTMTemplate<-read.csv("_TIN/data/JaBar/LUTMTemplate_his2.csv", header=TRUE)
          LUTMTemplate[is.na(LUTMTemplate)]<-paste0("x",1:length(LUTMTemplate[is.na(LUTMTemplate)]))
        }
      }
      
      # landCover 
      if(!is.null(inputLandCover)){
        impact$landCover<-as.matrix(landCoverProjection)+as.matrix(inputLandCover)
      } else{
        impact$landCover<-as.matrix(landCoverProjection)
      }
      
      # LUTM Template
      jumlahVariabel<-length(LUTMTemplate[LUTMTemplate!=0])
      namaVariabel<-paste0("x",1:length(LUTMTemplate[LUTMTemplate!=0]))
      LUTMTemplate[LUTMTemplate!=0]<-namaVariabel
      
      # matrix E
      impact$matrixE<-matrix(NA,nrow = 46, ncol = jumlahVariabel)
      colnames(impact$matrixE)<-namaVariabel
      variabel_x<-list()
      variabel_y<-list()
      for (a in 1:nrow(LUTMTemplate)){  ## constraint 1
        variabel_x[[a]]<-t(LUTMTemplate[a,])[t(LUTMTemplate[a,])!= 0]
        eval(parse(text=paste0("variabel_x_",a,"<-NULL")))
        eval(parse(text=paste0("variabel_x_",a,"<-variabel_x[[",a,"]]")))
        for (i in 1:length(variabel_x[[a]])){
          if(!identical(variabel_x[[a]],c(numeric(0), character(0),integer(0)))){
            eval(parse(text=paste0("impact$matrixE[",a,",paste0(variabel_x_",a,"[",i,"])]<-1")))
            # impact$matrixE[a,paste0(variabel_n[i])]<-1
          } else {impact$matrixE[a,]<-0}
        }
      }
      for (a in 1:ncol(LUTMTemplate)){  ##constraint 2
        variabel_y[[a]]<-t(LUTMTemplate[,a])[t(LUTMTemplate[,a])!= 0]
        eval(parse(text=paste0("variabel_y_",a,"<-NULL")))
        eval(parse(text=paste0("variabel_y_",a,"<-variabel_y[[",a,"]]")))
        for (i in 1:length(variabel_y[[a]])){
          if(!identical(variabel_y[[a]],c(numeric(0), character(0), integer(0)))){
            eval(parse(text=paste0("impact$matrixE[(23+",a,"),paste0(variabel_y_",a,"[",i,"])]<-1")))
            # impact$matrixE[a,paste0(variabel_n[i])]<-1
          } else {impact$matrixE[(23+a),]<-0}
        }
      }
      impact$matrixE[is.na(impact$matrixE)]<-0
      impact$matrixE<-impact$matrixE[(!(rbind(as.matrix(impact$landCover[,1]),as.matrix(impact$landCover[,1]))) == 0),]  #hapus constraint untuk tupla yg jumlahnya 0 agar compatible saat perhitungan LSEI
      if (is.null(additionalE)){
        impact$matrixE<-impact$matrixE
      } else{
        impact$matrixE<- rbind(impact$matrixE, as.matrix(additionalE))
      }
      
      # matrix F
      impact$matrixF<-rbind(landCoverProjectionMin,as.matrix(impact$landCover))
      impact$matrixF<- as.matrix(impact$matrixF[!(rowSums(impact$matrixF) == 0),])
      if (is.null(additionalF)){
        impact$matrixF<-impact$matrixF
      } else{
        impact$matrixF<- rbind(impact$matrixF, as.matrix(additionalF))
      }
      
      # check all diagonal variable names
      diagVariable<-matrix(NA, ncol=1, nrow=ncol(LUTMTemplate))
      for (i in 1:ncol(LUTMTemplate)){
        diagVariable[i,1]<-LUTMTemplate[i,i]
      }
      diagVariable<-diagVariable[!(diagVariable==0),]
      
      # matrix G
      impact$matrixG<-rbind(diag(nrow=(jumlahVariabel)), matrix(0, nrow=length(diagVariable),ncol=jumlahVariabel))  ## buat matrix G constraint 1 & 2
      colnames(impact$matrixG)<-namaVariabel
      for (i in 1:length(diagVariable)){
        impact$matrixG[jumlahVariabel+i,diagVariable[i]]<-1   #assign 1 untuk semua variabel diagonal
      }
      if (is.null(additionalG)){
        impact$matrixG<-impact$matrixG
      } else{
        impact$matrixG<- rbind(impact$matrixG, as.matrix(additionalG))
      }
      
      # get TPM value for each diagonal variable
      diagTPM<-matrix(NA, ncol=1, nrow=ncol(sec$TPM))
      for (i in 1:ncol(sec$TPM)){
        diagTPM[i,1]<-sec$TPM[i,i]
      }
      diagTPM<-as.matrix(diagTPM[!(diagTPM==0),])
      
      # matrix H
      diagTPM <- diagTPM*multiplier 
      impact$matrixH<-rbind(matrix(0,nrow=jumlahVariabel,ncol=1),as.matrix(diagTPM*landCoverProjectionMin[sec$landCover_his!=0]))
      
      if (is.null(additionalH)){
        impact$matrixH<-impact$matrixH
      } else{
        impact$matrixH<- rbind(impact$matrixH, as.matrix(additionalH))
      }
      
      # LUTM dengan metode LSEI
      variabelLSEI<-lsei(E = impact$matrixE, F = impact$matrixF, G=impact$matrixG, H=impact$matrixH)
      variabelLSEI<-as.matrix(unlist(variabelLSEI[["X"]]))
      variabelLSEI<-as.matrix(as.numeric(variabelLSEI[1:jumlahVariabel,]))
      row.names(variabelLSEI)<-namaVariabel
      impact$LUTM<-as.matrix(LUTMTemplate)
      # impact$LUTM<-matrix(ncol=ncol(LUTMTemplate), nrow=nrow(LUTMTemplate))
      # colnames(impact$LUTM)<-colnames(LUTMTemplate)
      # colnames(impact$LUTM)<-rownames(LUTMTemplate)
      for (a in 1:nrow(impact$LUTM)){
        for(b in 1:ncol(impact$LUTM)){
          if (impact$LUTM[a,b]==0){
            impact$LUTM[a,b]<-as.numeric(0)
          } else {impact$LUTM[a,b]<-as.numeric(variabelLSEI[paste0(LUTMTemplate[a,b]),1])
          }
        }
      }
      class(impact$LUTM)<-"numeric"
      if (!is.null(LUTMChange)){
        impact$LUTM<- as.matrix(impact$LUTM)+as.matrix(LUTMChange)
      }
      
    }
    
    # emission
    impact$emission<-matrix(NA,nrow=nrow(as.matrix(impact$LUTM)), ncol=ncol(as.matrix(impact$LUTM)))
    for (a in 1:nrow(impact$LUTM)){
      for (b in 1:ncol(impact$LUTM)){
        impact$emission[a,b]<-as.numeric(impact$LUTM[a,b])*(sec$carbonStock_his[b,]-sec$carbonStock_his[a,])*3.67*(-1)
      }
    }
    
    impact$emission<-matrix(sum(impact$emission),nrow=nrow(GDP))
    impact$emission<-impact$emission *GDP/sum(GDP)
    
    
    # rapikan
    impact$landCover <- data.frame(as.character(1:23),
                                   colnames(sec$LDMProp_his),
                                   impact$landCover[,1],stringsAsFactors=FALSE)
    colnames(impact$landCover)<-c("id.land.use", "land.use", "luas.land.use")
    
    impact$LUTM <- data.frame(as.character(1:23),
                              colnames(sec$LDMProp_his),
                              impact$LUTM,stringsAsFactors=FALSE)
    colnames(impact$LUTM)<-c("id.land.use", "land.use", colnames(sec$LDMProp_his))
    
    impact$emission <- data.frame(rownames(allDataProv$ioSector),
                                  as.character(allDataProv$ioSector[,1]),
                                  impact$emission,stringsAsFactors=FALSE)
    colnames(impact$emission)<-c("id.sector", "sector", "emission")
    
    
    return(impact)
  }
  
  #4 Function for calculating LUTM new LUTMTemplate, additional matrix G, additional matrix H, & delta land Cover (inputLandCover)
  functionSatelliteLand3<-function (inputLandScen = NULL,
                                    timeScen = timeStep){
    impact<-list()
    
    if (is.null(inputLandScen)){
      impact$LUTMTemplate<-LUTMTemplate_his
      impact$additionalG<-NULL
      impact$additionalH<-NULL
      impact$inputLandCover<-NULL
    } else{
      # calculate scenario LUTM Template
      impact$LUTMTemplate<-LUTMTemplate_his
      impact$LUTMTemplate[impact$LUTMTemplate!="0"]<-NA
      rownames(impact$LUTMTemplate)<-colnames(impact$LUTMTemplate)
      for (i in 1:nrow(inputLandScen)){
        impact$LUTMTemplate[paste0(inputLandScen[i,1]), paste0(inputLandScen[i,2])]<- NA
      }
      impact$LUTMTemplate[is.na(impact$LUTMTemplate)]<-paste0("x",1:length(impact$LUTMTemplate[is.na(impact$LUTMTemplate)]))
      
      # additional G & additional H
      impact$additionalG<-matrix(0,ncol=length(impact$LUTMTemplate[impact$LUTMTemplate!=0]), nrow=nrow(inputLandScen))
      impact$additionalH<-matrix(ncol=1, nrow=nrow(inputLandScen))
      
      colnames(impact$additionalG)<-as.character(impact$LUTMTemplate[impact$LUTMTemplate!=0])
      
      for (i in 1:nrow(inputLandScen)){
        impact$additionalG[i,impact$LUTMTemplate[paste0(inputLandScen[i,1]), paste0(inputLandScen[i,2])]]<-1
        impact$additionalH[i,1]<-inputLandScen[i,paste0(timeScen)]
      }
      
      # inputLandCover
      impact$inputLandCover<- matrix(0,ncol=1, nrow=23)
      rownames(impact$inputLandCover)<-colnames(impact$LUTMTemplate)
      
      for (landCoverClass in unique(inputLandScen[,2])){
        impact$inputLandCover[paste(landCoverClass),]<-sum(inputLandScen[inputLandScen[,2]==paste(landCoverClass), timeScen]) # pertambahan luas <- positif jumlah total luas kelas tupla yang sama di tahun akhir
      } 
      
      for (landCoverClass in as.character(unique(inputLandScen[,1]))){
        impact$inputLandCover[landCoverClass,]<--sum(inputLandScen[inputLandScen[,1]==paste(landCoverClass), timeScen]) # penurunan luas <- negatif jumlah total luas kelas tupla yang sama tahun akhir
      } 
    }
    return (impact)
  }
  ###END : Define function ####
  
  analysisResult <- sec$result
  
  rowImport <- 1
  rowIncome <- 2
  rowProfit <- 3

  initialYear <- as.numeric(input$initialYear)
  finalYear <- as.numeric(input$finalYear)
  iteration <- finalYear - initialYear
  
  bauSeriesOfGDP <- data.frame(Sektor = allDataProv$ioSector[,1], stringsAsFactors = FALSE)
  bauSeriesOfGDP$y2015 <- analysisResult$analysisGDP
  
  # Final Demand
  matrixIoFinalDemand <- as.matrix(sec$ioFinalDemand)
  rowSumsMatrixIoFinalDemand <- as.matrix(rowSums(matrixIoFinalDemand))
  bauSeriesOfFinalDemand <- rowSumsMatrixIoFinalDemand
  
  # Total Output
  matrixIoIntermediateDemand <- as.matrix(sec$ioIntermediateDemand)
  colSumsMatrixIoIntermediateDemand <- colSums(matrixIoIntermediateDemand)
  matrixIoAddedValue <- as.matrix(sec$ioAddedValue)
  colSumsMatrixIoAddedValue <- colSums(matrixIoAddedValue)
  ioTotalOutput <- colSumsMatrixIoIntermediateDemand + colSumsMatrixIoAddedValue 
  bauSeriesOfOutput <- ioTotalOutput
  
  # Series of Intervention Point
  bauSeriesOfIntermediateDemand <- list()
  bauSeriesOfAddedValue <- list()
  bauSeriesOfFinalDemandComponent <- list()
  bauSeriesOfImpactLabour <- list()
  bauSeriesOfImpactEnergy <- list()
  bauSeriesOfImpactWaste <- list()
  bauSeriesOfImpactAgriculture <- list()
  bauSeriesOfImpactLand1<-list() ###perlu dipanggil landCover <- bauSeriesOfImpactLand1$landCover
  bauSeriesOfImpactLand2<-list() ###perlu dipanggil jadiin reactive value: bauResult$bauSeriesOfImpactLand2 <- bauSeriesOfImpactLand2
  ###untuk dipanggil: bauSeriesOfImpactLand2 <- bauResult$bauSeriesOfImpactLand2
  
  # Historical consumption and emission data
  eval(parse(text=paste0("bauSeriesOfIntermediateDemand$y",sec$ioPeriod," <- matrixIoIntermediateDemand")))
  eval(parse(text=paste0("bauSeriesOfAddedValue$y",sec$ioPeriod," <- matrixIoAddedValue")))
  eval(parse(text=paste0("bauSeriesOfFinalDemandComponent$y",sec$ioPeriod," <- matrixIoFinalDemand")))
  eval(parse(text=paste0("bauSeriesOfImpactLabour$y",sec$ioPeriod," <- functionSatelliteImpact('labour', satellite = satelliteLabour, matrix_output = as.matrix(ioTotalOutput))")))
  eval(parse(text=paste0("bauSeriesOfImpactEnergy$y",sec$ioPeriod,"<- functionSatelliteImpact('energy', satellite = satelliteEnergy, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorEnergy)")))
  eval(parse(text=paste0("bauSeriesOfImpactWaste$y",sec$ioPeriod," <- functionSatelliteImpact('waste', satellite = satelliteWaste, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorWaste)")))
  eval(parse(text=paste0("bauSeriesOfImpactAgriculture$y",sec$ioPeriod,"<- functionSatelliteImpact('agriculture', satellite = satelliteAgriculture, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorAgriculture)")))
  
  # historical LRC, land requirement, & land cover 
  eval(parse(text=paste0("bauSeriesOfImpactLand1$y",sec$ioPeriod,"<-functionSatelliteLand1(type= 'historis', matrix_output= as.matrix(ioTotalOutput))")))
  # LSEI
  eval(parse(text=paste0("bauSeriesOfImpactLand2$y",sec$ioPeriod," <- functionSatelliteLand2(type='historis',carbonStock=carbonStock_his, GDP= as.matrix(bauSeriesOfGDP$y",sec$ioPeriod,") )")))
  
  growthRateSeries <- growthRate
  growthRateSeries$Lapangan_usaha <- NULL
  growthRateSeries <- as.matrix(1+growthRateSeries)
  
  projectionYear <- initialYear
  listYear <- paste0("y", sec$ioPeriod)
  
  # browser()
  # economic & impact (energy, waste, & agriculture projection 
  for(step in 1:(iteration+1)){
    projectionFinalDemand <- growthRateSeries[, step] * bauSeriesOfFinalDemand[, step]
    bauSeriesOfFinalDemand <- cbind(bauSeriesOfFinalDemand, projectionFinalDemand)
    projectionOutput <- allDataProv$ioLeontiefInverse %*% projectionFinalDemand 
    bauSeriesOfOutput <- cbind(bauSeriesOfOutput, projectionOutput)
    
    # notes on the year
    timeStep <- paste0("y", projectionYear)
    
    # add additional values to the list
    eval(parse(text=paste0("bauSeriesOfFinalDemandComponent$", timeStep, " <- as.matrix(sec$proportionFinalDemand*projectionFinalDemand)"))) # contains NaN
    eval(parse(text=paste0("bauSeriesOfIntermediateDemand$", timeStep, " <-  sec$analysisCT %*% diag(as.vector(projectionOutput), ncol = sec$ioDimention, nrow= sec$ioDimention)")))
    eval(parse(text=paste0("bauSeriesOfAddedValue$", timeStep, " <-  sec$analysisCPI %*% diag(as.vector(projectionOutput), ncol = sec$ioDimention, nrow= sec$ioDimention)")))
    
    # GDP projection 
    eval(parse(text = paste0("bauSeriesOfGDP$", timeStep, "<- colSums(bauSeriesOfAddedValue$", timeStep, "[setdiff(1:nrow(matrixIoAddedValue), rowImport),])")))
    
    # Impact projection
    eval(parse(text= paste0("bauSeriesOfImpactLabour$", timeStep, " <- functionSatelliteImpact('labour', satellite = satelliteLabour, matrix_output = as.matrix(projectionOutput))")))
    eval(parse(text= paste0("bauSeriesOfImpactEnergy$", timeStep, " <- functionSatelliteImpact('energy', satellite = satelliteEnergy, matrix_output = as.matrix(projectionOutput), emission_factor = emissionFactorEnergy)")))
    eval(parse(text= paste0("bauSeriesOfImpactWaste$", timeStep, " <- functionSatelliteImpact('waste', satellite = satelliteWaste, matrix_output = as.matrix(projectionOutput), emission_factor = emissionFactorWaste)")))
    eval(parse(text= paste0("bauSeriesOfImpactAgriculture$", timeStep, " <- functionSatelliteImpact('agriculture', satellite = satelliteAgriculture, matrix_output = as.matrix(projectionOutput), emission_factor = emissionFactorAgriculture)")))
    
    listYear <- c(listYear, timeStep)
    projectionYear <- initialYear+step
    
  }
  
  colnames(bauSeriesOfOutput) <- as.character(listYear)
  colnames(bauSeriesOfFinalDemand)<- as.character(listYear)
  
  ioSector <- allDataProv$ioSector
  bauSeriesOfFinalDemandTable <- cbind(data.frame(ioSector$V1), bauSeriesOfFinalDemand)
  colnames(bauSeriesOfFinalDemandTable) <- c("Sektor", as.character(listYear)) 
  
  # land cover projection 
  # non-advance mode
  for (i in 1:2){
    projectionYear <- initialYear
    listYear <- paste0("y", sec$ioPeriod)
    for(step in 1:(iteration+1)){
      # notes on the year
      timeStep <- paste0("y", projectionYear)
      # projection
      eval(parse(text= paste0("bauSeriesOfImpactLand1$", timeStep, " <- functionSatelliteLand1(type= 'projection', 
                                                                                          matrix_output= as.matrix(bauSeriesOfOutput[,'",timeStep,"']), 
                                                                                          advanceMode = FALSE,
                                                                                          currYear=projectionYear,
                                                                                          runNum = ",i,", # input for advanceMode = FALSE
                                                                                          LRCRate= NULL)")))
      listYear <- c(listYear, timeStep)
      projectionYear <- initialYear+step
    } 
    # jika tidak ada value landCover yang negatif, break loop
    if(any(unlist(sapply(bauSeriesOfImpactLand1,'[[', "landCover"))<0)==FALSE){  
      if(i==1){
        textDataLRCRate="historis"
      } else {
        textDataLRCRate="historis yang dimodifikasi" 
      }
      print(paste0("laju perubahan LRC yang digunakan untuk membangun proyeksi tutupan lahan adalah data laju LRC ", textDataLRCRate)) # use as UI textoutput 
      break
    } else {
      if(i==2){
        print("proyeksi luas tutupan lahan yang dihasilkan bernilai negatif. Silakan masukkan data laju perubahan LRC secara manual")
      }
    }
  }
  
  # jika masih ada value landCover yang negatif, force to enter advanceMode pada UI
  if(any(unlist(sapply(bauSeriesOfImpactLand1,'[[', "landCover"))<0)==TRUE){
    repeat{
      # insert UI here to request for new inputLRCRate 
      inputLRCRate<-LRCRate_2  
      projectionYear <- initialYear
      listYear <- paste0("y", ioPeriod)
      for(step in 1:(iteration+1)){
        # notes on the year
        timeStep <- paste0("y", projectionYear)
        eval(parse(text= paste0("bauSeriesOfImpactLand1$", timeStep, " <- functionSatelliteLand1(type= 'projection', 
                                                                                          matrix_output= as.matrix(bauSeriesOfOutput[,'",timeStep,"']), 
                                                                                          advanceMode = TRUE,
                                                                                          runNum = NULL,
                                                                                          LRCRate= inputLRCRate)")))
        listYear <- c(listYear, timeStep)
        projectionYear <- initialYear+step
      }  
      # jika tidak ada value landCover yang negatif, break loop
      if(any(unlist(sapply(bauSeriesOfImpactLand1,'[[', "landCover"))<0)==FALSE){ 
        print("laju perubahan LRC yang digunakan untuk membangun proyeksi tutupan lahan adalah data laju LRC yang telah Anda modifikasi") # use as UI textoutput 
        break
      } else {
        print("proyeksi tutupan lahan yang dihasilkan memiliki luasan negatif. Silakan menyunting ulang laju perubahan LRC dan atau kembali ke target permintaan akhir") # use as UI textoutput 
      }
    }
  }
  
  # LUTM Projection 
  projectionYear <- initialYear
  listYear <- paste0("y", sec$ioPeriod)
  
  for(step in 1:(iteration+1)){
    for (i in 1:6){   # 6 tipe yg akan dirun otomatis
      timeStep <- paste0("y", projectionYear)
      eval(parse(text=paste0(
        "bauSeriesOfImpactLand2$",timeStep,"<-tryCatch({
    functionSatelliteLand2 (type ='projected',
                            landCoverProjection = as.matrix(bauSeriesOfImpactLand1[['",timeStep,"']][['landCover']][['luas.land.use']]) ,
                            landCoverProjectionMin=  as.matrix(bauSeriesOfImpactLand1[[paste0('y',",projectionYear,"-1)]][['landCover']][['luas.land.use']]),
                            LUTMTemplate = sec$LUTMTemplate_his, 
                            advanceMode = FALSE,
                            runNum =",i," , 
                            GDP=as.matrix(bauSeriesOfGDP$",timeStep,")
    )
  }, warning = function (a){NA}, error = function(b){NA})"
      )))
      if(any(is.na(bauSeriesOfImpactLand2[[timeStep]]))==FALSE){
        print(paste0("use constraint ", i ," to make LUTM ",timeStep))
        break
      } else {
        if(i==6){
          print(paste0("tidak berhasil menghitung LUTM ",timeStep))
        } 
      }
    }
    listYear <- c(listYear, timeStep)
    projectionYear <- initialYear+step
  }
  
  # jika tidak berhasil menghitung LUTM, force to enter advanceMode pada UI (spt pada land cover)
  
  #####END : BAU projection ####
  
  #####BEGIN : BAU projection visualization ####
  # 1. GDP (ind. 1)
  resultGDP <- data.frame(year = 0, sector = "", category="", GDP = 0, stringsAsFactors = FALSE)
  # resultGDP <- data.frame(year = 0, id.sector = 0, sector = "", GDP = 0, stringsAsFactors = FALSE)
  for(c in 2:ncol(bauSeriesOfGDP)){
    add.row <- cbind(allDataProv$ioSector, bauSeriesOfGDP[, c])
    names(add.row) <- c("sector", "category", "GDP")
    add.row$year <- initialYear + (c-3)
    add.row <- add.row[, colnames(resultGDP)]
    resultGDP <- data.frame(rbind(resultGDP, add.row), stringsAsFactors = FALSE)
  }
  resultGDP <- resultGDP[resultGDP$year != 0, ] # remove initial values
  
  # 2. Income per capita (ind. 9)
  resultIncomePerCapita <- data.frame(year = 0, Income.per.capita = 0)
  for(t in 0:iteration){
    t_curr <- initialYear + t
    pop_curr <- sec$populationProjection[which(sec$populationProjection[, 1] == t_curr), 2]
    inc_curr <- sum(bauSeriesOfAddedValue[[t+2]][rowIncome,])
    inc_capita <- inc_curr/pop_curr
    add.row <- data.frame(cbind(t_curr, inc_capita))
    names(add.row) <- names(resultIncomePerCapita)
    resultIncomePerCapita <- data.frame(rbind(resultIncomePerCapita, add.row), stringsAsFactors = FALSE)
  }
  resultIncomePerCapita <- resultIncomePerCapita[resultIncomePerCapita$year != 0, ]
  
  # 3. Wages or Income (ind. 7)
  resultIncome <- data.frame(year = 0, sector= "", income = 0, stringsAsFactors = FALSE)
  sc.name <- allDataProv$ioSector[,1]
  for(t in 0:iteration){
    t_curr <- initialYear + t
    inc_curr <- data.frame(bauSeriesOfAddedValue[[t+2]][rowIncome,])
    add.row <- data.frame(cbind(t_curr, sc.name, inc_curr), stringsAsFactors = FALSE)
    names(add.row) <- names(resultIncome)
    resultIncome <- data.frame(rbind(resultIncome, add.row), stringsAsFactors = FALSE)
  }
  resultIncome <- resultIncome[resultIncome$year != 0, ]
  
  # 4. Labour (ind. number 10)
  resultLabour <- data.frame(year = 0, id.sector = 0, sector= "", labour = 0, stringsAsFactors = FALSE)
  for(t in 0:iteration){
    t_curr <- initialYear + t
    add.row <- data.frame(bauSeriesOfImpactLabour[[t+2]][[1]])
    names(add.row) <- names(resultLabour)[2:4]
    add.row$year <- t_curr
    add.row <- add.row[, names(resultLabour)]
    resultLabour <- data.frame(rbind(resultLabour, add.row), stringsAsFactors = FALSE)
  }
  resultLabour <- resultLabour[resultLabour$year != 0, ]
  
  # 5. Energy cons (indicator number 2)
  resultEnergyConsumption <- bauSeriesOfImpactEnergy[[2]][[1]]
  resultEnergyConsumption$year <- initialYear
  resultEnergyConsumption <- resultEnergyConsumption[, c("year", names(bauSeriesOfImpactEnergy[[2]][[1]]))]
  for(t in 1:iteration){
    t_curr <- initialYear + t
    add.row <- data.frame(bauSeriesOfImpactEnergy[[t+2]][[1]]) # [[2]] for emission
    add.row$year <- t_curr
    add.row <- add.row[, names(resultEnergyConsumption)]
    resultEnergyConsumption <- data.frame(rbind(resultEnergyConsumption, add.row), stringsAsFactors = FALSE)
  }
  names(resultEnergyConsumption)[2:3] <- c("id.sector", "sector")
  
  # 6. Energy emission (indicator number 3)
  resultEnergyEmission <- bauSeriesOfImpactEnergy[[2]][[2]]
  resultEnergyEmission$year <- initialYear
  resultEnergyEmission <- resultEnergyEmission[, c("year", names(bauSeriesOfImpactEnergy[[2]][[2]]))]
  for(t in 1:iteration){
    t_curr <- initialYear + t
    add.row <- data.frame(bauSeriesOfImpactEnergy[[t+2]][[2]]) # [[2]] for emission
    add.row$year <- t_curr
    add.row <- add.row[, names(resultEnergyEmission)]
    resultEnergyEmission <- data.frame(rbind(resultEnergyEmission, add.row), stringsAsFactors = FALSE)
  }
  names(resultEnergyEmission)[2:3] <- c("id.sector", "sector")
  
  # 7. Waste cons (indicator number 2)
  resultWasteDisposal <- bauSeriesOfImpactWaste[[2]][[1]]
  resultWasteDisposal$year <- initialYear
  resultWasteDisposal <- resultWasteDisposal[, c("year", names(bauSeriesOfImpactWaste[[2]][[1]]))]
  for(t in 1:iteration){
    t_curr <- initialYear + t
    add.row <- data.frame(bauSeriesOfImpactWaste[[t+2]][[1]]) # [[2]] for emission
    add.row$year <- t_curr
    add.row <- add.row[, names(resultWasteDisposal)]
    resultWasteDisposal <- data.frame(rbind(resultWasteDisposal, add.row), stringsAsFactors = FALSE)
    
  }
  names(resultWasteDisposal)[2:3] <- c("id.sector", "sector")
  
  # 8. Waste emission (indicator number 3)
  resultWasteEmission <- bauSeriesOfImpactWaste[[2]][[2]]
  resultWasteEmission$year <- initialYear
  resultWasteEmission <- resultWasteEmission[, c("year", names(bauSeriesOfImpactWaste[[2]][[2]]))]
  for(t in 1:iteration){
    t_curr <- initialYear + t
    add.row <- data.frame(bauSeriesOfImpactWaste[[t+2]][[2]]) # [[2]] for emission
    add.row$year <- t_curr
    add.row <- add.row[, names(resultWasteEmission)]
    resultWasteEmission <- data.frame(rbind(resultWasteEmission, add.row), stringsAsFactors = FALSE)
  }
  names(resultWasteEmission)[2:3] <- c("id.sector", "sector")
  
  # 9. Fertilizer cons (indicator number 2)
  resultFertilizerUsed <- bauSeriesOfImpactAgriculture[[2]][[1]]
  resultFertilizerUsed$year <- initialYear
  resultFertilizerUsed <- resultFertilizerUsed[, c("year", names(bauSeriesOfImpactAgriculture[[2]][[1]]))]
  for(t in 1:iteration){
    t_curr <- initialYear + t
    add.row <- data.frame(bauSeriesOfImpactAgriculture[[t+2]][[1]]) # [[2]] for emission
    add.row$year <- t_curr
    add.row <- add.row[, names(resultFertilizerUsed)]
    resultFertilizerUsed <- data.frame(rbind(resultFertilizerUsed, add.row), stringsAsFactors = FALSE)
    
  }
  names(resultFertilizerUsed)[2:3] <- c("id.sector", "sector")
  
  # 10. Fertilizer emission (indicator number 3)
  resultFertilizerEmission <- bauSeriesOfImpactAgriculture[[2]][[2]]
  resultFertilizerEmission$year <- initialYear
  resultFertilizerEmission <- resultFertilizerEmission[, c("year", names(bauSeriesOfImpactAgriculture[[2]][[2]]))]
  for(t in 1:iteration){
    t_curr <- initialYear + t
    add.row <- data.frame(bauSeriesOfImpactAgriculture[[t+2]][[2]]) # [[2]] for emission
    add.row$year <- t_curr
    add.row <- add.row[, names(resultFertilizerEmission)]
    resultFertilizerEmission <- data.frame(rbind(resultFertilizerEmission, add.row), stringsAsFactors = FALSE)
  }
  names(resultFertilizerEmission)[2:3] <- c("id.sector", "sector")
  
  # browser()
  # 11. Land Requirement 
  resultLandReq <- bauSeriesOfImpactLand1[[2]][["landReq"]]
  resultLandReq$year <- initialYear
  resultLandReq <-resultLandReq[,c("year", names(bauSeriesOfImpactLand1[[2]][["landReq"]]))]
  for(t in 1:iteration){
    t_curr <- initialYear + t
    add.row <- data.frame(bauSeriesOfImpactLand1[[t+2]][["landReq"]])
    add.row$year <- t_curr
    add.row <- add.row[,names(resultLandReq)]
    resultLandReq <- data.frame(rbind(resultLandReq, add.row), stringsAsFactors = FALSE)
  }
  
  # 12. Land Cover
  resultLandCover <- bauSeriesOfImpactLand2[[2]][["landCover"]]
  resultLandCover$year <- initialYear
  resultLandCover <-resultLandCover[,c("year", names(bauSeriesOfImpactLand2[[2]][["landCover"]]))]
  for(t in 1:iteration){
    t_curr <- initialYear + t
    add.row <- data.frame(bauSeriesOfImpactLand2[[t+2]][["landCover"]])
    add.row$year <- t_curr
    add.row <- add.row[,names(resultLandCover)]
    resultLandCover <- data.frame(rbind(resultLandCover, add.row), stringsAsFactors = FALSE)
  }
  
  # 13. LUTM
  resultLUTM <- bauSeriesOfImpactLand2[[2]][["LUTM"]]
  resultLUTM$year <- initialYear
  resultLUTM <-resultLUTM[,c("year", names(bauSeriesOfImpactLand2[[2]][["LUTM"]]))]
  for(t in 1:iteration){
    t_curr <- initialYear + t
    add.row <- data.frame(bauSeriesOfImpactLand2[[t+2]][["LUTM"]])
    add.row$year <- t_curr
    add.row <- add.row[,names(resultLUTM)]
    resultLUTM <- data.frame(rbind(resultLUTM, add.row), stringsAsFactors = FALSE)
  }
  
  # 14. Land Emission by sector 
  resultLandEmission <- bauSeriesOfImpactLand2[[2]][["emission"]]
  resultLandEmission$year <- initialYear
  resultLandEmission <-resultLandEmission[,c("year", names(bauSeriesOfImpactLand2[[2]][["emission"]]))]
  for(t in 1:iteration){
    t_curr <- initialYear + t
    add.row <- data.frame(bauSeriesOfImpactLand2[[t+2]][["emission"]])
    add.row$year <- t_curr
    add.row <- add.row[,names(resultLandEmission)]
    resultLandEmission <- data.frame(rbind(resultLandEmission, add.row), stringsAsFactors = FALSE)
  } 
  
  # 15. Total Emission
  # resultTotalEmission <- baselineEmission[which(baselineEmission$Year>=initialYear & baselineEmission$Year<= finalYear),]
  resultTotalEmission <- data.frame(Year=initialYear:finalYear)
  emissionEnergyConsumption <- numeric()
  emissionWasteDisposal <- numeric()
  emissionFertilizer <- numeric()
  emissionLand <- numeric()
  for(t in 0:iteration){
    t_curr <- initialYear + t
    add_MEcons <- sum(resultEnergyEmission[resultEnergyEmission$year==t_curr, "Temission"])
    add_MWdisp <- sum(resultWasteEmission[resultWasteEmission$year==t_curr, "Temission"])
    add_MF <- sum(resultFertilizerEmission[resultFertilizerEmission$year==t_curr, "Temission"])
    add_MLand <-sum(resultLandEmission[resultLandEmission$year==t_curr, "emission"])
    emissionEnergyConsumption <- c(emissionEnergyConsumption, add_MEcons)
    emissionWasteDisposal <- c(emissionWasteDisposal, add_MWdisp)
    emissionFertilizer <- c(emissionFertilizer, add_MF)
    emissionLand<-c(emissionLand, add_MLand)
  }
  resultTotalEmission$emissionEnergyCons <- emissionEnergyConsumption
  resultTotalEmission$emissionWasteDisp <- emissionWasteDisposal
  resultTotalEmission$emissionFert <- emissionFertilizer
  resultTotalEmission$emissionLand <-emissionLand
  resultTotalEmission$TotalEmission <- rowSums(resultTotalEmission[, 2:ncol(resultTotalEmission)])
  resultTotalEmission$CummulativeEmission <- cumsum(resultTotalEmission$TotalEmission)
  
  # 16. BAU emission[economic sector, years]
  bauSeriesOfEmissionBySector <- data.frame(Sektor=allDataProv$ioSector[,1], Kategori=allDataProv$ioSector[,2])
  for(t in 0:iteration){
    t_curr <- initialYear + t
    add_MEcons <- resultEnergyEmission[resultEnergyEmission$year==t_curr, "Temission"]
    add_MWdisp <- resultWasteEmission[resultWasteEmission$year==t_curr, "Temission"]
    add_MF <- resultFertilizerEmission[resultFertilizerEmission$year==t_curr, "Temission"]
    add_MLand <- resultLandEmission[c(resultLandEmission$year==t_curr & resultLandEmission$sector != "lainnya (tidak menghasilkan output"), "emission"]
    eval(parse(text=paste0("bauSeriesOfEmissionBySector$y", t_curr, " <- add_MEcons + add_MWdisp + add_MF + add_MLand")))
  }
  
  #browser()
  # resultTotalGDP <- colSums(bauSeriesOfGDP[,2:(ncol(bauSeriesOfGDP)-1)])
  bauAllResult <- subset(resultTotalEmission, select=c(Year, TotalEmission, CummulativeEmission))
  # bauAllResult <- cbind(bauAllResult, resultTotalGDP)
  # bauAllResult$ResultTotalGDP<-colSums(bauSeriesOfGDP[,2:(ncol(bauSeriesOfGDP)-1)])
  bauAllResult$ResultTotalGDP<-colSums(bauSeriesOfGDP[,which(colnames(bauSeriesOfGDP)==paste0("y",initialYear)):ncol(bauSeriesOfGDP)])
  bauAllResult$CummulativeGDP <- cumsum(bauAllResult$ResultTotalGDP)
  bauAllResult$EmissionIntensity <- bauAllResult$TotalEmission / bauAllResult$ResultTotalGDP
  bauAllResult$CummulativeEmissionIntensity <-cumsum(bauAllResult$EmissionIntensity)
  
  
  ggplot(data=bauAllResult, aes(x=Year, y=TotalEmission, group=1)) + geom_line() + geom_point()
  ggplot(data=bauAllResult, aes(x=Year, y=CummulativeEmission, group=1)) + geom_line() + geom_point()
  ggplot(data=bauAllResult, aes(x=Year, y=EmissionIntensity, group=1)) + geom_line() + geom_point()
  ggplot(data=bauAllResult, aes(x=Year, y=ResultTotalGDP, group=1)) + geom_line() + geom_point()
  ggplot(data=bauAllResult, aes(x=Year, y=CummulativeGDP, group=1)) + geom_line() + geom_point()
  ggplot(data=bauAllResult, aes(x=Year, y=CummulativeEmissionIntensity, group=1)) + geom_line() + geom_point()
  
  #browser()
  #####END : BAU projection visualization #### 
  
  recordActivities("Simulasi skenario BAU", "Berhasil", paste0(Sys.time()))
  notif_id <<- showNotification("Simulasi skenario bisnis seperti biasa telah berhasil", duration = 4, closeButton = TRUE, type = "warning")
  
  bauResults$populationProjection = sec$populationProjection ###dipakai yg kanan
  bauResults$baselineEmission = sec$baselineEmission ###kiri diganti menjadi nama variable yg kanan
  bauResults$resultGDP = resultGDP
  bauResults$resultIncomePerCapita = resultIncomePerCapita
  bauResults$resultIncome = resultIncome
  bauResults$resultLabour = resultLabour
  bauResults$resultEnergyConsumption = resultEnergyConsumption
  bauResults$resultEnergyEmission = resultEnergyEmission
  bauResults$resultWasteDisposal = resultWasteDisposal
  bauResults$resultWasteEmission = resultWasteEmission
  bauResults$resultTotalEmission = resultTotalEmission
  bauResults$bauSeriesOfImpactLabour = bauSeriesOfImpactLabour
  bauResults$bauSeriesOfImpactEnergy = bauSeriesOfImpactEnergy
  bauResults$bauSeriesOfImpactWaste = bauSeriesOfImpactWaste
  bauResults$bauSeriesOfGDP = bauSeriesOfGDP
  bauResults$bauSeriesOfOutput = bauSeriesOfOutput
  bauResults$bauSeriesOfFinalDemandComponent = bauSeriesOfFinalDemandComponent
  bauResults$bauSeriesOfIntermediateDemand = bauSeriesOfIntermediateDemand
  bauResults$bauSeriesOfAddedValue = bauSeriesOfAddedValue
  bauResults$growthRateSeries = growthRateSeries
  bauResults$finalYear = finalYear
  bauResults$initialYear = initialYear
  bauResults$resultLandCover = resultLandCover
  bauResults$bauSeriesOfImpactLand1 = bauSeriesOfImpactLand1
  bauResults$bauSeriesOfImpactLand2 = bauSeriesOfImpactLand2
  bauResults$bauSeriesOfFinalDemandTable = bauSeriesOfFinalDemandTable
  bauResults$bauSeriesOfImpactAgriculture = bauSeriesOfImpactAgriculture 
  bauResults$bauSeriesOfFinalDemand = bauSeriesOfFinalDemand
  bauResults$bauAllResult = bauAllResult
  # bauResults$landCover_t1=landCover_t1
  # bauResults$landCover_t1_years=landCover_t1_years
  bauResults$resultFertilizerUsed = resultFertilizerUsed #pertanian
  bauResults$resultFertilizerEmission = resultFertilizerEmission #pertanian
  bauResults$bauSeriesOfImpactAgriculture = bauSeriesOfImpactAgriculture #pertanian
  bauResults$resultLandReq = resultLandReq
  bauResults$bauAllResult = bauAllResult
  bauResults$resultLandEmission = resultLandEmission
  
  updateTabItems(session, "tabs", selected = "intScenario")
})

#### BEGIN : input BAU sektor lahan, edit tabel land cover ====
landCoverTable_0 <- reactive({
  sec <- blackBoxInputs()
  LU_tahun<-sec$LU_tahun
  tahun<-sec$tahun
  colnames(LU_tahun)<-tahun
  sum<-colSums(LU_tahun)
  as.data.frame(rbind(LU_tahun, sum), row.names=c(colnames(allDataProv$LDMProp_his), "total luas"))
})

landCoverTable_fun <- reactive ({
  if(is.null(input$inputBAULahanLandCover)){return(landCoverTable_0())}
  else if (!identical(landCoverTable_0(), input$inputBAULahanLandCover)){
    landCoverTable_1 <- as.data.frame(hot_to_r(input$inputBAULahanLandCover))
    landCoverTable_1[nrow(landCoverTable_1),]<-colSums(landCoverTable_1[1:nrow(landCoverTable_1)-1,])
    landCoverTable_1
  }
})


output$inputBAULahanLandCover <- renderRHandsontable({
  rhandsontable(landCoverTable_fun(),
                # fixedColumnsLeft=1,
                fixedRowsBottom=1,
                height=640,
                rowHeaderWidth = 180
                # )%>% hot_col("sektor", readOnly = TRUE,colWidths=180, worldWrap=TRUE)
  )
})

observeEvent(input$saveInputBAULahanLandCover,{
  tabLandCover<-hot_to_r(input$inputBAULahanLandCover)
  tabLandCoverMinSum<-tabLandCover[1:nrow(allDataProv$LU_tahun),1:ncol(allDataProv$LU_tahun)]
  editable$BAULahan_landCover<-tabLandCoverMinSum
  notif_id <<- showNotification("Tabel berhasil disimpan", duration = 4, closeButton = TRUE, type = "warning")
})

#### END : input BAU sektor lahan, edit tabel land cover ====

#### BEGIN : all inputs BAU sektor lahan====
allInputsBAULahan <- eventReactive(input$buttonBAULahan, {
  if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
  
  GDPAll<-sec$GDPAll
  # landTable_t0<-sec$landTable_t0
  landReq<-sec$landReq
  analysisResult <- sec$result
  analysisML <- analysisResult$analysisML
  ioSector <- sec$ioSector
  kategori <- sec$kategori
  ioIntermediateDemand <- sec$ioIntermediateDemand
  ioFinalDemand <- sec$ioFinalDemand
  ioAddedValue <- sec$ioAddedValue
  ioFinalDemandComponent <- sec$ioFinalDemandComponent
  ioAddedValueComponent <- sec$ioAddedValueComponent
  satelliteLabour <- sec$satelliteLabour
  satelliteEnergy <- sec$satelliteEnergy
  emissionFactorEnergy <- sec$emissionFactorEnergy
  satelliteWaste <-sec$satelliteWaste
  emissionFactorWaste <- sec$emissionFactorWaste
  growthRate <- allDataProv$growthRate
  tahun<-sec$tahun
  # GDPAll<-sec$GDPAll
  # LDMProp<-allDataProv$LDMProp
  analysisCL<-analysisResult$analysisCL
  ioLeontiefInverse<-sec$ioLeontiefInverse
  
  ##### Gunakan input LDM yang sudah diedit 
  if (input$LDMPropUse=="LDM historis"){
    LDMProp=sec$LDMProp_his
  } else {
    LDMProp = readRDS(paste0("LDMData/Prov/",input$LDMPropUse))   # ganti mas alfa
  }
  
  
  ##### Gunakan input Land cover yang sudah diedit
  if(is.null(editable$BAULahan_landCover)){
    LU_tahun <- sec$LU_tahun
  }
  else {
    LU_tahun<-editable$BAULahan_landCover
  }
  
  ### Sektor Lahan: hitung LDM dalam satuan luas, LPC, LRC
  
  LU_tahun<-as.data.frame(LU_tahun)
  LU_tahun<-as.matrix(LU_tahun)
  LDMdimcol<-ncol(LDMProp)
  LDMdimrow<-nrow(LDMProp)
  LDMProp<-as.matrix(LDMProp)
  GDPAll<-as.data.frame(GDPAll)
  #proporis findem/output
  propFindemOutput<- as.matrix(rowSums(findem)/GDPAll[,4])
  
  diagLU <- list()
  landTable<-list()
  landReq<-matrix(nrow=nrow(LDMProp),ncol=ncol(LU_tahun))
  
  for (i in 1:ncol(LU_tahun)){
    diagLU[[i]]<-as.matrix(diag(LU_tahun[,i]))
    landTable[[i]]<-LDMProp%*%diagLU[[i]]
    landReq[,i]<-as.matrix(rowSums(landTable[[i]]))
  }
  
  LPC<-GDPAll[,4]/landReq[,1]
  LPC[is.infinite(LPC)]<-0
  LRC<-1/LPC
  LRC[is.infinite(LRC)]<-0
  landTable_t0<-cbind(sector,kategori, landTable[[1]],landReq[,1], LPC, LRC)
  colnames(landTable_t0)<-c("Sektor", "Kategori",colnames(LDMProp),"Total Kebutuhan Lahan", "LPC", "LRC")
  
  
  #### proyeksi output sektor lahan
  
  #generate tabel findem sektor u/ tiap tahun
  findemLahan_tahun<-matrix(nrow=nrow(landReq), ncol=ncol(landReq))
  for(x in 1:ncol(landReq)){
    findemLahan_tahun[,x]<-landReq[,x]*landTable_t0[,"LPC"]*propFindemOutput
  }
  
  findemLahan_tahun[is.na(findemLahan_tahun)]<-0
  
  # generate tabel output sektor u/ tiap tahun
  outputlahan_tahun<-matrix(nrow=nrow(findemLahan_tahun), ncol=ncol(findemLahan_tahun))
  for(x in 1:ncol(findemLahan_tahun)){
    outputlahan_tahun[,x]<-leontief %*% findemLahan_tahun[,x]
  }
  
  # generate tabel BAU output sektor lahan untuk ditampilkan di shiny
  colnames(outputlahan_tahun)<-tahun
  outputlahan<-data.frame(sector.id=1:nrow(outputlahan_tahun),sector=sector[,1], outputlahan_tahun)
  
  outputlahan_result<-data.frame(year=0, id.sector=0, sector="", Output=0)
  for (x in 3:ncol(outputlahan)){
    newtable<- outputlahan[,c(1,2,x)]
    names(newtable) <- c("id.sector", "sector", "Output")
    yearcol<-(str_extract_all(colnames(outputlahan), '[0-9]+'))
    newtable$year<-yearcol[x]
    newtable<-newtable[,colnames(outputlahan_result)]
    outputlahan_result<-data.frame(rbind(newtable, outputlahan_result))
  }
  
  outputlahan_table <- outputlahan_result[outputlahan_result$year != 0, ] # remove initial values
  
  ##### proyeksi lain2
  landProp<-as.data.frame(cbind(
    GDPAll$GDP/GDPAll$OUTPUT,
    t(addval[2,]/GDPAll$OUTPUT),
    t(addval[3,]/GDPAll$OUTPUT),
    t(addval[5,]/GDPAll$OUTPUT),
    t(addval[1,]/GDPAll$OUTPUT),
    findem[,5]/GDPAll$OUTPUT,
    findem[,2]/GDPAll$OUTPUT,
    findem[,1]/GDPAll$OUTPUT,
    labour_coef
  ))
  
  landProp[is.na(landProp)]<-0
  colnames(landProp)<-c("PDRB",
                        "income",
                        "profit",
                        "pajak",
                        "impor",
                        "ekspor", 
                        "belanja_pemerintah", 
                        "belanja_RT",
                        "labour"
  )
  landProp_name<-colnames(landProp)
  
  # generate tabel proyeksi BAU tiap indikator per tahun & sektor (landProp * outputlahan_tahun)
  for(b in 1:ncol(landProp)){
    eval(parse(text=(paste0("tabel_",b, "<- matrix(nrow=nrow(outputlahan_tahun), ncol=ncol(outputlahan_tahun))"))))
    eval(parse(text=(paste0("BAULahan_",b,"<- matrix(nrow=ncol(outputlahan_tahun), ncol=2)"))))
    for (c in 1:ncol(outputlahan_tahun)){
      #tabel indikator ekonomi per tahun & sektor
      eval(parse(text=paste0("tabel_",b,"[,c]<-landProp[,",b,"]*outputlahan_tahun[,c]")))
      eval(parse(text = paste0("tabel_",b,"[is.na(tabel_",b,")]<-0")))
      #tabel total indikator tiap tahun
      eval(parse(text= paste0("BAULahan_",b,"<-cbind(tahun,as.data.frame(colSums(tabel_",b,")))")))
      #tabel untuk ditampilkan di shiny
    }
    eval(parse(text=(paste0("colnames(BAULahan_",b,")=c('year',landProp_name[",b,"])"))))
    eval(parse(text=(paste0("colnames(tabel_",b,")<-tahun"))))
    eval(parse(text=(paste0("lahan_",b,"<-data.frame(sector.id=1:nrow(tabel_",b,"),sector=sector[,1], tabel_",b,")"))))
    eval(parse(text=(paste0('lahanResult_',b,'<-data.frame(year=0, id.sector=0, sector="",indikator=0)'))))
    eval(parse(text=(paste0("newtable_",b,"<-data.frame(id.sector=0,sector='', indikator=0, year=0)"))))
    
  }
  
  tabel_list <- list (tabel_1,
                      tabel_2,
                      tabel_3, 
                      tabel_4,
                      tabel_5,
                      tabel_6,
                      tabel_7,
                      tabel_8,
                      tabel_9
  )
  lahan_list <- list( lahan_1, 
                      lahan_2, 
                      lahan_3, 
                      lahan_4, 
                      lahan_5, 
                      lahan_6, 
                      lahan_7, 
                      lahan_8,
                      lahan_9
  )
  newtable_list<-list(newtable_1, 
                      newtable_2, 
                      newtable_3, 
                      newtable_4, 
                      newtable_5, 
                      newtable_6, 
                      newtable_7, 
                      newtable_8,
                      newtable_9
  )    
  lahanResult_list<-list(lahanResult_1,
                         lahanResult_2,
                         lahanResult_3,
                         lahanResult_4,
                         lahanResult_5,
                         lahanResult_6,
                         lahanResult_7,
                         lahanResult_8,
                         lahanResult_9
  )
  
  for (i in 1:length(tabel_list)){
    for(x in 3:ncol(lahan_list[[i]])){
      newtable_list[[i]]<- lahan_list[[i]][,c(1,2,x)]
      names(newtable_list[[i]]) <- c("id.sector", "sector", "indikator")
      yearcol<-(str_extract_all(colnames(outputlahan), '[0-9]+'))
      newtable_list[[i]]$year<-as.double(yearcol[x])
      newtable_list[[i]]<-newtable_list[[i]][,colnames(lahanResult_list[[i]])]
      lahanResult_list[[i]]<-data.frame(rbind(newtable_list[[i]], lahanResult_list[[i]]))
    }
    eval(parse(text=(paste0("lahanResult_",i,"<-matrix(unlist(lahanResult_list[[",i,"]]), byrow=TRUE)"))))
    eval(parse(text=(paste0("lahanResult_list[[",i,"]]<-as.matrix(lahanResult_list[[",i,"]], header=TRUE)"))))
    eval(parse(text=(paste0("lahanResult_",i,"<-matrix(unlist(lahanResult_list[[",i,"]]),ncol=4)"))))
    eval(parse(text=(paste0('colnames(lahanResult_',i,')<-c("year", "id.sector", "sector",landProp_name[',i,'])'))))
  }
  
  BAULahan_0<-as.data.frame(cbind(tahun,colSums(outputlahan_tahun)))
  colnames(BAULahan_0)<-c("year","output")
  
  listBAU_lahan<-list(BAULahan_0=BAULahan_0,
                      BAULahan_1=BAULahan_1,
                      BAULahan_2=BAULahan_2,
                      BAULahan_3=BAULahan_3,
                      BAULahan_4=BAULahan_4,
                      BAULahan_5=BAULahan_5,
                      BAULahan_6=BAULahan_6,
                      BAULahan_7=BAULahan_7,
                      BAULahan_8=BAULahan_8,
                      BAULahan_9=BAULahan_9,
                      lahanResult_0=outputlahan_result,
                      lahanResult_1=lahanResult_1,
                      lahanResult_2=lahanResult_2,
                      lahanResult_3=lahanResult_3,
                      lahanResult_4=lahanResult_4,
                      lahanResult_5=lahanResult_5,
                      lahanResult_6=lahanResult_6,
                      lahanResult_7=lahanResult_7,
                      lahanResult_8=lahanResult_8,
                      lahanResult_9=lahanResult_9,
                      tahun=tahun, 
                      landTable_t0=landTable_t0
  )
  listBAU_lahan
})
#### END : all inputs BAU sektor lahan ####

output$yearSelection <- renderUI({
  selectInput("selectedYear", "Tahun", "Pilih tahun", choices=c(input$initialYear:input$finalYear))
})

output$plotlyResultsBAU <- renderPlotly({
  resultGDP <- bauResults$resultGDP
  resultIncomePerCapita <- bauResults$resultIncomePerCapita  
  resultIncome <- bauResults$resultIncome
  resultLabour <- bauResults$resultLabour
  resultEnergyConsumption <- bauResults$resultEnergyConsumption 
  resultEnergyEmission <- bauResults$resultEnergyEmission 
  resultWasteDisposal <- bauResults$resultWasteDisposal  
  resultWasteEmission <- bauResults$resultWasteEmission 
  resultTotalEmission <- bauResults$resultTotalEmission
  # landCover_t1 <- bauResults$landCover_t1
  # landCover_t1_years <- bauResults$landCover_t1_years
  
  if(input$bauResults == "Proyeksi PDRB"){
    removeUI(selector = '#baupdrb')
    graph <- resultGDP[resultGDP$year==input$selectedYear,]
    GDPvalues <- as.matrix(graph$GDP)
    GDPTotal <- colSums(GDPvalues)
    insertUI(
      selector="#bauplaceholder",
      ui = tags$div(
        valueBox(format(GDPTotal, nsmall = 1, big.mark = ","), "Juta Rupiah", icon = icon("credit-card"), width = 8),
        id='baupdrb'
      )
    )
    # ggplot(data=graph, aes(x=sector, y=GDP)) + 
    #   geom_bar(colour="blue", stat="identity") + 
    #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
    GDP_all <- aggregate(x = resultGDP$GDP, by = list(resultGDP$year), FUN = sum)
    colnames(GDP_all) = c("year", "PDRB")
    gplot4<-ggplot(data=GDP_all, aes(x=year, y=PDRB, group=1)) + geom_line() + geom_point()
    ggplotly(gplot4)
    # plot_ly(GDP_all, x = ~year, y = ~PDRB, type = 'scatter', mode = 'lines')
    
  } else if(input$bauResults == "Proyeksi Upah per Kapita"){
    removeUI(selector = '#baupdrb')
    gplot5<-ggplot(data=resultIncomePerCapita, aes(x=year, y=Income.per.capita, group=1)) + geom_line() + geom_point()
    ggplotly(gplot5)
    
  } else if(input$bauResults == "Proyeksi Upah Gaji"){
    removeUI(selector = '#baupdrb')
    graph <- resultIncome[resultIncome$year==input$selectedYear,]
    # ggplot(data=graph, aes(x=sector, y=income)) +
    #   geom_bar(colour="blue", stat="identity") +
    #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
    income_all <- aggregate(x = resultIncome$income, by = list(resultIncome$year), FUN = sum)
    colnames(income_all) = c("year", "income")
    gplot6<-ggplot(data=income_all, aes(x=year, y=income, group=1)) + geom_line() + geom_point()
    ggplotly(gplot6)
    
  } else if(input$bauResults == "Proyeksi Tenaga Kerja"){
    removeUI(selector = '#baupdrb')
    graph <- resultLabour[resultLabour$year==input$selectedYear,]
    # ggplot(data=graph, aes(x=sector, y=labour)) +
    #   geom_bar(colour="blue", stat="identity") +
    #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
    labour_all <- aggregate(x = resultLabour$labour, by = list(resultLabour$year), FUN = sum)
    colnames(labour_all) = c("year", "Labour")
    gplot7<-ggplot(data=labour_all, aes(x=year, y=Labour, group=1)) + geom_line() + geom_point()
    ggplotly(gplot7)
    
    
  } else if(input$bauResults == "Proyeksi Konsumsi Energi"){
    removeUI(selector = '#baupdrb')
    graph <- resultEnergyConsumption[resultEnergyConsumption$year==input$selectedYear,]
    # ggplot(data=graph, aes(x=sector, y=Tconsumption)) +
    #   geom_bar(colour="blue", stat="identity") +
    #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
    energy_all <- aggregate(x = resultEnergyConsumption$Tconsumption, by = list(resultEnergyConsumption$year), FUN = sum)
    colnames(energy_all) = c("year", "Energy")
    gplot8<-ggplot(data=energy_all, aes(x=year, y=Energy, group=1)) + geom_line() + geom_point()
    ggplotly(gplot8)
    
  } else if(input$bauResults == "Proyeksi Emisi Terkait Konsumsi Energi"){
    removeUI(selector = '#baupdrb')
    graph <- resultEnergyEmission[resultEnergyEmission$year==input$selectedYear,]
    # ggplot(data=graph, aes(x=sector, y=Temission)) +
    #   geom_bar(colour="blue", stat="identity") +
    #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
    em_energy_all <- aggregate(x = resultEnergyEmission$Temission, by = list(resultEnergyEmission$year), FUN = sum)
    colnames(em_energy_all) = c("year", "EmEnergy")
    gplot9<-ggplot(data=em_energy_all, aes(x=year, y=EmEnergy, group=1)) + geom_line() + geom_point()
    ggplotly(gplot9)
    
  } else if(input$bauResults == "Proyeksi Buangan Limbah"){
    removeUI(selector = '#baupdrb')
    graph <- resultWasteDisposal[resultWasteDisposal$year==input$selectedYear,]
    # ggplot(data=graph, aes(x=sector, y=Tconsumption)) +
    #   geom_bar(colour="blue", stat="identity") +
    #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
    waste_all <- aggregate(x = resultWasteDisposal$Tconsumption, by = list(resultWasteDisposal$year), FUN = sum)
    colnames(waste_all) = c("year", "Waste")
    gplot10<-ggplot(data=waste_all, aes(x=year, y=Waste, group=1)) + geom_line() + geom_point()
    ggplotly(gplot10)
    
  } else if(input$bauResults == "Proyeksi Emisi Terkait Buangan Limbah"){
    removeUI(selector = '#baupdrb')
    graph <- resultWasteEmission[resultWasteEmission$year==input$selectedYear,]
    # ggplot(data=graph, aes(x=sector, y=Temission)) +
    #   geom_bar(colour="blue", stat="identity") +
    #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
    em_waste_all <- aggregate(x = resultWasteEmission$Temission, by = list(resultWasteEmission$year), FUN = sum)
    colnames(em_waste_all) = c("year", "EmWaste")
    gplot11<-ggplot(data=em_waste_all, aes(x=year, y=EmWaste, group=1)) + geom_line() + geom_point()
    ggplotly(gplot11)
    
  } else if(input$bauResults == "Proyeksi Total Emisi"){
    removeUI(selector = '#baupdrb')
    gplot12<-ggplot(data=resultTotalEmission[resultTotalEmission$Year > input$initialYear,], aes(x=Year, y=TotalEmission, group=1)) + geom_line() + geom_point()
    ggplotly(gplot12)
  } else if(input$bauResults == "Proyeksi Intensitas Emisi"){
    removeUI(selector = '#baupdrb')
    GDP_all <- aggregate(x = resultGDP$GDP, by = list(resultGDP$year), FUN = sum)
    colnames(GDP_all) = c("year", "PDRB")
    GDP_all$emisi <- resultTotalEmission$TotalEmission
    GDP_all$intensitas <-  GDP_all$emisi / GDP_all$PDRB 
    gplot13<-ggplot(data=GDP_all[GDP_all$year > input$initialYear,], aes(x=year, y=intensitas, group=1)) + geom_line() + geom_point()
    ggplotly(gplot13)
  } else if(input$bauResults=="Proyeksi Tutupan Lahan"){
    removeUI(selector='#baupdrb')
    landCoverData<-cbind(landCover_t1_years,colSums(landCover_t1))
    colnames(landCoverData)<-c("year", "Tutupan_Lahan")
    landCover_plot<-ggplot(data=landCoverData, aes(x=year, y=Tutupan_Lahan, group=1)) + geom_line() + geom_point()
    ggplotly(landCover_plot)
  } 
  # else if(input$bauResults=="Proyeksi Emisi Terkait Tutupan Lahan"){
  #   removeUI(selector='#baupdrb')
  #   landCoverData<-cbind(landCover_t1_years,colSums(landCover_t1))
  #   colnames(landCoverData)<-c("year", "Tutupan_Lahan")
  #   landCover_plot<-ggplot(data=landCoverData, aes(x=year, y=Tutupan_Lahan, group=1)) + geom_line() + geom_point()
  #   ggplotly(landCover_plot)
  # }
  
  
})

output$tableResultsBAU <- renderDataTable({
  resultGDP <- bauResults$resultGDP
  resultIncomePerCapita <- bauResults$resultIncomePerCapita  
  resultIncome <- bauResults$resultIncome
  resultLabour <- bauResults$resultLabour
  resultEnergyConsumption <- bauResults$resultEnergyConsumption 
  resultEnergyEmission <- bauResults$resultEnergyEmission 
  resultWasteDisposal <- bauResults$resultWasteDisposal  
  resultWasteEmission <- bauResults$resultWasteEmission 
  resultTotalEmission <- bauResults$resultTotalEmission
  resultLandCover <- bauResults$resultLandCover
  
  if(input$bauResults == "Proyeksi PDRB"){
    tables <- resultGDP[resultGDP$year==input$selectedYear,]
    tables
  } else if(input$bauResults == "Proyeksi Upah per Kapita"){
    return(NULL)
  } else if(input$bauResults == "Proyeksi Upah Gaji"){
    tables <- resultIncome[resultIncome$year==input$selectedYear,]
    tables
  } else if(input$bauResults == "Proyeksi Tenaga Kerja"){
    tables <- resultLabour[resultLabour$year==input$selectedYear,]
    tables
  } else if(input$bauResults == "Proyeksi Konsumsi Energi"){
    tables <- resultEnergyConsumption[resultEnergyConsumption$year==input$selectedYear,]
    tables
  } else if(input$bauResults == "Proyeksi Emisi Terkait Konsumsi Energi"){
    tables <- resultEnergyEmission[resultEnergyEmission$year==input$selectedYear,]
    tables
  } else if(input$bauResults == "Proyeksi Buangan Limbah"){
    tables <- resultWasteDisposal[resultWasteDisposal$year==input$selectedYear,]
    tables
  } else if(input$bauResults == "Proyeksi Emisi Terkait Buangan Limbah"){
    tables <- resultWasteEmission[resultWasteEmission$year==input$selectedYear,]
    tables
  } else if(input$bauResults == "Proyeksi Total Emisi"){
    return(NULL)
  } else if(input$bauResults == "Proyeksi Intensitas Emisi"){
    return(NULL)
  } else if (input$bauResults=='Proyeksi Tutupan Lahan'){
    tables <- resultLandCover[resultLandCover$year==input$selectedYear,]
    tables 
  }
  datatable(tables, extensions = "FixedColumns", options=list(pageLength=100, scrollX=TRUE, scrollY="500px", fixedColumns=list(leftColumns=1)), rownames=FALSE)%>%
    formatRound(columns=c(3:length(tables)),2)
}) #extensions = "FixedColumns", options=list(pageLength=50, scrollX=TRUE, scrollY="600px", fixedColumns=list(leftColumns=1)), rownames=FALSE)  

output$downloadTableBAU <- downloadHandler(
  filename = input$bauResults,
  contentType = "text/csv",
  content = function(file) {
    resultGDP <- bauResults$resultGDP
    resultIncomePerCapita <- bauResults$resultIncomePerCapita  
    resultIncome <- bauResults$resultIncome
    resultLabour <- bauResults$resultLabour
    resultEnergyConsumption <- bauResults$resultEnergyConsumption 
    resultEnergyEmission <- bauResults$resultEnergyEmission 
    resultWasteDisposal <- bauResults$resultWasteDisposal  
    resultWasteEmission <- bauResults$resultWasteEmission 
    resultTotalEmission <- bauResults$resultTotalEmission
    
    if(input$bauResults == "Proyeksi PDRB"){
      tables <- resultGDP[resultGDP$year==input$selectedYear,]
    } else if(input$bauResults == "Proyeksi Upah per Kapita"){
      return(NULL)
    } else if(input$bauResults == "Proyeksi Upah Gaji"){
      tables <- resultIncome[resultIncome$year==input$selectedYear,]
    } else if(input$bauResults == "Proyeksi Tenaga Kerja"){
      tables <- resultLabour[resultLabour$year==input$selectedYear,]
    } else if(input$bauResults == "Proyeksi Konsumsi Energi"){
      tables <- resultEnergyConsumption[resultEnergyConsumption$year==input$selectedYear,]
    } else if(input$bauResults == "Proyeksi Emisi Terkait Konsumsi Energi"){
      tables <- resultEnergyEmission[resultEnergyEmission$year==input$selectedYear,]
    } else if(input$bauResults == "Proyeksi Buangan Limbah"){
      tables <- resultWasteDisposal[resultWasteDisposal$year==input$selectedYear,]
    } else if(input$bauResults == "Proyeksi Emisi Terkait Buangan Limbah"){
      tables <- resultWasteEmission[resultWasteEmission$year==input$selectedYear,]
    } else if(input$bauResults == "Proyeksi Total Emisi"){
      return(NULL)
    }
    write.table(tables, file, quote=FALSE, row.names=FALSE, sep=",")
  }
)  

#### BEGIN : tampilkan result BAU sektor lahan====
output$yearSelection2 <- renderUI({
  sec<-blackBoxInputs()
  tahun<-sec$tahun
  selectInput("selectedYear2", "Tahun", "Pilih tahun", choices=c(tahun))
}) 

output$plotlyResultsBAU_lahan <- renderPlotly({
  results2 <- allInputsBAULahan()
  OutputLahan_plot<-results2$BAULahan_0
  PDRBLahan_plot <-results2$BAULahan_1
  incomeLahan_plot<-results2$BAULahan_2
  profitLahan_plot<-results2$BAULahan_3
  pajakLahan_plot<-results2$BAULahan_4
  imporLahan_plot<-as.data.frame(results2$BAULahan_5)
  eksporLahan_plot<-as.data.frame(results2$BAULahan_6)
  belpemLahan_plot<-results2$BAULahan_7
  belrtLahan_plot<-results2$BAULahan_8
  labourLahan_plot<-results2$BAULahan_9
  tahun<-as.data.frame(results2$tahun)
  ekspor_impor<-as.vector(eksporLahan_plot$ekspor) - as.vector(imporLahan_plot$impor)
  neracaLahan_plot <-as.data.frame(cbind(tahun,ekspor_impor))
  colnames(neracaLahan_plot)<-c("year", "ekspor_impor")
  
  if(input$lahanResults=="Proyeksi Output"){
    removeUI(selector = '#baupdrb')
    OutputLahan_gplot<-ggplot(data=OutputLahan_plot, aes(x=year, y=output, group=1)) + geom_line() + geom_point()
    ggplotly(OutputLahan_gplot)
  } 
  else if(input$lahanResults=="Proyeksi PDRB"){
    removeUI(selector = '#baupdrb')
    PDRBLahan_gplot<-ggplot(data=PDRBLahan_plot, aes(x=year, y=PDRB, group=1)) + geom_line() + geom_point()
    ggplotly(PDRBLahan_gplot)
  } 
  else if (input$lahanResults=="Proyeksi Income"){
    removeUI(selector = '#baupdrb')
    incomeLahan_gplot<-ggplot(data=incomeLahan_plot, aes(x=year, y=income, group=1)) + geom_line() + geom_point()
    ggplotly(incomeLahan_gplot)
  }
  else if (input$lahanResults=="Proyeksi Profit"){
    removeUI(selector = '#baupdrb')
    profitLahan_gplot<-ggplot(data=profitLahan_plot, aes(x=year, y=profit, group=1)) + geom_line() + geom_point()
    ggplotly(profitLahan_gplot)
  }
  else if (input$lahanResults=="Proyeksi Pajak"){
    removeUI(selector = '#baupdrb')
    pajakLahan_gplot<-ggplot(data=pajakLahan_plot, aes(x=year, y=pajak, group=1)) + geom_line() + geom_point()
    ggplotly(pajakLahan_gplot)
  }
  else if (input$lahanResults=="Proyeksi Impor"){
    removeUI(selector = '#baupdrb')
    imporLahan_gplot<-ggplot(data=imporLahan_plot, aes(x=year, y=impor, group=1)) + geom_line() + geom_point()
    ggplotly(imporLahan_gplot)
  }
  else if (input$lahanResults=="Proyeksi Ekspor"){
    removeUI(selector = '#baupdrb')
    eksporLahan_gplot<-ggplot(data=eksporLahan_plot, aes(x=year, y=ekspor, group=1)) + geom_line() + geom_point()
    ggplotly(eksporLahan_gplot)
  }
  else if (input$lahanResults=="Proyeksi Belanja Pemerintah"){
    removeUI(selector = '#baupdrb')
    belpemLahan_gplot<-ggplot(data=belpemLahan_plot, aes(x=year, y=belanja_pemerintah, group=1)) + geom_line() + geom_point()
    ggplotly(belpemLahan_gplot)
  }
  else if (input$lahanResults=="Proyeksi Belanja Rumah Tangga"){
    removeUI(selector = '#baupdrb')
    belrtLahan_gplot<-ggplot(data=belrtLahan_plot, aes(x=year, y=belanja_RT, group=1)) + geom_line() + geom_point()
    ggplotly(belrtLahan_gplot)
  }
  else if (input$lahanResults=="Proyeksi Tenaga Kerja"){
    removeUI(selector = '#baupdrb')
    labourLahan_gplot<-ggplot(data=labourLahan_plot, aes(x=year, y=labour, group=1)) + geom_line() + geom_point()
    ggplotly(labourLahan_gplot)
  }
  else if (input$lahanResults=="Proyeksi Neraca Perdagangan"){
    removeUI(selector = '#baupdrb')
    neracaLahan_gplot<-ggplot(data=neracaLahan_plot, aes(x=year, y=ekspor_impor, group=1)) + geom_line() + geom_point()
    ggplotly(neracaLahan_gplot)
  }
})
output$tableResultsBAU_lahan <- renderDataTable({
  results2 <- allInputsBAULahan()
  OutputLahan_table<-as.data.frame(results2$lahanResult_0)
  PDRBLahan_table <-as.data.frame(results2$lahanResult_1)
  incomeLahan_table<-as.data.frame(results2$lahanResult_2)
  profitLahan_table<-as.data.frame(results2$lahanResult_3)
  pajakLahan_table<-as.data.frame(results2$lahanResult_4)
  imporLahan_table<-as.data.frame(results2$lahanResult_5)
  eksporLahan_table<-as.data.frame(results2$lahanResult_6)
  belpemLahan_table<-as.data.frame(results2$lahanResult_7)
  belrtLahan_table<-as.data.frame(results2$lahanResult_8)
  resultLabour<-as.data.frame(results2$lahanResult_9)
  
  if(input$lahanResults== "Proyeksi Output"){
    tables <- OutputLahan_table[OutputLahan_table$year==input$selectedYear2,]
    tables
  }
  else if(input$lahanResults == "Proyeksi PDRB"){
    tables <- PDRBLahan_table[PDRBLahan_table$year==input$selectedYear2,]
    tables
  } 
  else if(input$lahanResults == "Proyeksi Income"){
    tables <- incomeLahan_table[incomeLahan_table$year==input$selectedYear2,]
    tables} 
  else if(input$lahanResults == "Proyeksi Profit"){
    tables <- profitLahan_table[profitLahan_table$year==input$selectedYear2,]
    tables
  } 
  else if(input$lahanResults == "Proyeksi Pajak"){
    tables <- pajakLahan_table[pajakLahan_table$year==input$selectedYear2,]
    tables} 
  else if(input$lahanResults == "Proyeksi Impor"){
    tables <- imporLahan_table[imporLahan_table$year==input$selectedYear2,]
    tables} 
  else if(input$lahanResults == "Proyeksi Ekspor"){
    tables <- eksporLahan_table[eksporLahan_table$year==input$selectedYear2,]
    tables} 
  else if(input$lahanResults == "Proyeksi Belanja Pemerintah"){
    tables <- belpemLahan_table[belpemLahan_table$year==input$selectedYear2,]
    tables
  } 
  else if(input$lahanResults == "Proyeksi Belanja Rumah Tangga"){
    tables <- belrtLahan_table[belrtLahan_table$year==input$selectedYear2,]
    tables
  } 
  else if(input$lahanResults == "Proyeksi Tenaga Kerja"){
    tables <- resultLabour[resultLabour$year==input$selectedYear2,]
    tables
  }
})

## download table BAU sektor lahan
output$downloadTableBAU_lahan <- downloadHandler(
  filename = input$lahanResults,
  contentType = "text/csv",
  content = function(file) {
    results2 <- allInputsBAULahan()
    OutputLahan_table<-as.data.frame(results2lahanResult_0)
    PDRBLahan_table <-as.data.frame(results2$lahanResult_1)
    incomeLahan_table<-as.data.frame(results2$lahanResult_2)
    profitLahan_table<-as.data.frame(results2$lahanResult_3)
    pajakLahan_table<-as.data.frame(results2$lahanResult_4)
    imporLahan_table<-as.data.frame(results2$lahanResult_5)
    eksporLahan_table<-as.data.frame(results2$lahanResult_6)
    belpemLahan_table<-as.data.frame(results2$lahanResult_7)
    belrtLahan_table<-as.data.frame(results2$lahanResult_8)
    resultLabour<-as.data.frame(results2$lahanResult_9)
    
    if(input$lahanResults== "Proyeksi Output"){
      tables <- OutputLahan_table[OutputLahan_table$year==input$selectedYear2,]
    }
    else if(input$lahanResults == "Proyeksi PDRB"){
      tables <- PDRBLahan_table[PDRBLahan_table$year==input$selectedYear2,]
    } 
    else if(input$lahanResults == "Proyeksi Income"){
      tables <- incomeLahan_table[incomeLahan_table$year==input$selectedYear2,]
    }
    else if(input$lahanResults == "Proyeksi Profit"){
      tables <- profitLahan_table[profitLahan_table$year==input$selectedYear2,]
    }
    else if(input$lahanResults == "Proyeksi Pajak"){
      tables <- pajakLahan_table[pajakLahan_table$year==input$selectedYear2,]
    }
    else if(input$lahanResults == "Proyeksi Impor"){
      tables <- imporLahan_table[imporLahan_table$year==input$selectedYear2,]
    }
    else if(input$lahanResults == "Proyeksi Ekspor"){
      tables <- eksporLahan_table[eksporLahan_table$year==input$selectedYear2,]
    }
    else if(input$lahanResults == "Proyeksi Belanja Pemerintah"){
      tables <- belpemLahan_table[belpemLahan_table$year==input$selectedYear2,]
    }
    else if(input$lahanResults == "Proyeksi Belanja Rumah Tangga"){
      tables <- belrtLahan_table[belrtLahan_table$year==input$selectedYear2,]
    } 
    else if(input$lahanResults == "Proyeksi Tenaga Kerja"){
      tables <- resultLabour[resultLabour$year==input$selectedYear2,]
    }
    else if(input$lahanResults == "Proyeksi Neraca Perdagangan"){
      return(NULL)
    }
    write.table(tables, file, quote=FALSE, row.names=FALSE, sep=",")
  }
)  

output$selectizeSector <- renderUI({
  if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
  analysisResult <- sec$result
  selectizeInput('selectMultiSector', 'Lapangan Usaha Terkait', choices=list(
    Sektor=as.character(analysisResult$Sektor)
  ), multiple=TRUE)
})


output$yearSelectionInter <- renderUI({
  selectInput("selectedYearInter", "Tahun", "Pilih tahun", choices=c(input$initialYear:input$finalYear))
})  

#### END : tampilkan result BAU sektor lahan====