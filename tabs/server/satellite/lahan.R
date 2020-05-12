#### Server: Satelit Lahan ####

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
  }
  else {
    removeUI(selector='#projTypeEconomyUI')
    insertUI(selector='#inputProjType',
             where = 'afterEnd',
             ui=uiOutput('projTypeLandUI'))
    
  }
})

observeEvent(input$buttonBAU,{
  removeUI(selector='#gdpRateUI') 
  insertUI(selector='#inputProjType',
           where='afterEnd',
           ui= uiOutput('resultUI'))
})

output$projTypeEconomyUI<-renderUI(
  tagList(selectInput("typeIntervention", "Tipe Intervensi", choices = c("Tipe 1", "Tipe 2")),
          selectInput("initialYear", "Tahun awal:", choices = 1990:2100, selected=2015),
          selectInput("finalYear", "Tahun akhir:", choices = 1990:2100, selected=2030),
          # fileInput("populationTable", "Tabel Populasi per Tahun", buttonLabel="Browse...", placeholder="No file selected"),
          # fileInput("emissionSectorRADTable", "Tabel Emisi Sumber Lain", buttonLabel="Browse...", placeholder="No file selected"),
          actionButton("generateBAUTable", "Buat Tabel"),
          hr(),
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
                      ))
          # actionButton("showResult", "Tampilkan")
  )
)

# observeEvent(input$showResult, {
#   tagList(uiOutput("resultUI"))
# })

output$resultUI<-renderUI(
  tagList(h3("Hasil Analasis"),
          tags$div(id='bauplaceholder'),
          conditionalPanel(
            condition="input.bauResults!='Proyeksi Upah per Kapita' & input.bauResults!='Proyeksi Total Emisi'",
            uiOutput("yearSelection")),
          plotlyOutput("plotlyResultsBAU"),
          hr(),
          fluidRow(column(width=12,
                          box(width=NULL,
                              dataTableOutput('tableResultsBAU'),
                              downloadButton('downloadTableBAU', 'Download Table (.csv)'))))
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
  
})