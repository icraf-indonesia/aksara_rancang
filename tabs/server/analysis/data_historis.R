# debug mode
debugMode <- 1
notif_id <- NULL

provList <- readRDS("data/provList")
# usersList <- load("usersList")

# LDMProp_new<-reactiveValues(
#   tablo = NULL,
#   coba= NULL
# )
# 
# tabel<-reactiveValues(
#   manualSave=NULL
# )

# ldmRV<-reactiveValues(
#   LDMListFile = unique(list.files(paste0("LDMData/Prov/"))),   # ganti mas alfa
#   LDMTotFile= unique(length(list.files("LDMData/Prov/")))   # ganti mas alfa
# )

# editable<-reactiveValues(
#   BAULahan_landCover=NULL
# )

allDataProv <- reactiveValues(
  username = NULL,
  prov = NULL,
  sector = NULL,
  indem = NULL,
  findem = NULL,
  addval = NULL,
  labour = NULL,
  energy = NULL,
  waste = NULL,
  ef_energy = NULL,
  ef_waste = NULL,
  findemcom = NULL,
  addvalcom = NULL,
  population = NULL,
  otherEm = NULL,
  LUtahun=NULL,
  LDMProp=NULL,
  LDMProp_his=NULL,
  # landDemand = NULL,
  # landDemand_prop = NULL,
  I_A = NULL,
  leontief = NULL,
  GDPAll = NULL,
  linkagesTable = NULL,
  multiplierAll = NULL,
  periodIO = NULL,
  rtffile = NULL,
  bau_scenario = NULL,
  prk_scenario = data.frame(time=NULL, action=NULL, year=NULL, username=NULL, provinsi=NULL, sector=NULL, fd_value=NULL)
)

finalResults <- reactiveValues(table1=NULL, plot23=NULL, plot24=NULL, plot25=NULL)

historicalResults <- reactiveValues()
bauResults <- reactiveValues()
interventionResults <- reactiveValues()

observeEvent(input$inputLogin, {
  fullname <- input$fullname
  username <- input$username
  password <- input$password
  selectedProv <- input$categoryProvince
  
  # if(password %in% provList$Password){
  #   
  # } else {
  #   return(NULL)
  # }
  # usersList <- data.frame(id=NULL, fullname=NULL, username=NULL, password=NULL, provinsi=NULL)
  
  datapath <- paste0("data/", selectedProv, "/")
  userFolder <- paste0(datapath, username)
  if(!dir.exists(userFolder)) dir.create(userFolder, mode = 777)
  # system(paste0("chmod -R 777 ", userFolder))
  
  sector <- readRDS(paste0(datapath, "sector"))
  indem <- readRDS(paste0(datapath, "indem"))
  findem <- readRDS(paste0(datapath, "findem"))
  addval <- readRDS(paste0(datapath, "addval"))
  labour <- readRDS(paste0(datapath, "labour"))
  energy <- readRDS(paste0(datapath, "energy"))
  waste <- readRDS(paste0(datapath, "waste"))
  ef_energy <- readRDS(paste0(datapath, "ef_energy"))
  ef_waste <- readRDS(paste0(datapath, "ef_waste"))
  findemcom <- readRDS(paste0(datapath, "findemcom"))
  addvalcom <- readRDS(paste0(datapath, "addvalcom"))
  currentPopulation <- readRDS(paste0(datapath, "currentPopulation"))
  population <- readRDS(paste0(datapath, "population"))
  otherEm <- readRDS(paste0(datapath, "otherEm"))
  LU_tahun<-readRDS(paste0(datapath,"LU_tahun"))
  LDMProp_his<-readRDS(paste0(datapath,"LDMProp"))
  # row.names(LDMProp_his)<-sector[,1]
  # landDemand <- readRDS(paste0(datapath, "landDemand"))
  # landDemand_prop <- readRDS(paste0(datapath, "landDemand_prop"))
  # landtable <- readRDS(paste0(datapath, "landtable"))
  I_A <- readRDS(paste0(datapath, "I_A"))
  leontief <- readRDS(paste0(datapath, "leontief"))
  GDPAll <- readRDS(paste0(datapath, "GDPAll"))
  linkagesTable <- readRDS(paste0(datapath, "linkagesTable"))
  multiplierAll <- readRDS(paste0(datapath, "multiplierAll"))
  periodIO <- readRDS(paste0(datapath, "periodIO"))
  rtffile <- readRDS(paste0(datapath, "rtffile"))
  
  allDataProv$username = username 
  allDataProv$prov = selectedProv 
  allDataProv$sector = sector 
  allDataProv$indem = indem 
  allDataProv$findem = findem 
  allDataProv$addval = addval 
  allDataProv$labour = labour 
  allDataProv$energy = energy 
  allDataProv$waste = waste 
  allDataProv$ef_energy = ef_energy 
  allDataProv$ef_waste = ef_waste 
  allDataProv$findemcom = findemcom 
  allDataProv$addvalcom = addvalcom 
  allDataProv$population = population 
  allDataProv$currentPopulation = currentPopulation
  allDataProv$otherEm = otherEm 
  allDataProv$LU_tahun = LU_tahun
  allDataProv$LDMProp_his = LDMProp_his
  allDataProv$LDMProp = data.frame()
  # allDataProv$landDemand = landDemand 
  # allDataProv$landDemand_prop = landDemand_prop 
  allDataProv$I_A = I_A 
  allDataProv$leontief = leontief 
  allDataProv$GDPAll = GDPAll 
  allDataProv$linkagesTable = linkagesTable 
  allDataProv$multiplierAll = multiplierAll 
  allDataProv$periodIO = periodIO 
  allDataProv$rtffile = rtffile 
  allDataProv$bau_scenario = data.frame(Lapangan_usaha=as.character(sector[,1]))
  
  recordActivities("Login redcluwe.id", "Berhasil", paste0(Sys.time()))
  
  notif_id <<- showNotification("Anda berhasil masuk", duration = 4, closeButton = TRUE, type = "warning")
  updateTabItems(session, "tabs", selected = "pageOne")
})

blackBoxInputs <- function(){
  sector <- allDataProv$sector
  indem <- allDataProv$indem
  findem <- allDataProv$findem
  addval <- allDataProv$addval
  labour <- allDataProv$labour
  energy <- allDataProv$energy
  waste <- allDataProv$waste
  ef_energy <- allDataProv$ef_energy
  ef_waste <- allDataProv$ef_waste
  findemcom <- allDataProv$findemcom
  addvalcom <- allDataProv$addvalcom
  population <- allDataProv$population
  currentPopulation <- allDataProv$currentPopulation
  otherEm <- allDataProv$otherEm
  LU_tahun <- allDataProv$LU_tahun
  LDMProp_his <- allDataProv$LDMProp_his
  row.names(LDMProp_his)<-sector[,1]
  LDMProp <- allDataProv$LDMProp
  # landDemand <- allDataProv$landDemand
  # landDemand_prop <- allDataProv$landDemand_prop
  # landtable <- allDataProv$landtable
  I_A <- allDataProv$I_A
  leontief <- allDataProv$leontief
  GDPAll <- allDataProv$GDPAll
  linkagesTable <- allDataProv$linkagesTable
  multiplierAll <- allDataProv$multiplierAll
  periodIO <- allDataProv$periodIO
  rtffile <- allDataProv$rtffile
  
  # Row explicit definition for Income (Wages & Salary)
  income_row <- 2
  
  indem_matrix <- as.matrix(indem)
  addval_matrix <- as.matrix(addval)
  num_addval <- nrow(addval_matrix)
  dimensi <- ncol(indem_matrix)
  
  indem_colsum <- colSums(indem_matrix)
  addval_colsum <- colSums(addval_matrix)
  # total_output <- indem_colsum+addval_colsum
  fin_con <- 1/(indem_colsum+addval_colsum)
  fin_con[is.infinite(fin_con)] <- 0
  tinput_invers <- diag(fin_con)
  
  # Backward Linkage
  DBL <- colSums(leontief)
  DBL <- DBL/(mean(DBL))
  
  # Forward Linkage
  DFL <- rowSums(leontief)
  DFL <- DFL/(mean(DFL))
  
  # GDP
  GDP <- colSums(addval_matrix[2:num_addval,])
  
  # Multiplier Output
  multiplierOutput <- colSums(leontief)
  
  # Multiplier Income
  income_coef <- as.matrix(addval_matrix[income_row,]) / as.matrix((indem_colsum+addval_colsum))
  multiplierIncome <- leontief %*% income_coef
  # income_coef <- tinput_invers %*% as.matrix(addval_matrix[income_row,])
  # income_matrix <- diag(as.vector(income_coef), ncol = dimensi, nrow = dimensi)
  # InvIncome_matrix <- diag(as.vector(1/income_coef), ncol = dimensi, nrow = dimensi)
  # multiplierIncome <- income_matrix %*% leontief %*% InvIncome_matrix 
  # multiplierIncome <- as.matrix(colSums(multiplierIncome), dimensi, 1)
  multiplierIncome[is.na(multiplierIncome)] <- 0
  
  # Labour
  labour_coef <- as.matrix(labour[,3]) / as.matrix((indem_colsum+addval_colsum))
  multiplierLabour <- leontief %*% labour_coef
  # labour_coef <- tinput_invers %*% as.matrix(labour[,3])
  # labour_matrix <- diag(as.vector(labour_coef), ncol = dimensi, nrow = dimensi)
  # InvLabour_matrix <- diag(as.vector(1/labour_coef), ncol = dimensi, nrow = dimensi)
  # multiplierLabour <- labour_matrix %*% leontief %*% InvLabour_matrix
  # multiplierLabour <- as.matrix(colSums(multiplierLabour), dimensi, 1)
  multiplierLabour[is.na(multiplierLabour)] <- 0
  
  # Multiplier Energy Used
  energy_coef <- as.matrix(energy[,3]) / as.matrix((indem_colsum+addval_colsum))
  multiplierEnergy <- leontief %*% energy_coef
  # energy_coef <- tinput_invers %*% as.matrix(energy[,3])
  # energy_matrix <- diag(as.vector(energy_coef), ncol = dimensi, nrow = dimensi)
  # InvEnergy_matrix <- diag(as.vector(1/energy_coef), ncol = dimensi, nrow = dimensi)
  # multiplierEnergy <- energy_matrix %*% leontief %*% InvEnergy_matrix
  # multiplierEnergy <- as.matrix(colSums(multiplierEnergy), dimensi, 1)
  multiplierEnergy[is.na(multiplierEnergy)] <- 0
  
  # Multiplier Waste Product
  waste_coef <- as.matrix(waste[,3]) / as.matrix((indem_colsum+addval_colsum))
  multiplierWaste <- leontief %*% waste_coef
  # waste_coef <- tinput_invers %*% as.matrix(waste[,3])
  # waste_matrix <- diag(as.vector(energy_coef), ncol = dimensi, nrow = dimensi)
  # InvWaste_matrix <- diag(as.vector(1/waste_coef), ncol = dimensi, nrow = dimensi)
  # multiplierWaste <- waste_matrix %*% leontief %*% InvWaste_matrix
  # multiplierWaste <- as.matrix(colSums(multiplierWaste), dimensi, 1)
  multiplierWaste[is.na(multiplierWaste)] <- 0
  
  # Ratio Wages / Business Surplus
  ratio_ws <- t(as.matrix(addval[2,] / addval[3,]))
  ratio_ws[is.na(ratio_ws)] <- 0
  ratio_ws[ratio_ws == Inf] <- 0
  colnames(ratio_ws) <- "ratio_ws"
  
  # Koefisien Intensitas Energi
  # total sectoral energy cons / sectoral GDP
  coef_energy <- as.matrix(energy[,3]) / sum(addval_matrix[2:num_addval,])
  
  # Koefisien Produk Limbah
  coef_waste <- as.matrix(waste[,3]) / sum(addval_matrix[2:num_addval,])
  
  # Emission from energy
  f_energy_diag <- diag(ef_energy[,2], ncol = nrow(ef_energy), nrow = nrow(ef_energy))
  em_energy <- as.matrix(energy[,4:ncol(energy)]) %*% f_energy_diag
  em_energy_total <- rowSums(em_energy)
  
  # Emission from waste
  f_waste_diag <- diag(ef_waste[,2], ncol = nrow(ef_waste), nrow = nrow(ef_waste))
  em_waste <- as.matrix(waste[,4:ncol(waste)]) %*% f_waste_diag
  em_waste_total <- rowSums(em_waste)
  
  # Wages
  wages <- as.matrix(t(addval[2,]))
  colnames(wages) <- "wages"
  
  # Income per capita
  income_per_capita <- sum(as.matrix(addval_matrix[income_row,])) / currentPopulation
  
  # for calculate landTable, LPC, LRC historis 
  LU_tahun<-as.data.frame(LU_tahun[,3:ncol(LU_tahun)])
  LU_tahun<-as.matrix(LU_tahun)
  LDMProp_his<-as.matrix(LDMProp_his[,2:ncol(LDMProp_his)])
  GDPAll<-as.data.frame(GDPAll)
  diagLU_his<-as.matrix(diag(LU_tahun[,1]))
  landTable_his<-LDMProp_his %*% diagLU_his
  landReq_his<-as.matrix(rowSums(landTable_his))
  
  LPC_his<-GDPAll[,4]/landReq_his
  LPC_his[is.infinite(LPC_his)]<-0
  LRC_his<-1/LPC_his
  LRC_his[is.infinite(LRC_his)]<-0
  landTable_his<-cbind(sector, landTable_his, landReq_his, LPC_his, LRC_his)
  colnames(landTable_his)<-c("Sektor", "Kategori", colnames(LDMProp_his),"Total Kebutuhan Lahan", "LPC", "LRC")
  tahun<-as.vector(str_extract_all(colnames(LU_tahun), '[0-9]+'))
  tahun<-as.data.frame(tahun)
  tahun<-t(tahun)
  
  result <- cbind(sector,
                  DBL,
                  DFL, 
                  GDP, 
                  multiplierOutput, 
                  multiplierIncome,
                  multiplierLabour,
                  labour_coef,
                  multiplierEnergy,
                  multiplierWaste,
                  wages,
                  ratio_ws, 
                  coef_energy,
                  coef_waste,
                  em_energy_total,
                  em_waste_total
  )
  colnames(result)[1] <- "Sektor"
  
  list_table <- list(result=result,
                     sector=sector, 
                     indem=indem, 
                     findem=findem, 
                     addval=addval, 
                     labour=labour, 
                     energy=energy, 
                     findemcom=findemcom, 
                     addvalcom=addvalcom,
                     waste=waste,
                     ef_waste=ef_waste,
                     ef_energy=ef_energy,
                     landcover=allDataProv$LU_tahun,
                     income_per_capita=income_per_capita,
                     otherEm=otherEm,
                     population=population,
                     LU_tahun=LU_tahun,
                     LDM=allDataProv$LDMProp_his,
                     GDPAll=GDPAll,
                     # landTable_t0=landTable_t0,
                     # landReq=landReq,
                     tahun=tahun, 
                     landTable_his=landTable_his,
                     leontief=leontief, 
                     LDMProp_his = LDMProp_his
  ) 
  
  return(list_table)
}

allInputs <- eventReactive(input$button, {
  inSector <- input$sector
  if(is.null(inSector))
    return(NULL)
  
  inIntermediateDemand <- input$intermediateDemand
  if(is.null(inIntermediateDemand))
    return(NULL)
  
  inFinalDemand <- input$finalDemand
  if(is.null(inFinalDemand))
    return(NULL)
  
  inAddedValue <- input$addedValue
  if(is.null(inAddedValue))
    return(NULL)    
  
  inLabour <- input$labour
  if(is.null(inLabour))
    return(NULL)
  
  inEnergy <- input$energyTable
  if(is.null(inEnergy))
    return(NULL) 
  
  inWaste <- input$wasteTable
  if(is.null(inWaste))
    return(NULL)
  
  inEmissionFactorEnergiTable <- input$emissionFactorEnergiTable
  if(is.null(inEmissionFactorEnergiTable))
    return(NULL)
  
  inEmissionFactorLandWasteTable <- input$emissionFactorLandWasteTable
  if(is.null(inEmissionFactorLandWasteTable))
    return(NULL)
  
  inFinalDemandComp <- input$finalDemandComponent
  if(is.null(inFinalDemandComp))
    return(NULL) 
  
  inAddedValueComp <- input$addedValueComponent
  if(is.null(inAddedValueComp))
    return(NULL)  
  
  sector <- read.table(inSector$datapath, header=FALSE, sep=",")
  indem <- read.table(inIntermediateDemand$datapath, header=FALSE, sep=",")
  findem <- read.table(inFinalDemand$datapath, header=FALSE, sep=",")
  addval <- read.table(inAddedValue$datapath, header=FALSE, sep=",")
  labour <- read.table(inLabour$datapath, header=TRUE, sep=",")
  energy <- read.table(inEnergy$datapath, header=TRUE, sep=",")
  waste <- read.table(inWaste$datapath, header=TRUE, sep=",")
  ef_energy <- read.table(inEmissionFactorEnergiTable$datapath, header=TRUE, sep=",")
  ef_waste <- read.table(inEmissionFactorLandWasteTable$datapath, header=TRUE, sep=",")
  findemcom <- read.table(inFinalDemandComp$datapath, header=FALSE, sep=",")
  addvalcom <- read.table(inAddedValueComp$datapath, header=FALSE, sep=",")
  
  # Row explicit definition
  incomeRow <- 2
  
  indem_matrix <- as.matrix(indem)
  addval_matrix <- as.matrix(addval)
  num_addval <- nrow(addval_matrix)
  dimensi <- ncol(indem_matrix)
  
  indem_colsum <- colSums(indem_matrix)
  addval_colsum <- colSums(addval_matrix)
  fin_con <- 1/(indem_colsum+addval_colsum)
  fin_con[is.infinite(fin_con)] <- 0
  tinput_invers <- diag(fin_con)
  A <- indem_matrix %*% tinput_invers
  I <- as.matrix(diag(dimensi))
  I_A <- I-A
  leontief <- solve(I_A)
  
  # Backward Linkage
  DBL <- colSums(leontief)
  DBL <- DBL/(mean(DBL))
  # Forward Linkage
  DFL <- rowSums(leontief)
  DFL <- DFL/(mean(DFL))
  # GDP
  GDP <- colSums(addval_matrix[2:num_addval,])
  # Multiplier Output
  multiplierOutput <- colSums(leontief)
  # Multiplier Income
  income_coef <- tinput_invers %*% as.matrix(addval_matrix[incomeRow,])
  income_matrix <- diag(as.vector(income_coef), ncol = dimensi, nrow = dimensi)
  InvIncome_matrix <- diag(as.vector(1/income_coef), ncol = dimensi, nrow = dimensi)
  multiplierIncome <- income_matrix %*% leontief %*% InvIncome_matrix
  multiplierIncome <- as.matrix(colSums(multiplierIncome), dimensi, 1)
  multiplierIncome[is.na(multiplierIncome)] <- 0
  # Labour
  labour_coef <- tinput_invers %*% as.matrix(labour[,3])
  labour_matrix <- diag(as.vector(labour_coef), ncol = dimensi, nrow = dimensi)
  InvLabour_matrix <- diag(as.vector(1/labour_coef), ncol = dimensi, nrow = dimensi)
  multiplierLabour <- labour_matrix %*% leontief %*% InvLabour_matrix
  multiplierLabour <- as.matrix(colSums(multiplierLabour), dimensi, 1)
  multiplierLabour[is.na(multiplierLabour)] <- 0
  # Multiplier Energy Used
  energy_coef <- tinput_invers %*% as.matrix(energy[,3])
  energy_matrix <- diag(as.vector(energy_coef), ncol = dimensi, nrow = dimensi)
  InvEnergy_matrix <- diag(as.vector(1/energy_coef), ncol = dimensi, nrow = dimensi)
  multiplierEnergy <- energy_matrix %*% leontief %*% InvEnergy_matrix
  multiplierEnergy <- as.matrix(colSums(multiplierEnergy), dimensi, 1)
  multiplierEnergy[is.na(multiplierEnergy)] <- 0
  # Multiplier Waste Product
  waste_coef <- tinput_invers %*% as.matrix(waste[,3])
  waste_matrix <- diag(as.vector(energy_coef), ncol = dimensi, nrow = dimensi)
  InvWaste_matrix <- diag(as.vector(1/waste_coef), ncol = dimensi, nrow = dimensi)
  multiplierWaste <- waste_matrix %*% leontief %*% InvWaste_matrix
  multiplierWaste <- as.matrix(colSums(multiplierWaste), dimensi, 1)
  multiplierWaste[is.na(multiplierWaste)] <- 0
  # Ratio Wages / Business Surplus
  ratio_ws <- t(as.matrix(addval[2,] / addval[3,]))
  ratio_ws[is.na(ratio_ws)] <- 0
  ratio_ws[ratio_ws == Inf] <- 0
  colnames(ratio_ws) <- "ratio_ws"
  # Koefisien Intensitas Energi
  # total sectoral energy cons / sectoral GDP
  coef_energy <- as.matrix(energy[,3]) / sum(addval_matrix[2:num_addval,])
  # Koefisien Produk Limbah
  coef_waste <- as.matrix(waste[,3]) / sum(addval_matrix[2:num_addval,])
  # Emission from energy
  f_energy_diag <- diag(ef_energy[,2], ncol = nrow(ef_energy), nrow = nrow(ef_energy))
  em_energy <- as.matrix(energy[,4:ncol(energy)]) %*% f_energy_diag # need to count ncol
  em_energy_total <- rowSums(em_energy)
  # Emission from waste
  f_waste_diag <- diag(ef_waste[,2], ncol = nrow(ef_waste), nrow = nrow(ef_waste))
  em_waste <- as.matrix(waste[,4:ncol(waste)]) %*% f_waste_diag # need to count ncol
  em_waste_total <- rowSums(em_waste)
  # Wages
  wages <- as.matrix(t(addval[2,]))
  colnames(wages) <- "wages"
  
  # Income per capita
  income_per_capita <- sum(as.matrix(addval_matrix[incomeRow,])) / input$popDensTable
  
  result <- cbind(sector,
                  DBL,
                  DFL, 
                  GDP, 
                  multiplierOutput, 
                  multiplierIncome,
                  multiplierLabour,
                  multiplierEnergy,
                  multiplierWaste,
                  wages,
                  ratio_ws, 
                  coef_energy,
                  coef_waste,
                  em_energy_total,
                  em_waste_total
  )
  colnames(result)[1] <- "Sektor"
  
  list_table <- list(result=result, 
                     sector=sector, 
                     indem=indem, 
                     findem=findem, 
                     addval=addval, 
                     labour=labour, 
                     energy=energy, 
                     findemcom=findemcom, 
                     addvalcom=addvalcom,
                     waste=waste,
                     ef_waste=ef_waste,
                     ef_energy=ef_energy,
                     income_per_capita=income_per_capita
  ) 
  list_table
})

output$yearIO <- renderText({ paste0("Tahun Tabel IO: ", allDataProv$periodIO) })

output$sectorSelection <- renderUI({
  if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
  analysisResult <- sec$result
  selectInput("selectedSector", "Sektor", "Pilih sektor", choices=as.character(analysisResult$Sektor))
})

output$plotlyResults <- renderPlotly({
  if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
  analysisResult <- sec$result
  income_per_capita <- sec$income_per_capita
  graph <- data.frame(Sektor="", Analysis="")
  landTable_his<-sec$landTable_his
  
  if(input$categorySector=="Ekonomi"){
    if(input$pprkResults == "PDRB"){
      graph <- subset(analysisResult, select = c(Sektor, GDP))
      GDPvalues <- as.matrix(analysisResult$GDP)
      GDPTotal <- colSums(GDPvalues)
      GDPTotal <- round(GDPTotal,digits = 2)
      #GDPTotalL <- formattable(GDPTotal, digits = 2, format = "f")
      insertUI(
        selector="#placeholder",
        ui = tags$div(
          valueBox(format(GDPTotal, nsmall = 2, big.mark = ".", decimal.mark = ","), "Juta Rupiah", icon = icon("credit-card"), width = 12),
          id='pdrb'
        )
      )
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Backward Linkage"){
      graph <- subset(analysisResult, select = c(Sektor, DBL))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Forward Linkage"){
      graph <- subset(analysisResult, select = c(Sektor, DFL))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Angka Pengganda Output"){
      graph <- subset(analysisResult, select = c(Sektor, multiplierOutput))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Angka Pengganda Pendapatan Rumah Tangga"){
      graph <- subset(analysisResult, select = c(Sektor, multiplierIncome))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Angka Pengganda Tenaga Kerja"){
      graph <- subset(analysisResult, select = c(Sektor, multiplierLabour))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita') 
    } else if(input$pprkResults == "Upah gaji"){
      graph <- subset(analysisResult, select = c(Sektor, wages))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Rasio Upah gaji per Surplus Usaha"){
      graph <- subset(analysisResult, select = c(Sektor, ratio_ws))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Pendapatan per kapita"){
      removeUI(selector = '#pdrb')
      insertUI(
        selector="#placeholder",
        ui = tags$div(
          valueBox(format(income_per_capita, nsmall = 2, big.mark = ".", decimal.mark = ","), "Juta Rupiah/Jiwa", icon = icon("credit-card"), width = 8),
          id='capita'
        )
      )
    } 
    
    if(input$pprkResults == "Perbandingan Angka Pengganda"){
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
      
      multiplierTable <- subset(analysisResult, select = c(Sektor, multiplierIncome, multiplierOutput, multiplierLabour, multiplierEnergy, multiplierWaste))
      tabel_radarchart <- multiplierTable[multiplierTable==input$selectedSector,]
      
      normalize<- function(x){
        return((x-min(x))/(max(x)-min(x)))
      }
      
      tabel_radarchart<-as.data.frame(tabel_radarchart[2:6])
      tabel_radar<-normalize(tabel_radarchart)
      nilai_temp<-t(tabel_radar)
      plot_ly(
        type='scatterpolar',
        r = c(nilai_temp),
        theta = c('multiplierIncome','multiplierOutput','multiplierLabour','multiplierEnergy','multiplierWaste'),
        fill='toself'
      ) %>%
        layout(
          polar=list(
            radialaxis=list(
              visible=T,
              range=c(0,1)
            )
          ),
          showlegend=F
        )
      # tabel_radar <- tabel_radarchart
      # tabel_radar$Sektor <- NULL
      # tabel_radarmax <- data.frame(multiplierIncome=max(multiplierTable$multiplierIncome), 
      #                              multiplierOutput=max(multiplierTable$multiplierOutput), 
      #                              multiplierLabour=max(multiplierTable$multiplierLabour), 
      #                              multiplierEnergy=max(multiplierTable$multiplierEnergy),
      #                              multiplierWaste=max(multiplierTable$multiplierWaste) 
      #                              )
      # tabel_radarmin <- data.frame(multiplierIncome=min(multiplierTable$multiplierIncome),  
      #                              multiplierOutput=min(multiplierTable$multiplierOutput),  
      #                              multiplierLabour=min(multiplierTable$multiplierLabour),  
      #                              multiplierEnergy=min(multiplierTable$multiplierEnergy),
      #                              multiplierWaste=min(multiplierTable$multiplierWaste) 
      #                              )
      # tabel_radar <- rbind(tabel_radarmax, tabel_radarmin, tabel_radar)
      # radarchart(tabel_radar)
      
    } else {
      colnames(graph) <- c("Sektor", "Analisis")
      gplot<-ggplot(data=graph, aes(x=Sektor, y=Analisis, fill=Sektor)) +
        geom_bar(stat="identity", colour="black") + theme_void() +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      ggplotly(gplot)
      
      # plot_ly(data=graph, x = ~Analisis, y = ~Sektor, type = 'bar', orientation = 'h') %>% layout(xaxis = list(title = ""), yaxis = list(title = "", showticklabels=F))
      
      # plot_ly(graph, x=~Analisis, y=~Sektor, fill=~Sektor) %>%
      #   add_bars(orientation = 'h',name=~Sektor) %>%
      #   layout(barmode = 'stack',
      #          xaxis = list(title = "Nilai"),
      #          yaxis = list(title ="Sektor"))
    }
  } else if(input$categorySector=="Energi"){
    if(input$pprkEnergy == "Angka Pengganda Energi"){
      graph <- subset(analysisResult, select = c(Sektor, multiplierEnergy))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkEnergy == "Koefisien Intensitas Energi"){
      graph <- subset(analysisResult, select = c(Sektor, coef_energy))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkEnergy == "Emisi dari Penggunaan Energi"){
      graph <- subset(analysisResult, select = c(Sektor, em_energy_total))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } 
    
    colnames(graph) <- c("Sektor", "Analisis")
    gplot1<-ggplot(data=graph, aes(x=Sektor, y=Analisis, fill=Sektor)) +
      geom_bar(colour="black", stat="identity") + theme_void() +
      coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
    ggplotly(gplot1)
    # plot_ly(graph, x=~Nilai, y=~Sektor, fill=~Sektor) %>%
    #   add_bars(orientation = 'h',name=~Sektor) %>%
    #   layout(barmode = 'stack',
    #          xaxis = list(title = "Nilai"),
    #          yaxis = list(title ="Sektor"))
  } else if(input$categorySector=="Lahan"){
    removeUI(selector = '#pdrb')
    removeUI(selector = '#capita')
    if(input$pprkLand == "Koefisien Kebutuhan Lahan") {
      graph <- subset(landTable_his, select=c(Sektor, Kategori, LRC))
      colnames(graph) <- c("Sektor", "Kategori", "LRC")
      gplot2<-ggplot(data=graph, aes(x=Sektor, y=LRC, fill=Kategori)) +
        geom_bar(colour="black", stat="identity")+ coord_flip() + theme_void() +
        guides(fill=FALSE) + xlab("Sectors") + ylab("Koefisien Kebutuhan Lahan")
      ggplotly(gplot2)
      # plot_ly(graph, x=~LRC, y=~Sektor, fill=~Kategori) %>%
      #   add_bars(orientation = 'h',name=~Kategori) %>%
      #   layout(barmode = 'stack',
      #          xaxis = list(title = "Koefisien Kebutuhan Lahan"),
      #          yaxis = list(title ="Sectors"))
    } else if(input$pprkLand == "Koefisien Produktivitas Lahan") {
      graph <- subset(landTable_his, select=c(Sektor, Kategori, LPC))
      colnames(graph) <- c("Sektor", "Kategori", "LPC")
      gplot2<-ggplot(data=graph, aes(x=Sektor, y=LPC, fill=Kategori)) +
        geom_bar(colour="black", stat="identity")+ coord_flip() + theme_void() +
        guides(fill=FALSE) + xlab("Sektor") + ylab("Koefisien Produktivitas Lahan")
      ggplotly(gplot2)
      # plot_ly(graph, x=~LPC, y=~Sektor, fill=~Kategori) %>%
      #   add_bars(orientation = 'h',name=~Kategori) %>%
      #   layout(barmode = 'stack',
      #          xaxis = list(title = "Koefisien Produktivitas Lahan"),
      #          yaxis = list(title ="Sektor"))
    }
  } else {
    if(input$pprkWaste == "Angka Pengganda Buangan Limbah"){
      graph <- subset(analysisResult, select = c(Sektor, multiplierWaste))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkWaste == "Koefisien Produk Limbah"){
      graph <- subset(analysisResult, select = c(Sektor, coef_waste))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita') 
    } else if(input$pprkWaste == "Emisi dari Limbah"){
      graph <- subset(analysisResult, select = c(Sektor, em_waste_total))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    }
    
    colnames(graph) <- c("Sektor", "Analisis")
    gplot3<-ggplot(data=graph, aes(x=Sektor, y=Analisis, fill=Sektor)) +
      geom_bar(colour="black", stat="identity") + theme_void() +
      coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
    ggplotly(gplot3)
    # plot_ly(graph, x=~Analisis, y=~Sektor, fill=~Sektor) %>%
    #   add_bars(orientation = 'h',name=~Sektor) %>%
    #   layout(barmode = 'stack',
    #          xaxis = list(title = "Nilai"),
    #          yaxis = list(title ="Sektor"))
  }
})

output$tableDesc <- renderText({
  if(input$categorySector=="Ekonomi"){
    if(input$pprkResults == "PDRB"){
      return(NULL)
    } else if(input$pprkResults == "Backward Linkage"){
      paste0("Direct Backward Linkage (DBL) menunjukkan tingkat keterkaitan kebelakang dari sebuah sektor ekonomi.
               Nilai DBL yang tinggi dari sebuah sektor menunjukkan bahwa sektor tersebut banyak menggunakan output yang dihasilkan oleh sektor lain dalam menghasilkan outputnya sendiri")
    } else if(input$pprkResults == "Forward Linkage"){
      paste0("Direct Forward Linkage (DBL) menunjukkan tingkat keterkaitan kedepan dari sebuah sektor ekonomi.
        Nilai DFL yang tinggi dari sebuah sektor menunjukkan bahwa output dari sektor tersebut banyak digunakan oleh sektor lain.")
    } else if(input$pprkResults == "Angka Pengganda Output"){
      paste0("Angka Pengganda Output menunjukkan dampak perubahan permintaan akhir sebuah sektor terhadap total output masing-masing sektor di sebuah daerah.
        Angka Pengganda Output yang tinggi menunjukkan seberapa besarnya pengaruh sebuah sektor terhadap kondisi perekonomian daerah")
    } else if(input$pprkResults == "Angka Pengganda Pendapatan Rumah Tangga"){
      paste0("Angka Pengganda Pendapatan Rumah Tangga menunjukkan dampak perubahan permintaan akhir sebuah sektor terhadap total income yang dihasilkan masing-masing sektor di sebuah daerah.")
    } else if(input$pprkResults == "Angka Pengganda Tenaga Kerja"){
      paste0("Angka Pengganda Tenaga Kerja menunjukkan dampak perubahan permintaan akhir sebuah sektor ekonomi terhadap penyerapan tenaga kerja suatu provinsi.")
    } else if(input$pprkResults == "Upah gaji"){
      return(NULL)
    } else if(input$pprkResults == "Rasio Upah gaji per Surplus Usaha"){
      return(NULL)
    } else if(input$pprkResults == "Pendapatan per kapita"){
      return(NULL)
    } else if(input$pprkResults == "Perbandingan Angka Pengganda"){
      
    }
  } else if(input$categorySector=="Energi"){
    
  } else if(input$categorySector=="Lahan"){
    
  } else {
    
  }
})

output$tableResults <- renderDataTable({
  if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
  analysisResult <- sec$result
  landTable_his <- sec$landTable_his
  
  if(input$categorySector=="Ekonomi"){
    if(input$pprkResults == "PDRB"){
      tables <- subset(analysisResult, select = c(Sektor, GDP))
      tables
    } else if(input$pprkResults == "Backward Linkage"){
      tables <- subset(analysisResult, select = c(Sektor, DBL))
      tables
    } else if(input$pprkResults == "Forward Linkage"){
      tables <- subset(analysisResult, select = c(Sektor, DFL))
      tables
    } else if(input$pprkResults == "Angka Pengganda Output"){
      tables <- subset(analysisResult, select = c(Sektor, multiplierOutput))
      tables
    } else if(input$pprkResults == "Angka Pengganda Pendapatan Rumah Tangga"){
      tables <- subset(analysisResult, select = c(Sektor, multiplierIncome))
      tables
    } else if(input$pprkResults == "Angka Pengganda Tenaga Kerja"){
      tables <- subset(analysisResult, select = c(Sektor, multiplierLabour))
      tables
    } else if(input$pprkResults == "Upah gaji"){
      tables <- subset(analysisResult, select = c(Sektor, wages))
      tables
    } else if(input$pprkResults == "Rasio Upah gaji per Surplus Usaha"){
      tables <- subset(analysisResult, select = c(Sektor, ratio_ws))
      tables
    } else if(input$pprkResults == "Pendapatan per kapita"){
      return(NULL)
    } else if(input$pprkResults == "Perbandingan Angka Pengganda"){
      tables <- multiplierTable <- subset(analysisResult, select = c(Sektor, multiplierIncome, multiplierOutput, multiplierLabour, multiplierEnergy, multiplierWaste)) 
      tables
    }
  } else if(input$categorySector=="Energi"){
    if(input$pprkEnergy == "Angka Pengganda Energi"){
      tables <- subset(analysisResult, select = c(Sektor, multiplierEnergy))
      tables
    } else if(input$pprkEnergy == "Koefisien Intensitas Energi"){
      tables <- subset(analysisResult, select = c(Sektor, coef_energy))
      tables
    } else if(input$pprkEnergy == "Emisi dari Penggunaan Energi"){
      tables <- subset(analysisResult, select = c(Sektor, em_energy_total))
      tables
    }
  } else if (input$categorySector=="Lahan"){
    if(input$pprkLand == "Matriks Distribusi Lahan"){
      # removeUI(selector = '#plotlyResults') 
      tables <- subset(landTable_his <- sec$landTable_his, select=-Kategori)
      tables
    } else if(input$pprkLand == "Koefisien Kebutuhan Lahan") {
      tables <- subset(landTable_his <- sec$landTable_his, select=c(Sektor, LRC, Kategori))
      tables
    } else if(input$pprkLand == "Koefisien Produktivitas Lahan") {
      tables <- subset(landTable_his <- sec$landTable_his, select=c(Sektor, LPC, Kategori))
      tables
    } else {
      # removeUI(selector = '#plotlyResults')
      tables <- landTable_his <- sec$landTable_his[,c("Sektor", colnames(landTable_his <- sec$landTable_his)[ncol(landTable_his <- sec$landTable_his)-2])]
      tables
    }
  } else {
    if(input$pprkWaste == "Angka Pengganda Buangan Limbah"){
      tables <- subset(analysisResult, select = c(Sektor, multiplierWaste))
      tables
    }  else if(input$pprkWaste == "Koefisien Produk Limbah"){
      tables <- subset(analysisResult, select = c(Sektor, coef_waste))
      tables
    }  else if(input$pprkWaste == "Emisi dari Limbah"){
      tables <- subset(analysisResult, select = c(Sektor, em_waste_total))
      tables
    } 
  }
  datatable(tables, extensions = "FixedColumns", options=list(pageLength=100, scrollX=TRUE, scrollY="70vh", fixedColumns=list(leftColumns=1)), rownames=FALSE, height=540) %>%
    formatRound(columns=c(1:length(tables)),2) %>%
    formatStyle(colnames(tables)[2], background = styleColorBar(tables[,2], 'lightblue'), backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center')
}) #extensions = "FixedColumns", options=list(pageLength=50,scrollX=TRUE, scrollY="600px", fixedColumns=list(leftColumns=1)), rownames=FALSE)

output$downloadTable <- downloadHandler(
  filename = input$pprkResults,
  contentType = "text/csv",
  content = function(file) {
    if(debugMode){
      sec <- blackBoxInputs()
    } else {
      sec <- allInputs()
    }
    analysisResult <- sec$result
    landTable_his <- sec$landTable_his
    
    if(input$categorySector=="Ekonomi"){
      if(input$pprkResults == "PDRB"){
        tables <- subset(analysisResult, select = c(Sektor, GDP))
      } else if(input$pprkResults == "Backward Linkage"){
        tables <- subset(analysisResult, select = c(Sektor, DBL))
      } else if(input$pprkResults == "Forward Linkage"){
        tables <- subset(analysisResult, select = c(Sektor, DFL))
      } else if(input$pprkResults == "Angka Pengganda Output"){
        tables <- subset(analysisResult, select = c(Sektor, multiplierOutput))
      } else if(input$pprkResults == "Angka Pengganda Pendapatan Rumah Tangga"){
        tables <- subset(analysisResult, select = c(Sektor, multiplierIncome))
      } else if(input$pprkResults == "Angka Pengganda Tenaga Kerja"){
        tables <- subset(analysisResult, select = c(Sektor, multiplierLabour))
      } else if(input$pprkResults == "Upah gaji"){
        tables <- subset(analysisResult, select = c(Sektor, wages))
      } else if(input$pprkResults == "Rasio Upah gaji per Surplus Usaha"){
        tables <- subset(analysisResult, select = c(Sektor, ratio_ws))
      } else if(input$pprkResults == "Pendapatan per kapita"){
        tables <- data.frame(NODATA="")
      } else if(input$pprkResults == "Perbandingan Angka Pengganda"){
        tables <- data.frame(NODATA="")
      }
    } else if(input$categorySector=="Energi"){
      if(input$pprkResults == "Angka Pengganda Energi"){
        tables <- subset(analysisResult, select = c(Sektor, multiplierEnergy))
      } else if(input$pprkResults == "Koefisien Intensitas Energi"){
        tables <- subset(analysisResult, select = c(Sektor, coef_energy))
      } else if(input$pprkResults == "Emisi dari Penggunaan Energi"){
        tables <- subset(analysisResult, select = c(Sektor, em_energy_total))
      } 
    } else if (input$categorySector== "Lahan"){
      if(input$pprkLand == "Matriks Distribusi Lahan"){
        tables <- subset(landTable_his, select=-Kategori)
      } else if(input$pprkLand == "Koefisien Kebutuhan Lahan") {
        tables <- subset(landTable_his, select=c(Sektor, LRC, Kategori))
      } else if(input$pprkLand == "Koefisien Produktivitas Lahan") {
        tables <- subset(landTable_his, select=c(Sektor, LPC, Kategori))
      } else {
        # removeUI(selector = '#plotlyResults')
        tables <- landTable_his[,c("Sektor", colnames(landTable_his)[ncol(landTable_his)-2])]
      }
    } else {
      if(input$pprkResults == "Angka Pengganda Buangan Limbah"){
        tables <- subset(analysisResult, select = c(Sektor, multiplierWaste))
      } else if(input$pprkResults == "Koefisien Produk Limbah"){
        tables <- subset(analysisResult, select = c(Sektor, coef_waste))
      } else if(input$pprkResults == "Emisi dari Limbah"){
        tables <- subset(analysisResult, select = c(Sektor, em_waste_total))
      }
    }
    write.table(tables, file, quote=FALSE, row.names=FALSE, sep=",")
  }
)

output$downloadReport <- downloadHandler(
  filename = "report.doc",
  content = function(file){
    file.copy(paste0("data/", allDataProv$prov, "/", allDataProv$prov, "_analisa_deskriptif.doc"), file)
  }
)

# output$tableIO <- renderDataTable({
#   if(debugMode){
#     sec <- blackBoxInputs()
#   } else {
#     sec <- allInputs()
#   }
#   sector <- sec$sector
#   indem <- sec$indem
#   findem <- sec$findem
#   addval <- sec$addval
#   findemcom <- sec$findemcom
#   addvalcom <- sec$addvalcom
#   
#   io_table <- cbind(as.data.frame(sector[,1]), indem)
#   colnames(io_table) <- c("Sektor", t(as.data.frame(sector[,1])))
#   io_table$`Total Permintaan Antara` <- rowSums(indem)
#   
#   colnames(findem) <- c(t(findemcom))
#   findem$`Total Permintaan Akhir` <- rowSums(findem)
#   io_table <- cbind(io_table, findem)
#   
#   total_indem <- colSums(indem)
#   out_indem <- sum(total_indem)
#   total_findem <- colSums(findem)
#   out_findem <- sum(total_findem)
#   total_all_indem <- as.data.frame(cbind("JUMLAH INPUT ANTARA", t(total_indem), out_indem, t(total_findem)))
#   
#   colnames(total_all_indem) <- colnames(io_table)
#   io_table<-rbind(io_table, total_all_indem)
#   
#   totalrow_addval <- rowSums(addval)
#   totalcol_addval <- colSums(addval)
#   total_addval <- sum(totalrow_addval)
#   addval_table <- cbind(addvalcom, addval, totalrow_addval)
#   total_addval_table <- as.data.frame(cbind("JUMLAH INPUT", t(totalcol_addval), total_addval))
#   
#   remaining_col <- ncol(io_table) - ncol(total_addval_table) 
#   for(i in 1:remaining_col){
#     eval(parse(text=(paste("addval_table$new_col",  i, "<- ''", sep=""))))
#     eval(parse(text=(paste("total_addval_table$new_col",  i, "<- ''", sep=""))))
#   }
#   colnames(addval_table) <- colnames(io_table)
#   colnames(total_addval_table) <- colnames(io_table)
#   io_table <- rbind(io_table, addval_table, total_addval_table)
#   io_table
#   
#   datatable(io_table, extensions = "FixedColumns", options=list(paging=FALSE, scrollX=TRUE, scrollY='70vh', fixedColumns=list(leftColumns=1)), rownames=FALSE) %>%
#     formatStyle('Sektor',target = "row", backgroundColor = styleEqual(c("JUMLAH INPUT ANTARA"), c('orange'))) %>%
#     formatStyle(columns = "Total Permintaan Antara", target = "cell", backgroundColor = "#F7080880") %>%
#     formatRound(columns=c(1:length(io_table)),2)
# })
# 
# output$SatelitTenagaKerja <- renderDataTable({
#   if(debugMode){
#     sec <- blackBoxInputs()
#   } else {
#     sec <- allInputs()
#   }
#   labour <- sec$labour
# }, options=list(paging=FALSE, scrollY='70vh'))

output$SatelitEnergi <- renderDataTable({
  if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
  energy <- sec$energy
}, extensions = "FixedColumns", options=list(paging = FALSE, scrollY='70vh', scrollX=TRUE, fixedColumns=list(leftColumns=3)))  

output$SatelitLimbah <- renderDataTable({
  if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
  waste <- sec$waste
}, extensions = "FixedColumns", options=list(paging = FALSE, scrollY='70vh', scrollX=TRUE, fixedColumns=list(leftColumns=3)))

output$SatelitLahan <- renderDataTable({
  if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
  LDM <- sec$LDM
}, extensions = "FixedColumns", options=list(paging = FALSE, scrollY='70vh', scrollX=TRUE, fixedColumns=list(leftColumns=3)))

output$TutupanLahan <- renderDataTable({
  if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
  landcover <- sec$landcover
}, extensions = "FixedColumns", options=list(paging = FALSE, scrollY='70vh', scrollX=TRUE, fixedColumns=list(leftColumns=3)))

output$tableIO <- renderDataTable({

  test <- read_excel("data/test.xlsx")
  test
})

output$SatelitTenagaKerja <- renderDataTable({
  test <- read_excel("data/test.xlsx")
  test
})
