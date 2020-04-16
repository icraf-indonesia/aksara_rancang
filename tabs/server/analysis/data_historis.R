# debug mode
debugMode <- 1
notif_id <- NULL

provList <- readRDS("data/provList")
# usersList <- load("usersList")

LDMProp_new<-reactiveValues(
  tablo = NULL,
  coba= NULL
)

tabel<-reactiveValues(
  manualSave=NULL
)

ldmRV<-reactiveValues(
  LDMListFile = unique(list.files(paste0("LDMData/Prov/"))),   # ganti mas alfa
  LDMTotFile= unique(length(list.files("LDMData/Prov/")))   # ganti mas alfa
)

editable<-reactiveValues(
  BAULahan_landCover=NULL
)

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

###*historical input####
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

output$tableIO <- renderDataTable({

  test <- read_excel("data/test.xlsx")
  test
  # if(debugMode){
  #   sec <- blackBoxInputs()
  # } else {
  #   sec <- allInputs()
  # }
  # sector <- sec$sector
  # indem <- sec$indem
  # findem <- sec$findem
  # addval <- sec$addval
  # findemcom <- sec$findemcom
  # addvalcom <- sec$addvalcom
  # 
  # io_table <- cbind(as.data.frame(sector[,1]), indem)
  # colnames(io_table) <- c("Sektor", t(as.data.frame(sector[,1])))
  # io_table$`Total Permintaan Antara` <- rowSums(indem)
  # 
  # colnames(findem) <- c(t(findemcom))
  # findem$`Total Permintaan Akhir` <- rowSums(findem)
  # io_table <- cbind(io_table, findem)
  # 
  # total_indem <- colSums(indem)
  # out_indem <- sum(total_indem)
  # total_findem <- colSums(findem)
  # out_findem <- sum(total_findem)
  # total_all_indem <- as.data.frame(cbind("JUMLAH INPUT ANTARA", t(total_indem), out_indem, t(total_findem)))
  # 
  # colnames(total_all_indem) <- colnames(io_table)
  # io_table<-rbind(io_table, total_all_indem)
  # 
  # totalrow_addval <- rowSums(addval)
  # totalcol_addval <- colSums(addval)
  # total_addval <- sum(totalrow_addval)
  # addval_table <- cbind(addvalcom, addval, totalrow_addval)
  # total_addval_table <- as.data.frame(cbind("JUMLAH INPUT", t(totalcol_addval), total_addval))
  # 
  # remaining_col <- ncol(io_table) - ncol(total_addval_table) 
  # for(i in 1:remaining_col){
  #   eval(parse(text=(paste("addval_table$new_col",  i, "<- ''", sep=""))))
  #   eval(parse(text=(paste("total_addval_table$new_col",  i, "<- ''", sep=""))))
  # }
  # colnames(addval_table) <- colnames(io_table)
  # colnames(total_addval_table) <- colnames(io_table)
  # io_table <- rbind(io_table, addval_table, total_addval_table)
  # io_table
  # 
  # datatable(io_table, extensions = "FixedColumns", options=list(paging=FALSE, scrollX=TRUE, scrollY='70vh', fixedColumns=list(leftColumns=1)), rownames=FALSE) %>%
  #   formatStyle('Sektor',target = "row", backgroundColor = styleEqual(c("JUMLAH INPUT ANTARA"), c('orange'))) %>%
  #   formatStyle(columns = "Total Permintaan Antara", target = "cell", backgroundColor = "#F7080880") %>%
  #   formatRound(columns=c(1:length(io_table)),2)
})

output$SatelitTenagaKerja <- renderDataTable({
  test <- read_excel("data/test.xlsx")
  test
})

# ##-- Atualizações dos filtros ----
# ##-- + Atualizações os anos ----
# observeEvent(input$eleicoes_cargo_br,{
#   cargo <- isolate(input$eleicoes_cargo_br)
#   
#   if(!is.null(cargo)){
#     chaves_sub <- chaves %>%
#       filter(CODIGO_CARGO == cargo) 
#     
#     ##-- Setando o cargo default
#     anos <- sort(unique(chaves_sub$ANO_ELEICAO))
#     ano_default <- input$eleicoes_ano_br
#     
#     if(!(ano_default %in% anos)){
#       ano_default <- anos[1]
#     }
#     
#     ##-- Atualizando os cargos ----
#     updatePickerInput(session = session,
#                       inputId = "eleicoes_ano_br",
#                       label = "Year", 
#                       choices = anos, 
#                       selected = ano_default)
#     
#   }
#   
# }, priority = 1)
# ##-- + Atualizações dos turnos ----
# observeEvent(c(input$eleicoes_ano_br, 
#                input$eleicoes_cargo_br),{
#                  
#                  ano <- isolate(input$eleicoes_ano_br)
#                  cargo <- isolate(input$eleicoes_cargo_br)
#                  
#                  if(!is.null(cargo)){
#                    chaves_sub <- chaves %>%
#                      filter(ANO_ELEICAO == ano & CODIGO_CARGO == cargo)
#                    
#                    ##-- Setando o cargo default
#                    turnos <- unique(chaves_sub$NUM_TURNO)
#                    turno_default <- input$eleicoes_turno_br
#                    
#                    if(!(turno_default %in% paste0(turnos, "º round")) | length(turnos) == 0){
#                      turno_default <- "1º round"
#                    }
#                    
#                    if(length(turnos) == 0){
#                      turnos <- ""
#                    } else{
#                      turnos <- paste0(turnos, "º round")
#                    }
#                    
#                    ##-- Atualizando os turnos ----
#                    updatePickerInput(sessio = session,
#                                      inputId = "eleicoes_turno_br", 
#                                      label = "Round", 
#                                      choices = turnos, 
#                                      selected = turno_default)
#                  }
#                  
#                }, priority = 2)
# ##-- + Atualizações dos estados ----
# observeEvent(c(input$eleicoes_ano_br, 
#                input$eleicoes_cargo_br, 
#                input$eleicoes_turno_br),{
#                  
#                  ano <- isolate(input$eleicoes_ano_br)
#                  cargo <- isolate(input$eleicoes_cargo_br)
#                  turno <- isolate(input$eleicoes_turno_br)
#                  turno <- ifelse(turno != "1º round", "2", "1")
#                  
#                  if(!is.null(turno)){
#                    chaves_sub <- chaves %>%
#                      filter(ANO_ELEICAO == ano & NUM_TURNO == turno)
#                    ##-- Setando o estado default
#                    estados <- levels(factor(x = sort(unique(chaves_sub$UF)),
#                                              levels = sort(unique(chaves_sub$UF))))
#                    estado_default <- input$eleicoes_estado_br
#                    
#                    if(!(estado_default %in% estados)){
#                      estado_default <- "AC"
#                    }
#                    
#                    ##-- Atualizando os partidos ----
#                    updatePickerInput(session = session,
#                                      inputId = "eleicoes_estado_br",
#                                      label = "State", 
#                                      choices = estados, 
#                                      selected = estado_default)  
#                  }
#                  
#                }, priority = 3)
# ##-- Reactive para os dados ----
# dados_eleicao_geral_br <- reactive({
#   ##-- + Inputs ----
#   ano <- input$eleicoes_ano_br
#   cargo <- input$eleicoes_cargo_br
#   turno <- input$eleicoes_turno_br
#   turno <- ifelse(turno != "1º round", "2", "1")
#   
#   ##-- + Selecionando os dados ----
#   dados <- dados_gerais %>% filter(ANO_ELEICAO == ano & CODIGO_CARGO == cargo & NUM_TURNO == turno)
#   
#   return(dados)
# })
# ##-- Reactive para gerar as visualizações ----
# graficos_eleicao_geral_br <- eventReactive(input$eleicoes_gerar_visualizacoes_br, {
#   dados <- dados_eleicao_geral_br()
#   
#   cod_uf <- input$eleicoes_estado_br
#   if(cod_uf == "All states") cod_uf <- NULL
#   
#   graficos <- list()
#   
#   names(regUF)[c(1, 3)] <- c("UF", "REG")
#   graficos[[1]] <- mapa_uf(data = dados, poly = regUF)
#   graficos[[2]] <- mapa_mun(data = dados, poly = regMun, uf = cod_uf)
#   graficos[[3]] <- bar_plot(data = dados, uf = cod_uf, value_var = "QTDE_VOTOS_TOT", group_var = "NOME_URNA_CANDIDATO")
#   
#   names(graficos) <- c("mapa_uf_br", "mapa_mun_br", "bar_plot_br")
#   
#   return(graficos)
#   
# })
# ##-- Mapa dos candidatos à presidência por estados ----
# output$mapa_uf_geral_br <- renderLeaflet({
#   graficos_eleicao_geral_br()$mapa_uf_br
# })
# ##-- Mapa dos candidatos municipais ----
# output$mapa_mun_geral_br <- renderLeaflet({
#   graficos_eleicao_geral_br()$mapa_mun_br
# })
# ##-- Gráfico de barras com o percentual de votos por candidato ----
# output$barras_geral_br <- renderPlotly({
#   graficos_eleicao_geral_br()$bar_plot_br
# })