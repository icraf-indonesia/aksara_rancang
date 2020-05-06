### Server: Data Historis ###

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
  ioSector = NULL,
  ioIntermediateDemand = NULL,
  ioFinalDemand = NULL,
  ioAddedValue = NULL,
  satelliteLabour = NULL,
  satelliteEnergy = NULL,
  satelliteWaste = NULL,
  satelliteAgriculture = NULL,
  emissionFactorEnergy = NULL,
  emissionFactorWaste = NULL,
  ioFinalDemandComponent = NULL,
  ioAddedValueComponent = NULL,
  populationProjection = NULL,
  baselineEmission = NULL,
  LU_tahun=NULL,
  LDMProp_his=NULL,
  LDMProp=NULL,
  # landDemand = NULL,
  # landDemand_prop = NULL,
  ioLeontif = NULL,
  ioLeontiefInverse = NULL,
  GDPAll = NULL,
  linkagesTable = NULL,
  multiplierAll = NULL,
  ioPeriod = NULL,
  rtffile = NULL,
  growthRate = NULL,
  prk_scenario = data.frame(time=NULL, action=NULL, year=NULL, username=NULL, provinsi=NULL, sector=NULL, fd_value=NULL),
  ### Start : Land Section ###
  LUTMDatabase = NULL,
  LUTMTemplate_his = NULL,
  LRCRate_his = NULL,
  LRCRate_2 = NULL,
  carbonStock_his = NULL
  ### End : Land Section ###
)

finalResults <- reactiveValues(table1=NULL, plot23=NULL, plot24=NULL, plot25=NULL)

historicalResults <- reactiveValues()
bauResults <- reactiveValues()
interventionResults <- reactiveValues()

userActivities <- reactiveValues(
  latestAct= NULL, message=NULL, dateTime=NULL, listOfActs = data.frame(latestAct=NULL, message=NULL, dateTime=NULL)
)

recordActivities <- function(latestAct, message, dateTime){
  userActivities$latestAct<-latestAct
  userActivities$message<-message
  userActivities$dateTime=dateTime
  userActivities$listOfActs<-rbind(userActivities$listOfActs,data.frame(latestAct=NULL, message=NULL, dateTime=NULL))
}

observeEvent(input$inputLogin, {
  fullname <- input$fullname
  username <- input$username
  password <- input$password
  selectedProv <- input$categoryProvince
  # print(selectedProv)
  
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
  
  datapath <- paste0("data/", selectedProv, "/")
  userFolder <- paste0(datapath, username)
  if(!dir.exists(userFolder)) dir.create(userFolder, mode = 777)
  # system(paste0("chmod -R 777 ", userFolder))
  
  ioSector <- readRDS(paste0(datapath, "sector"))
  ioIntermediateDemand <- readRDS(paste0(datapath, "indem"))
  ioFinalDemand <- readRDS(paste0(datapath, "findem"))
  ioAddedValue <- readRDS(paste0(datapath, "addval"))
  satelliteLabour <- readRDS(paste0(datapath, "labour"))
  satelliteEnergy <- readRDS(paste0(datapath, "energy"))
  satelliteWaste <- readRDS(paste0(datapath, "waste"))
  satelliteAgriculture <- readRDS(paste0(datapath, "agriculture"))
  # satelliteAgriculture <- read.table("D:/YUMNA/ICRAF/icraf-indonesia/aksara_rancang/data/JaBar/satellite_pertanian.csv", header = T, sep = ",")
  emissionFactorEnergy <- readRDS(paste0(datapath, "ef_energy"))
  emissionFactorWaste <- readRDS(paste0(datapath, "ef_waste"))
  # emissionFactorAgriculture <- read.table("D:/My_Development/RProjects/lcd-scenario/raw/jabar_in_redcluwe/17_faktor_emisi_pertanian.csv", header = T, sep = ",")
  ioFinalDemandComponent <- readRDS(paste0(datapath, "findemcom"))
  ioAddedValueComponent <- readRDS(paste0(datapath, "addvalcom"))
  population <- readRDS(paste0(datapath, "currentPopulation"))
  populationProjection <- readRDS(paste0(datapath, "population"))
  baselineEmission <- readRDS(paste0(datapath, "otherEm"))
  LU_tahun<-readRDS(paste0(datapath,"LU_tahun"))
  # print(LU_tahun)
  LDMProp_his<-readRDS(paste0(datapath,"LDMProp"))
  row.names(LDMProp_his)<-ioSector[,1]
  # landDemand <- readRDS(paste0(datapath, "landDemand"))
  # landDemand_prop <- readRDS(paste0(datapath, "landDemand_prop"))
  # landtable <- readRDS(paste0(datapath, "landtable"))
  ioLeontif <- readRDS(paste0(datapath, "I_A"))
  ioLeontiefInverse <- readRDS(paste0(datapath, "leontief"))
  GDPAll <- readRDS(paste0(datapath, "GDPAll"))
  linkagesTable <- readRDS(paste0(datapath, "linkagesTable"))
  multiplierAll <- readRDS(paste0(datapath, "multiplierAll"))
  ioPeriod <- readRDS(paste0(datapath, "periodIO"))
  # print(ioPeriod)
  rtffile <- readRDS(paste0(datapath, "rtffile"))
  
  # growthRate <- read.table("_DB/jabar_in_redcluwe/growth5_1630.csv", header = T, sep = ",")
  
  ### Start : Land Section ###
  # datapath <- paste0("data/", selectedProv, "/")
  LUTMDatabase<-as.data.frame(read.csv("data/LUTMDatabaseID.csv"))
  LDMProp_his<-read.csv(paste0(datapath, "LDMProp.csv"))
  LUTMTemplate_his<-read.csv(paste0(datapath,"LUTM_template.csv"))
  LRCRate_his<-read.csv(paste0(datapath,"LRCRate.csv"),header = FALSE)
  LRCRate_2<-read.csv(paste0(datapath,"LRCRate_2.csv"),header=FALSE)
  # LRCRate_2<-read.csv(paste0(datapath,"LRCRate.csv"),header=FALSE)  #delete after use
  carbonStock_his<-data.matrix(read.csv(paste0(datapath,"carbonStock.csv")))
  carbonStock_his<-as.matrix(carbonStock_his[,3])
  ### End : Land Section ###
  
  allDataProv$username = username
  allDataProv$selectedProv = selectedProv
  allDataProv$ioSector = ioSector
  allDataProv$ioIntermediateDemand = ioIntermediateDemand
  allDataProv$ioFinalDemand = ioFinalDemand
  allDataProv$ioAddedValue = ioAddedValue
  allDataProv$satelliteLabour = satelliteLabour
  allDataProv$satelliteEnergy = satelliteEnergy
  allDataProv$satelliteWaste = satelliteWaste
  allDataProv$satelliteAgriculture = satelliteAgriculture
  allDataProv$emissionFactorEnergy = emissionFactorEnergy
  allDataProv$emissionFactorWaste = emissionFactorWaste
  allDataProv$ioFinalDemandComponent = ioFinalDemandComponent
  allDataProv$ioAddedValueComponent = ioAddedValueComponent
  allDataProv$populationProjection = populationProjection
  allDataProv$population = population
  allDataProv$baselineEmission = baselineEmission
  allDataProv$LU_tahun = LU_tahun
  allDataProv$LDMProp_his = LDMProp_his
  allDataProv$LDMProp = data.frame()
  # allDataProv$landDemand = landDemand
  # allDataProv$landDemand_prop = landDemand_prop
  allDataProv$ioLeontif = ioLeontif
  allDataProv$ioLeontiefInverse = ioLeontiefInverse
  allDataProv$GDPAll = GDPAll
  allDataProv$linkagesTable = linkagesTable
  allDataProv$multiplierAll = multiplierAll
  allDataProv$ioPeriod = ioPeriod
  allDataProv$rtffile = rtffile
  allDataProv$growthRate = data.frame(Lapangan_usaha=as.character(ioSector[,1]))
  ### Start : Land Section ###
  allDataProv$LUTMDatabase = LUTMDatabase
  allDataProv$LUTMTemplate_his = LUTMTemplate_his
  allDataProv$LRCRate_his = LRCRate_his
  allDataProv$LRCRate_2 = LRCRate_2
  allDataProv$carbonStock_his = carbonStock_his
  ### End : Land Section ###
  
  recordActivities("Login redcluwe.id", "Berhasil", paste0(Sys.time()))
  
  notif_id <<- showNotification("Anda berhasil masuk", duration = 4, closeButton = TRUE, type = "warning")
  updateTabItems(session, "tabs", selected = "pageOne")
})

blackBoxInputs <- function(){
  ioSector <- allDataProv$ioSector
  ioIntermediateDemand <- allDataProv$ioIntermediateDemand
  ioFinalDemand <- allDataProv$ioFinalDemand
  ioAddedValue <- allDataProv$ioAddedValue
  satelliteLabour <- allDataProv$satelliteLabour
  satelliteEnergy <- allDataProv$satelliteEnergy
  satelliteWaste <- allDataProv$satelliteWaste
  satelliteAgriculture <- allDataProv$satelliteAgriculture
  emissionFactorEnergy <- allDataProv$emissionFactorEnergy
  emissionFactorWaste <- allDataProv$emissionFactorWaste
  ioFinalDemandComponent <- allDataProv$ioFinalDemandComponent
  ioAddedValueComponent <- allDataProv$ioAddedValueComponent
  populationProjection <- allDataProv$populationProjection
  population <- allDataProv$population
  baselineEmission <- allDataProv$baselineEmission
  # LU_tahun <- allDataProv$LU_tahun
  LDMProp_his <- allDataProv$LDMProp_his
  # row.names(LDMProp_his)<-ioSector[,1]
  LDMProp <- allDataProv$LDMProp
  # landDemand <- allDataProv$landDemand
  # landDemand_prop <- allDataProv$landDemand_prop
  # landtable <- allDataProv$landtable
  ioLeontif <- allDataProv$ioLeontif
  ioLeontiefInverse <- allDataProv$ioLeontiefInverse
  GDPAll <- allDataProv$GDPAll
  linkagesTable <- allDataProv$linkagesTable
  multiplierAll <- allDataProv$multiplierAll
  ioPeriod <- allDataProv$ioPeriod
  rtffile <- allDataProv$rtffile
  ### Start : Land Section ###
  LUTMDatabase <- allDataProv$LUTMDatabase
  LUTMTemplate_his <- allDataProv$LUTMTemplate_his
  LRCRate_his <- allDataProv$LRCRate_his
  LRCRate_2 <- allDataProv$LRCRate_2
  carbonStock_his <-allDataProv$carbonStock_his
  ### End : Land Section ###
  
  # Row explicit definition for Income (Wages & Salary)
  
  matrixIoIntermediateDemand <- as.matrix(ioIntermediateDemand) #indem_matrix
  matrixIoAddedValue <- as.matrix(ioAddedValue) #addval_matrix
  nrowMatrixIoAddedValue <- nrow(matrixIoAddedValue) #num_addval
  ioDimention <- ncol(ioIntermediateDemand) #dimensi
  
  matrixIoFinalDemand <- as.matrix(ioFinalDemand)
  rowSumsMatrixIoFinalDemand <- as.matrix(rowSums(matrixIoFinalDemand))
  proportionFinalDemand <- ioFinalDemand/rowSumsMatrixIoFinalDemand
  proportionFinalDemand[is.na(proportionFinalDemand)] <- 0
  
  colSumsMatrixIoIntermediateDemand <- colSums(matrixIoIntermediateDemand) #indem_colsum
  colSumsMatrixIoAddedValue <- colSums(matrixIoAddedValue) #addval_colsum
  ioTotalOutput <- colSumsMatrixIoIntermediateDemand + colSumsMatrixIoAddedValue # ioTotalInput (total_output <- indem_colsum+addval_colsum)
  ioTotalOutputInverse <- 1/ioTotalOutput #fin_con
  ioTotalOutputInverse[is.infinite(ioTotalOutputInverse)] <- 0
  ioTotalOutputInverse <- diag(ioTotalOutputInverse) #tinput_invers
  
  rowImport <- 1
  rowIncome <- 2 #income_row <- 2
  rowProfit <- 3
  
  initialYear <- 2016
  finalYear <- 2030
  iteration <- finalYear - initialYear
  
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
  ###END: initiate ####
  
  ###BEGIN: regional economic impact analysis & historical emission from satellite account####
  # Direct Backward Linkage
  analysisDBL <- colSums(ioLeontiefInverse) #DBL
  analysisBPD <- analysisDBL/(mean(analysisDBL))
  
  # Direct Forward Linkage
  analysisDFL <- rowSums(ioLeontiefInverse) #DFL
  analysisFPD <- analysisDFL/(mean(analysisDFL))
  
  # GDP
  analysisGDP <- colSums(matrixIoAddedValue[rowIncome:nrowMatrixIoAddedValue,]) #GDP
  analysisTotalGDP <- sum(analysisGDP)
  
  # Multiplier Output (MO)
  analysisMO <- colSums(ioLeontiefInverse) #multiplierOutput
  
  # Coefficient Income (CI) & Multiplier Income (MI)
  analysisCI <- as.matrix(matrixIoAddedValue[rowIncome,]) / ioTotalOutput #income_coef
  analysisMI <- ioLeontiefInverse %*% analysisCI #multiplierIncome
  analysisMI[is.na(analysisMI)] <- 0
  
  # Coefficient Labour (CL) & Multiplier Labour (ML)
  analysisCL <- as.matrix(satelliteLabour[,3]) / ioTotalOutput #labour_coef
  analysisML <- ioLeontiefInverse %*% analysisCL #multiplierLabour
  analysisML[is.na(analysisML)] <- 0
  
  # Coefficient Energy Used (CE) & Multiplier Energy (ME)
  analysisCE <- as.matrix(satelliteEnergy[,3]) / ioTotalOutput #energy_coef
  analysisME <- ioLeontiefInverse %*% analysisCE #multiplierEnergy
  analysisME[is.na(analysisME)] <- 0
  
  # Coefficient Waste Product (CW) & Multiplier Waste (MW) 
  analysisCW <- as.matrix(satelliteWaste[,3]) / ioTotalOutput #waste_coef
  analysisMW <- ioLeontiefInverse %*% analysisCW #multiplierWaste
  analysisMW[is.na(analysisMW)] <- 0
  
  # # Coefficient Agriculture-Fertilizer (CA) & Multiplier Agriculture-Fertilizer (MA)
  # analysisCA <- as.matrix(satelliteAgriculture[,3]) / ioTotalOutput
  # analysisMA <- ioLeontiefInverse %*% analysisCA
  # analysisMA[is.na(analysisMA)] <- 0
  
  # Ratio Wages / Business Surplus
  analysisRatioWS <- t(as.matrix(ioAddedValue[2,] / ioAddedValue[3,])) #ratio_ws
  analysisRatioWS[is.na(analysisRatioWS)] <- 0
  analysisRatioWS[analysisRatioWS == Inf] <- 0
  colnames(analysisRatioWS) <- "ratio_ws"
  
  # Satellite account by sectoral GDP
  analysisEnergyByGDP <- as.matrix(satelliteEnergy[,3]) / analysisTotalGDP #coef_energy
  analysisWasteByGDP <- as.matrix(satelliteWaste[,3]) / analysisTotalGDP #coef_waste
  # analysisAgricultureByGDP <- as.matrix(satelliteAgriculture[,3]) / analysisTotalGDP
  
  # Emission from energy
  emissionFactorEnergyDiagonal <- diag(emissionFactorEnergy[,2], ncol = nrow(emissionFactorEnergy), nrow = nrow(emissionFactorEnergy)) #f_energy_diag
  emissionEnergy <- as.matrix(satelliteEnergy[,4:ncol(satelliteEnergy)]) %*% emissionFactorEnergyDiagonal #em_energy
  emissionEnergyTotal <- rowSums(emissionEnergy) #em_energy_total
  
  # Emission from waste
  emissionFactorWasteDiagonal <- diag(emissionFactorWaste[,2], ncol = nrow(emissionFactorWaste), nrow = nrow(emissionFactorWaste)) #f_waste_diag
  emissionWaste <- as.matrix(satelliteWaste[,4:ncol(satelliteWaste)]) %*% emissionFactorWasteDiagonal #em_waste
  emissionWasteTotal <- rowSums(emissionWaste) #em_waste_total
  
  # # Emission from agriculture-fertilizer
  # emissionFactorAgricultureDiagonal <- diag(emissionFactorAgriculture[,2], ncol = nrow(emissionFactorAgriculture), nrow = nrow(emissionFactorAgriculture))
  # emissionAgriculture <- as.matrix(satelliteAgriculture[,4:ncol(satelliteAgriculture)]) %*% emissionFactorAgricultureDiagonal
  # emissionAgricultureTotal <- rowSums(emissionAgriculture)
  
  # Wages
  analysisWages <- as.matrix(t(ioAddedValue[2,])) #wages
  colnames(analysisWages) <- "wages"
  
  # Income per capita
  analysisIncomePerCapita <- sum(as.matrix(matrixIoAddedValue[rowIncome,])) / population #income_per_capita
  
  # Coefficient technology (intermediate demand) or A
  analysisCT <- t( t(matrixIoIntermediateDemand) / ioTotalOutput)
  
  # Coefficient primary input
  analysisCPI <- t(t(ioAddedValue) / ioTotalOutput)
  
  ###END: analysis ####
  
  # # for calculate landTable, LPC, LRC historis     # TIN CEK, variabel u/ proyeksi bau ekonomi berdasarkan tutupan lahan
  # LU_tahun<-as.data.frame(LU_tahun[,1:ncol(LU_tahun)])
  # LU_tahun<-as.matrix(LU_tahun)
  # LDMProp_his<-as.matrix(LDMProp_his[,2:ncol(LDMProp_his)])
  # GDPAll<-as.data.frame(GDPAll)
  # diagLU_his<-as.matrix(diag(LU_tahun[,1]))
  # landTable_his<-LDMProp_his %*% diagLU_his
  # landReq_his<-as.matrix(rowSums(landTable_his))
  # 
  # LPC_his<-GDPAll[,4]/landReq_his
  # LPC_his[is.infinite(LPC_his)]<-0
  # LRC_his<-1/LPC_his
  # LRC_his[is.infinite(LRC_his)]<-0
  # landTable_his<-cbind(ioSector, landTable_his, landReq_his, LPC_his, LRC_his)
  # # # colnames(landTable_his)<-c("Sektor", "Kategori", colnames(LDMProp_his),"Total Kebutuhan Lahan", "LPC", "LRC")
  # tahun<-as.vector(str_extract_all(colnames(LU_tahun), '[0-9]+'))
  # tahun<-as.data.frame(tahun)
  # tahun<-t(tahun)
  
  ### START : TIN INI DICEK ####
  # land use transition matrix (LUTM) historis
  LUTMDatabase<-LUTMDatabase[LUTMDatabase$Provinsi==paste0(input$categoryProvince),c("Count",paste0("PL", ioPeriod-1, "RCL"), paste0("PL",ioPeriod,"RCL"))]  # tidak perlu di result
  colnames(LUTMDatabase)<-c("COUNT","ID_LC1","ID_LC2")
  tuplaID<- cbind(as.matrix(cbind(matrix(0, nrow=23^2, ncol=1), as.matrix(expand.grid(1:23, 1:23)))))  # tidak perlu di result
  colnames(tuplaID)<-c("COUNT","ID_LC1","ID_LC2")
  LUTMDatabase<-rbind(LUTMDatabase,tuplaID)
  LUTMDatabase<-aggregate(LUTMDatabase, by=list(LUTMDatabase$ID_LC1,LUTMDatabase$ID_LC2), FUN=sum)
  LUTMDatabase<-LUTMDatabase[,1:3]
  colnames(LUTMDatabase)<-c("ID_LC1","ID_LC2","COUNT")
  LUTMDatabase<-LUTMDatabase[LUTMDatabase$ID_LC1>0,]
  LUTMDatabase<-LUTMDatabase[LUTMDatabase$ID_LC2>0,]
  LUTMDatabase <- melt(data = LUTMDatabase, id.vars=c('ID_LC1','ID_LC2'), measure.vars=c('COUNT'))
  LUTM_his <- dcast(data = LUTMDatabase, formula = ID_LC1 ~ ID_LC2, fun.aggregate = sum)
  LUTM_his<-as.matrix(LUTM_his[,-1])
  
  
  # # land cover historis
  landCover_his <- dcast(data = LUTMDatabase, formula = ID_LC2 ~ ., fun.aggregate = sum)
  landCover_his<-as.matrix(landCover_his[,2])
  landCover_his0<- dcast(data = LUTMDatabase, formula = ID_LC1 ~ ., fun.aggregate = sum)
  landCover_his0<-as.matrix(landCover_his0[,2])
  
  #TPM
  TPM<-matrix(nrow=nrow(LUTM_his), ncol=ncol(LUTM_his))
  for (i in 1:ncol(TPM)){
    TPM[,i]<-LUTM_his[,i]/landCover_his0[i,1]   #proporsi semua elemen LUTM dibagi tupla tahun kedua
  }
  TPM[is.nan(TPM)]<-0
  
  #land distribution matrix dalam luas (analysisLDMLuas)
  analysisLDMLuas<-as.matrix(LDMProp_his)%*%as.matrix(diag(landCover_his[,1]))
  
  #land requirement historis (analysisLR)
  landReq_his<-as.matrix(rowSums(analysisLDMLuas))
  
  # Land requirement coefficient (analysisLRC) & land productivity coefficient (analysisLPC)
  analysisLPC<-rbind(as.matrix(rowSums(cbind(ioIntermediateDemand, ioFinalDemand))), 0)/landReq_his    #rowSums(cbind(indem, findem))=output
  analysisLPC[is.infinite(analysisLPC)]<-0
  analysisLPC[is.nan(analysisLPC)]<-0
  analysisLRC<-1/analysisLPC
  analysisLRC[is.infinite(analysisLRC)]<-0
  analysisLRC[is.nan(analysisLPC)]<-0
  
  # land distribution matrix proportion (total sector = 1)
  LDMProp_sektor<-matrix(NA, nrow=ncol(analysisLDMLuas), ncol=nrow(analysisLDMLuas))
  for (i in 1:ncol(LDMProp_sektor)){
    LDMProp_sektor[,i]<-as.matrix(analysisLDMLuas[i,]/sum(analysisLDMLuas[i,]))
  }
  LDMProp_sektor[is.na(LDMProp_sektor)]<-0
  
  
  # LUTM Template
  LUTMTemplate_his<-as.matrix(LUTMTemplate_his)
  for (i in 1:nrow(landCover_his)){
    if (sum(landCover_his[i,])==0){
      LUTMTemplate_his[i,]<-matrix(0,ncol=ncol(LUTMTemplate_his))    #LUTMTemplate bisa diedit di interface
      LUTMTemplate_his[,i]<-matrix(0,nrow=ncol(LUTMTemplate_his))
    } else {}
  }
  LUTMTemplate_his[is.na(LUTMTemplate_his)]<-paste0("x",1:length(LUTMTemplate_his[is.na(LUTMTemplate_his)]))
  
  #### END : TIN INI DICEK ####
  
  result <- cbind(ioSector,
                  analysisBPD,
                  analysisFPD, 
                  analysisGDP, 
                  analysisMO, 
                  analysisMI,
                  analysisML,
                  analysisCL,
                  analysisME,
                  analysisMW,
                  analysisWages,
                  analysisRatioWS, 
                  analysisCE,
                  analysisCW,
                  emissionEnergyTotal,
                  emissionWasteTotal
  )
  colnames(result)[1] <- "Sektor"
  
  list_table <- list(result=result,
                     ioSector=ioSector, 
                     ioIntermediateDemand=ioIntermediateDemand, 
                     ioFinalDemand=ioFinalDemand, 
                     ioAddedValue=ioAddedValue, 
                     satelliteLabour=satelliteLabour, 
                     satelliteEnergy=satelliteEnergy, 
                     ioFinalDemandComponent=ioFinalDemandComponent, 
                     ioAddedValueComponent=ioAddedValueComponent,
                     satelliteWaste=satelliteWaste,
                     satelliteAgriculture=satelliteAgriculture,
                     emissionFactorEnergy=emissionFactorEnergy,
                     emissionFactorWaste=emissionFactorWaste,
                     # landcover=allDataProv$LU_tahun,
                     analysisIncomePerCapita=analysisIncomePerCapita,
                     baselineEmission=baselineEmission,
                     populationProjection=populationProjection,
                     # LU_tahun=LU_tahun,
                     LDMProp=allDataProv$LDMProp_his, #bedanya 1
                     GDPAll=GDPAll,
                     # landTable_t0=landTable_t0,
                     # landReq=landReq,
                     # tahun=tahun, 
                     # landTable_his=landTable_his,
                     baselineEmission=baselineEmission, 
                     LDMProp_his = LDMProp_his, #bedanya 2
                     ### Function in BAU Scenario ###
                     analysisLRC = analysisLRC,
                     landReq_his = landReq_his,
                     landCover_his = landCover_his,
                     LDMProp_sektor = LDMProp_sektor,
                     LUTMTemplate_his = LUTMTemplate_his
  ) 
  
  return(list_table)
}

###*historical input####
allInputs <- eventReactive(input$button, {
  inSector <- input$ioSector
  if(is.null(inSector))
    return(NULL)
  
  inIntermediateDemand <- input$ioIntermediateDemand
  if(is.null(inIntermediateDemand))
    return(NULL)
  
  inFinalDemand <- input$ioFinalDemand
  if(is.null(inFinalDemand))
    return(NULL)
  
  inAddedValue <- input$ioAddedValue
  if(is.null(inAddedValue))
    return(NULL)    
  
  inLabour <- input$satelliteLabour
  if(is.null(inLabour))
    return(NULL)
  
  inEnergy <- input$energyTable
  if(is.null(inEnergy))
    return(NULL) 
  
  inWaste <- input$wasteTable
  if(is.null(inWaste))
    return(NULL)
  
  inAgriculture <- input$agricultureTable
  if(is.null(inAgriculture))
    return(NULL)
  
  inEmissionFactorEnergiTable <- input$emissionFactorEnergiTable
  if(is.null(inEmissionFactorEnergiTable))
    return(NULL)
  
  inEmissionFactorLandWasteTable <- input$emissionFactorLandWasteTable
  if(is.null(inEmissionFactorLandWasteTable))
    return(NULL)
  
  inFinalDemandComp <- input$ioFinalDemandComponent
  if(is.null(inFinalDemandComp))
    return(NULL) 
  
  inAddedValueComp <- input$ioAddedValueComponent
  if(is.null(inAddedValueComp))
    return(NULL)  
  
  ioSector <- read.table(inSector$datapath, header=FALSE, sep=",")
  ioIntermediateDemand <- read.table(inIntermediateDemand$datapath, header=FALSE, sep=",")
  ioFinalDemand <- read.table(inFinalDemand$datapath, header=FALSE, sep=",")
  ioAddedValue <- read.table(inAddedValue$datapath, header=FALSE, sep=",")
  satelliteLabour <- read.table(inLabour$datapath, header=TRUE, sep=",")
  satelliteEnergy <- read.table(inEnergy$datapath, header=TRUE, sep=",")
  satelliteWaste <- read.table(inWaste$datapath, header=TRUE, sep=",")
  satelliteAgriculture <- read.table(inAgriculture$datapath, header=TRUE, sep=",")
  emissionFactorEnergy <- read.table(inEmissionFactorEnergiTable$datapath, header=TRUE, sep=",")
  emissionFactorWaste <- read.table(inEmissionFactorLandWasteTable$datapath, header=TRUE, sep=",")
  ioFinalDemandComponent <- read.table(inFinalDemandComp$datapath, header=FALSE, sep=",")
  ioAddedValueComponent <- read.table(inAddedValueComp$datapath, header=FALSE, sep=",")
  
  # Row explicit definition
  rowImport <- 1
  rowIncome <- 2
  rowProfit <- 3
  
  initialYear <- 2016
  finalYear <- 2030
  iteration <- finalYear - initialYear
  
  matrixIoIntermediateDemand <- as.matrix(ioIntermediateDemand)
  matrixIoAddedValue <- as.matrix(ioAddedValue)
  nrowMatrixIoAddedValue <- nrow(matrixIoAddedValue)
  ioDimention <- ncol(ioIntermediateDemand)
  
  colSumsMatrixIoIntermediateDemand <- colSums(matrixIoIntermediateDemand)
  colSumsMatrixIoAddedValue <- colSums(matrixIoAddedValue)
  ioTotalOutput <- colSumsMatrixIoIntermediateDemand + colSumsMatrixIoAddedValue # ioTotalInput 
  ioTotalOutputInverse <- 1/ioTotalOutput
  ioTotalOutputInverse[is.infinite(ioTotalOutputInverse)] <- 0
  ioTotalOutputInverse <- diag(ioTotalOutputInverse)
  A <- matrixIoIntermediateDemand %*% ioTotalOutputInverse
  I <- as.matrix(diag(ioDimention))
  ioLeontif <- ioLeontif
  ioLeontiefInverse <- solve(ioLeontif)
  
  # Backward Linkage
  analysisDBL <- colSums(ioLeontiefInverse)
  analysisBPD <- analysisDBL/(mean(analysisDBL))
  # Forward Linkage
  analysisDFL <- rowSums(ioLeontiefInverse)
  analysisFPD <- analysisDFL/(mean(analysisDFL))
  # GDP
  analysisGDP <- colSums(matrixIoAddedValue[rowIncome:nrowMatrixIoAddedValue,])
  analysisTotalGDP <- sum(analysisGDP)
  # Multiplier Output (MO)
  analysisMO <- colSums(ioLeontiefInverse)
  # Coefficient Income (CI) & Multiplier Income (MI)
  analysisCI <- as.matrix(matrixIoAddedValue[rowIncome,]) / ioTotalOutput
  analysisMI <- ioLeontiefInverse %*% analysisCI
  analysisMI[is.na(analysisMI)] <- 0
  # Coefficient Labour (CL) & Multiplier Labour (ML)
  analysisCL <- as.matrix(satelliteLabour[,3]) / ioTotalOutput
  analysisML <- ioLeontiefInverse %*% analysisCL
  analysisML[is.na(analysisML)] <- 0
  # Coefficient Energy Used (CE) & Multiplier Energy (ME)
  analysisCE <- as.matrix(satelliteEnergy[,3]) / ioTotalOutput
  analysisME <- ioLeontiefInverse %*% analysisCE
  analysisME[is.na(analysisME)] <- 0
  # Coefficient Waste Product (CW) & Multiplier Waste (MW)
  analysisCW <- as.matrix(satelliteWaste[,3]) / ioTotalOutput
  analysisMW <- ioLeontiefInverse %*% analysisCW
  analysisMW[is.na(analysisMW)] <- 0
  # Ratio Wages / Business Surplus
  analysisRatioWS <- t(as.matrix(ioAddedValue[2,] / ioAddedValue[3,]))
  analysisRatioWS[is.na(analysisRatioWS)] <- 0
  analysisRatioWS[analysisRatioWS == Inf] <- 0
  colnames(analysisRatioWS) <- "ratio_ws"
  # Satellite account by sectoral GDP
  analysisEnergyByGDP <- as.matrix(satelliteEnergy[,3]) / analysisTotalGDP
  analysisWasteByGDP <- as.matrix(satelliteWaste[,3]) / analysisTotalGDP
  analysisAgricultureByGDP <- as.matrix(satelliteAgriculture[,3]) / analysisTotalGDP
  # Emission from energy
  emissionFactorEnergyDiagonal <- diag(emissionFactorEnergy[,2], ncol = nrow(emissionFactorEnergy), nrow = nrow(emissionFactorEnergy))
  emissionEnergy <- as.matrix(satelliteEnergy[,4:ncol(satelliteEnergy)]) %*% emissionFactorEnergyDiagonal
  emissionEnergyTotal <- rowSums(emissionEnergy)
  # Emission from waste
  emissionFactorWasteDiagonal <- diag(emissionFactorWaste[,2], ncol = nrow(emissionFactorWaste), nrow = nrow(emissionFactorWaste))
  emissionWaste <- as.matrix(satelliteWaste[,4:ncol(satelliteWaste)]) %*% emissionFactorWasteDiagonal
  emissionWasteTotal <- rowSums(emissionWaste)
  # Wages
  analysisWages <- as.matrix(t(ioAddedValue[2,]))
  colnames(analysisWages) <- "wages"
  # Income per capita
  analysisIncomePerCapita <- sum(as.matrix(matrixIoAddedValue[rowIncome,])) / population
  
  result <- cbind(analysisIncomePerCapita,
                  analysisBPD,
                  analysisFPD, 
                  analysisGDP, 
                  analysisMO, 
                  analysisMI,
                  analysisML,
                  analysisME,
                  analysisMW,
                  analysisWages,
                  analysisRatioWS, 
                  analysisCE,
                  analysisCW,
                  emissionEnergyTotal,
                  emissionWasteTotal
  )
  colnames(result)[1] <- "Sektor"
  
  list_table <- list(result=result, 
                     ioSector=ioSector, 
                     ioIntermediateDemand=ioIntermediateDemand, 
                     ioFinalDemand=ioFinalDemand, 
                     ioAddedValue=ioAddedValue, 
                     satelliteLabour=satelliteLabour, 
                     satelliteEnergy=satelliteEnergy, 
                     ioFinalDemandComponent=ioFinalDemandComponent, 
                     ioAddedValueComponent=ioAddedValueComponent,
                     satelliteWaste=satelliteWaste,
                     emissionFactorWaste=emissionFactorWaste,
                     emissionFactorEnergy=emissionFactorEnergy,
                     analysisIncomePerCapita=analysisIncomePerCapita,
                     satelliteAgriculture=satelliteAgriculture
  ) 
  list_table
})

output$yearIO <- renderText({ paste0("Tahun Tabel IO: ", allDataProv$ioPeriod) })

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
  analysisIncomePerCapita <- sec$analysisIncomePerCapita
  graph <- data.frame(Sektor="", Analysis="")
  landTable_his<-sec$landTable_his
  
  if(input$categorySector=="Ekonomi"){
    if(input$pprkResults == "PDRB"){
      graph <- subset(analysisResult, select = c(Sektor, analysisGDP))
      GDPvalues <- as.matrix(analysisResult$analysisGDP)
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
      graph <- subset(analysisResult, select = c(Sektor, analysisBPD))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Forward Linkage"){
      graph <- subset(analysisResult, select = c(Sektor, analysisFPD))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Angka Pengganda Output"){
      graph <- subset(analysisResult, select = c(Sektor, analysisMO))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Angka Pengganda Pendapatan Rumah Tangga"){
      graph <- subset(analysisResult, select = c(Sektor, analysisMI))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Angka Pengganda Tenaga Kerja"){
      graph <- subset(analysisResult, select = c(Sektor, analysisML))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita') 
    } else if(input$pprkResults == "Upah gaji"){
      graph <- subset(analysisResult, select = c(Sektor, wages)) #analysisWages
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Rasio Upah gaji per Surplus Usaha"){
      graph <- subset(analysisResult, select = c(Sektor, ratio_ws)) #analysisRatioWS
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkResults == "Pendapatan per kapita"){
      removeUI(selector = '#pdrb')
      insertUI(
        selector="#placeholder",
        ui = tags$div(
          valueBox(format(analysisIncomePerCapita, nsmall = 2, big.mark = ".", decimal.mark = ","), "Juta Rupiah/Jiwa", icon = icon("credit-card"), width = 8),
          id='capita'
        )
      )
    } 
    
    if(input$pprkResults == "Perbandingan Angka Pengganda"){
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
      
      # browser()
      multiplierTable <- subset(analysisResult, select = c(Sektor, analysisMI, analysisMO, analysisML, analysisME, analysisMW))
      tabel_radarchart <- multiplierTable[multiplierTable$Sektor==input$selectedSector,]
      
      normalize<- function(x){
        return((x-min(x))/(max(x)-min(x)))
      }
      
      radarchartValue <- as.data.frame(tabel_radarchart[2:6])
      tabel_radar <- normalize(radarchartValue)
      nilai_temp <- t(tabel_radar)
      plot_ly(
        type='scatterpolar',
        r = c(nilai_temp),
        theta = c('analysisMI','analysisMO','analysisML','analysisME','analysisMW'),
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
      graph <- subset(analysisResult, select = c(Sektor, analysisME))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkEnergy == "Koefisien Intensitas Energi"){
      graph <- subset(analysisResult, select = c(Sektor, analysisCE))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkEnergy == "Emisi dari Penggunaan Energi"){
      graph <- subset(analysisResult, select = c(Sektor, emissionEnergyTotal))
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
      graph <- subset(analysisResult, select = c(Sektor, analysisMW))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
    } else if(input$pprkWaste == "Koefisien Produk Limbah"){
      graph <- subset(analysisResult, select = c(Sektor, analysisCW))
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita') 
    } else if(input$pprkWaste == "Emisi dari Limbah"){
      graph <- subset(analysisResult, select = c(Sektor, emissionWasteTotal))
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
      paste0("Direct Forward Linkage (DFL) menunjukkan tingkat keterkaitan kedepan dari sebuah sektor ekonomi.
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
      tables <- subset(analysisResult, select = c(Sektor, analysisGDP))
      tables
    } else if(input$pprkResults == "Backward Linkage"){
      tables <- subset(analysisResult, select = c(Sektor, analysisBPD))
      tables
    } else if(input$pprkResults == "Forward Linkage"){
      tables <- subset(analysisResult, select = c(Sektor, analysisFPD))
      tables
    } else if(input$pprkResults == "Angka Pengganda Output"){
      tables <- subset(analysisResult, select = c(Sektor, analysisMO))
      tables
    } else if(input$pprkResults == "Angka Pengganda Pendapatan Rumah Tangga"){
      tables <- subset(analysisResult, select = c(Sektor, analysisMI))
      tables
    } else if(input$pprkResults == "Angka Pengganda Tenaga Kerja"){
      tables <- subset(analysisResult, select = c(Sektor, analysisML))
      tables
    } else if(input$pprkResults == "Upah gaji"){
      tables <- subset(analysisResult, select = c(Sektor, wages)) #analysisWages
      tables
    } else if(input$pprkResults == "Rasio Upah gaji per Surplus Usaha"){
      tables <- subset(analysisResult, select = c(Sektor, ratio_ws)) #analysisRatioWS
      tables
    } else if(input$pprkResults == "Pendapatan per kapita"){
      return(NULL)
    } else if(input$pprkResults == "Perbandingan Angka Pengganda"){
      tables <- multiplierTable <- subset(analysisResult, select = c(Sektor, analysisMI, analysisMO, analysisML, analysisME, analysisMW)) 
      tables
    }
  } else if(input$categorySector=="Energi"){
    if(input$pprkEnergy == "Angka Pengganda Energi"){
      tables <- subset(analysisResult, select = c(Sektor, analysisME))
      tables
    } else if(input$pprkEnergy == "Koefisien Intensitas Energi"){
      tables <- subset(analysisResult, select = c(Sektor, analysisCE))
      tables
    } else if(input$pprkEnergy == "Emisi dari Penggunaan Energi"){
      tables <- subset(analysisResult, select = c(Sektor, emissionEnergyTotal))
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
      tables <- subset(analysisResult, select = c(Sektor, analysisMW))
      tables
    }  else if(input$pprkWaste == "Koefisien Produk Limbah"){
      tables <- subset(analysisResult, select = c(Sektor, analysisCW))
      tables
    }  else if(input$pprkWaste == "Emisi dari Limbah"){
      tables <- subset(analysisResult, select = c(Sektor, emissionWasteTotal))
      tables
    } 
  }
  datatable(tables, extensions = "FixedColumns", options=list(pageLength=100, scrollX=TRUE, scrollY="70vh", fixedColumns=list(leftColumns=1)), rownames=FALSE, height=540) %>%
    formatRound(columns=c(2:length(tables)),2) %>%
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
        tables <- subset(analysisResult, select = c(Sektor, analysisGDP))
      } else if(input$pprkResults == "Backward Linkage"){
        tables <- subset(analysisResult, select = c(Sektor, analysisBPD))
      } else if(input$pprkResults == "Forward Linkage"){
        tables <- subset(analysisResult, select = c(Sektor, analysisFPD))
      } else if(input$pprkResults == "Angka Pengganda Output"){
        tables <- subset(analysisResult, select = c(Sektor, analysisMO))
      } else if(input$pprkResults == "Angka Pengganda Pendapatan Rumah Tangga"){
        tables <- subset(analysisResult, select = c(Sektor, analysisMI))
      } else if(input$pprkResults == "Angka Pengganda Tenaga Kerja"){
        tables <- subset(analysisResult, select = c(Sektor, analysisML))
      } else if(input$pprkResults == "Upah gaji"){
        tables <- subset(analysisResult, select = c(Sektor, analysisWages))
      } else if(input$pprkResults == "Rasio Upah gaji per Surplus Usaha"){
        tables <- subset(analysisResult, select = c(Sektor, analysisRatioWS))
      } else if(input$pprkResults == "Pendapatan per kapita"){
        tables <- data.frame(NODATA="")
      } else if(input$pprkResults == "Perbandingan Angka Pengganda"){
        tables <- data.frame(NODATA="")
      }
    } else if(input$categorySector=="Energi"){
      if(input$pprkResults == "Angka Pengganda Energi"){
        tables <- subset(analysisResult, select = c(Sektor, analysisME))
      } else if(input$pprkResults == "Koefisien Intensitas Energi"){
        tables <- subset(analysisResult, select = c(Sektor, analysisCE))
      } else if(input$pprkResults == "Emisi dari Penggunaan Energi"){
        tables <- subset(analysisResult, select = c(Sektor, emissionEnergyTotal))
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
        tables <- subset(analysisResult, select = c(Sektor, analysisMW))
      } else if(input$pprkResults == "Koefisien Produk Limbah"){
        tables <- subset(analysisResult, select = c(Sektor, analysisCW))
      } else if(input$pprkResults == "Emisi dari Limbah"){
        tables <- subset(analysisResult, select = c(Sektor, emissionWasteTotal))
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

output$tableIO <- renderDataTable({
  if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
  ioSector <- sec$ioSector
  ioIntermediateDemand <- sec$ioIntermediateDemand
  ioFinalDemand <- sec$ioFinalDemand
  ioAddedValue <- sec$ioAddedValue
  ioFinalDemandComponent <- sec$ioFinalDemandComponent
  ioAddedValueComponent <- sec$ioAddedValueComponent
  
  io_table <- cbind(as.data.frame(ioSector[,1]), ioIntermediateDemand)
  colnames(io_table) <- c("Sektor", t(as.data.frame(ioSector[,1])))
  io_table$`Total Permintaan Antara` <- rowSums(ioIntermediateDemand)
  
  colnames(ioFinalDemand) <- c(t(ioFinalDemandComponent))
  ioFinalDemand$`Total Permintaan Akhir` <- rowSums(ioFinalDemand)
  io_table <- cbind(io_table, ioFinalDemand)
  
  total_indem <- colSums(ioIntermediateDemand)
  out_indem <- sum(total_indem)
  total_findem <- colSums(ioFinalDemand)
  out_findem <- sum(total_findem)
  total_all_indem <- as.data.frame(cbind("JUMLAH INPUT ANTARA", t(total_indem), out_indem, t(total_findem)))
  
  colnames(total_all_indem) <- colnames(io_table)
  io_table<-rbind(io_table, total_all_indem)
  
  totalrow_addval <- rowSums(ioAddedValue)
  totalcol_addval <- colSums(ioAddedValue)
  total_addval <- sum(totalrow_addval)
  addval_table <- cbind(ioAddedValueComponent, ioAddedValue, totalrow_addval)
  total_addval_table <- as.data.frame(cbind("JUMLAH INPUT", t(totalcol_addval), total_addval))
  
  remaining_col <- ncol(io_table) - ncol(total_addval_table) 
  for(i in 1:remaining_col){
    eval(parse(text=(paste("addval_table$new_col",  i, "<- ''", sep=""))))
    eval(parse(text=(paste("total_addval_table$new_col",  i, "<- ''", sep=""))))
  }
  colnames(addval_table) <- colnames(io_table)
  colnames(total_addval_table) <- colnames(io_table)
  io_table <- rbind(io_table, addval_table, total_addval_table)
  io_table
  
  datatable(io_table, extensions = "FixedColumns", options=list(paging=FALSE, scrollX=TRUE, scrollY='70vh', fixedColumns=list(leftColumns=1)), rownames=FALSE) %>%
    formatStyle('Sektor',target = "row", backgroundColor = styleEqual(c("JUMLAH INPUT ANTARA"), c('orange'))) %>%
    formatStyle(columns = "Total Permintaan Antara", target = "cell", backgroundColor = "#F7080880") %>%
    formatRound(columns=c(2:length(io_table)),2)
})

output$SatelitTenagaKerja <- renderDataTable({
  if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
  satelliteLabour <- sec$satelliteLabour
}, options=list(paging=FALSE, scrollY='70vh'))

output$SatelitEnergi <- renderDataTable({
  if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
  satelliteEnergy   <- sec$satelliteEnergy
}, extensions = "FixedColumns", options=list(paging = FALSE, scrollY='70vh', scrollX=TRUE, fixedColumns=list(leftColumns=3)))  

output$SatelitLimbah <- renderDataTable({
  if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
  satelliteWaste <- sec$satelliteWaste
}, extensions = "FixedColumns", options=list(paging = FALSE, scrollY='70vh', scrollX=TRUE, fixedColumns=list(leftColumns=3)))

output$SatelitLahan <- renderDataTable({
  if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
  LDM <- sec$LDM
}, extensions = "FixedColumns", options=list(paging = FALSE, scrollY='70vh', scrollX=TRUE, fixedColumns=list(leftColumns=3)))

output$SatelitPertanian <- renderDataTable({
  if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
  satelliteAgriculture <- sec$satelliteAgriculture
}, extensions = "FixedColumns", options=list(paging = FALSE, scrollY='70vh', scrollX=TRUE, fixedColumns=list(leftColumns=3)))

output$TutupanLahan <- renderDataTable({
  if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
  landcover <- sec$landcover
}, extensions = "FixedColumns", options=list(paging = FALSE, scrollY='70vh', scrollX=TRUE, fixedColumns=list(leftColumns=3)))
