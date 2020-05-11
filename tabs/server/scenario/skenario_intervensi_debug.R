callModule(buttonModule, "forEnergy", modulData$energyData, type="energy")
callModule(buttonModule, "forWaste", modulData$wasteData, type="waste")
callModule(buttonModule, "forAgri", modulData$agriData, type="agriculture")
callModule(buttonModule, "forLand", modulData$landData, type="land")

modulData <- reactiveValues(
  energyData = NULL,
  wasteData = NULL,
  landData = NULL,
  agriData = NULL
)

observeEvent(input$buttonBAU, {
  # datapath <- paste0("data/", selectedProv, "/")
  # userFolder <- paste0(datapath, username)
  # if(!dir.exists(userFolder)) dir.create(userFolder, mode = 777)
  
  sec <- blackBoxInputs()
  analysis <- sec$result
  
  username = sec$username
  selectedProv = sec$selectedProv
  
  ### Begin: Main Variables ###
  ioSector = sec$ioSector
  ioIntermediateDemand = sec$ioIntermediateDemand
  ioFinalDemand = sec$ioFinalDemand
  ioFinalDemandComponent = sec$ioFinalDemandComponent
  ioAddedValue = sec$ioAddedValue
  ioAddedValueComponent = sec$ioAddedValueComponent
  ioLeontif = sec$ioLeontif
  ioLeontiefInverse = sec$ioLeontiefInverse
  ioPeriod = sec$ioPeriod
  satelliteLabour = sec$satelliteLabour
  satelliteEnergy = sec$satelliteEnergy
  satelliteWaste = sec$satelliteWaste
  satelliteAgriculture = sec$satelliteAgriculture
  emissionFactorEnergy = sec$emissionFactorEnergy
  emissionFactorWaste = sec$emissionFactorWaste
  emissionFactorAgriculture = sec$emissionFactorAgriculture
  population = sec$population
  populationProjection = sec$populationProjection
  baselineEmission = sec$baselineEmission
  analysisIncomePerCapita = sec$analysisIncomePerCapita
  GDPAll = sec$GDPAll
  LDMProp = sec$LDMProp_his #bedanya 1
  LDMProp_his = sec$LDMProp_his #bedanya 2
  growthRate = allDataProv$growthRate
  # LU_tahun=LU_tahun
  # landTable_t0=landTable_t0
  # landReq=landReq
  # tahun=tahun
  # landTable_his=landTable_his
  
  ### Begin: Result ###
  analysisBPD = analysis$analysisBPD
  analysisFPD = analysis$analysisFPD
  analysisGDP = analysis$analysisGDP
  analysisMO = analysis$analysisMO
  analysisMI = analysis$analysisMI
  analysisML = analysis$analysisML
  analysisCL = analysis$analysisCL
  analysisME = analysis$analysisME
  analysisMW = analysis$analysisMW
  analysisWages = analysis$analysisWages
  analysisRatioWS = analysis$analysisRatioWS
  analysisCE = analysis$analysisCE
  analysisCW = analysis$analysisCW
  emissionEnergyTotal = analysis$emissionEnergyTotal
  emissionWasteTotal = analysis$emissionWasteTotal
  
  matrixIoIntermediateDemand <- as.matrix(ioIntermediateDemand)
  matrixIoAddedValue <- as.matrix(ioAddedValue)
  nrowMatrixIoAddedValue <- nrow(matrixIoAddedValue)
  ioDimention <- ncol(ioIntermediateDemand)
  matrixIoFinalDemand <- as.matrix(ioFinalDemand)
  rowSumsMatrixIoFinalDemand <- as.matrix(rowSums(matrixIoFinalDemand))
  proportionFinalDemand <- ioFinalDemand/rowSumsMatrixIoFinalDemand
  proportionFinalDemand[is.na(proportionFinalDemand)] <- 0
  colSumsMatrixIoIntermediateDemand <- colSums(matrixIoIntermediateDemand)
  colSumsMatrixIoAddedValue <- colSums(matrixIoAddedValue)
  ioTotalOutput <- colSumsMatrixIoIntermediateDemand + colSumsMatrixIoAddedValue
  ioTotalOutputInverse <- 1/ioTotalOutput
  ioTotalOutputInverse[is.infinite(ioTotalOutputInverse)] <- 0
  ioTotalOutputInverse <- diag(ioTotalOutputInverse)
  
  rowImport <- 1
  rowIncome <- 2
  rowProfit <- 3
  
  initialYear <- input$dateFrom
  finalYear <- input$dateTo
  iteration <- finalYear - initialYear
  
  ### Begin: Variables in BAU Scenario ###
  analysisCPI = sec$analysisCPI
  analysisCT = sec$analysisCT
  analysisLRC = sec$analysisLRC
  carbonStock_his = sec$carbonStock_his
  ioDimention = sec$ioDimention
  landReq_his = sec$landReq_his
  landCover_his = sec$landCover_his
  LDMProp_sektor = sec$LDMProp_sektor
  LRCRate_his = sec$LRCRate_his
  LRCRate_2 = sec$LRCRate_2
  LUTMTemplate_his = sec$LUTMTemplate_his
  LUTM_his = sec$LUTM_his
  proportionFinalDemand = sec$proportionFinalDemand
  TPM = sec$TPM
  linkagesTable = sec$linkagesTable
  multiplierAll = sec$multiplierAll
  rtffile = sec$rtffile
  
  ### BAU Results ###
  populationProjection = bauResults$populationProjection
  baselineEmission = bauResults$baselineEmission
  resultGDP = bauResults$resultGDP
  resultIncomePerCapita = bauResults$resultIncomePerCapita
  resultIncome = bauResults$resultIncome
  resultLabour = bauResults$resultLabour
  resultEnergyConsumption = bauResults$resultEnergyConsumption
  resultEnergyEmission = bauResults$resultEnergyEmission
  resultWasteDisposal = bauResults$resultWasteDisposal
  resultWasteEmission = bauResults$resultWasteEmission
  resultTotalEmission = bauResults$resultTotalEmission
  bauSeriesOfImpactLabour = bauResults$bauSeriesOfImpactLabour
  bauSeriesOfImpactEnergy = bauResults$bauSeriesOfImpactEnergy
  bauSeriesOfImpactWaste = bauResults$bauSeriesOfImpactWastebauSeriesOfOutput 
  bauSeriesOfOutput = bauResults$bauSeriesOfOutput
  bauSeriesOfGDP = bauResults$bauSeriesOfGDP
  bauSeriesOfFinalDemandComponent = bauResults$bauSeriesOfFinalDemandComponent
  bauSeriesOfIntermediateDemand = bauResults$bauSeriesOfIntermediateDemand
  bauSeriesOfAddedValue = bauResults$bauSeriesOfAddedValue
  growthRateSeries = bauResults$growthRateSeries
  finalYear = bauResults$finalYear
  initialYear = bauResults$initialYear
  resultLandCover = bauResults$resultLandCover
  bauSeriesOfImpactLand1 = bauResults$bauSeriesOfImpactLand1
  bauSeriesOfImpactLand2 = bauResults$bauSeriesOfImpactLand2
  bauSeriesOfFinalDemandTable = bauResults$bauSeriesOfFinalDemandTable
  bauSeriesOfImpactAgriculture = bauResults$bauSeriesOfImpactAgriculture
  ### nama 52 Sector
  Sector <- ioSector[,1]
  Sector <- as.character(Sector)
  
  ### DATA MASTER
  fdBau <- bauSeriesOfFinalDemandTable[,-2] #tabel 2015 nya ga masuk
  fdBau$Sektor <- as.character(fdBau$Sektor) 
  
  ## FD zero
  fdZero <- fdBau
  fdZero[,2:16] <- 0
  
  ### BEGIN : SEKTOR LAHAN ####
  # inSatelliteLand <-paste0("data/", selectedProv, "/inputLandCoverZero.csv")
  # satelliteLand <- read.table(inSatelliteLand, header = T, sep = ",")
  satelliteLand <- read.table("data/JaBar/inputLandCoverZero.csv", header = T, sep = ",")
  colSectorLand <- factor(colnames(LDMProp_his),ordered=T)
  
  #alamat rds untuk menampilkan daftar di ListTableReact
  selectedSektor <- "lahan"
  alamatFile <- paste0("_DB/skenarioData/", selectedProv, "/", selectedSektor)
  
  landData <- list(
    listConsumZero=satelliteLand,
    alamatFile=alamatFile
  )
  
  modulData$landData <- landData
  ### END : SEKTOR LAHAN ####
  
  ### BEGIN : SEKTOR ENERGI ####
  
  #daftar nama FAKTOR EMISI 
  faktorEmisi <- as.character(emissionFactorEnergy[,1])  ###energi: nama 26 bahan bakar
  
  #ist konsumsi energi
  listConsumBAU <- lapply(bauSeriesOfImpactEnergy, 
                          function(x){
                            x[[1]]
                          })
  listConsumBAU <- listConsumBAU[-1] #tahun 2015 dihilangkan
  
  listConsumZero <- lapply(listConsumBAU, function(x){
    x[, 3:ncol(bauSeriesOfImpactEnergy[[1]][[1]])] <- 0 #dari kolom tcons sampai bahan bakar terakhir
    return(x)
  })
  
  #alamat rds untuk menampilkan daftar di ListTableReact
  selectedSektor <- "energi"
  alamatFile <- paste0("_DB/skenarioData/", selectedProv, "/", selectedSektor)
  
  energyData <- list(
    faktorEmisi=faktorEmisi,
    listConsumBAU=listConsumBAU,
    listConsumZero=listConsumZero,
    alamatFile=alamatFile
  )
  
  modulData$energyData <- energyData
  ### END : SEKTOR ENERGI ####
  
  ### BEGIN : SEKTOR LIMBAH ####
  
  #daftar nama FAKTOR EMISI 
  faktorEmisi <- as.character(emissionFactorWaste[,1])  ###limbah: nama2 limbah
  
  #list konsumsi energi
  listConsumBAU <- lapply(bauSeriesOfImpactWaste, 
                          function(x){
                            x[[1]]
                          })
  listConsumBAU <- listConsumBAU[-1] #tahun 2015 dihilangkan
  
  listConsumZero <- lapply(listConsumBAU, function(x){
    x[, 3:ncol(bauSeriesOfImpactWaste[[1]][[1]])] <- 0 #dari kolom tcons sampai bahan bakar terakhir
    return(x)
  })
  
  
  #alamat rds untuk menampilkan daftar di ListTableReact
  selectedSektor <- "limbah"
  alamatFile <- paste0("_DB/skenarioData/", selectedProv, "/", selectedSektor)
  
  wasteData <- list(
    faktorEmisi=faktorEmisi,
    listConsumBAU=listConsumBAU,
    listConsumZero=listConsumZero,
    alamatFile=alamatFile
  )
  
  modulData$wasteData <- wasteData
  ### END : SEKTOR LIMBAH ####
  
  ### BEGIN : SEKTOR PERTANIAN ####
  
  #daftar nama FAKTOR EMISI 
  faktorEmisi <- as.character(emissionFactorAgriculture[,1])  ###agri: nama pupuk
  
  #list konsumsi energi
  listConsumBAU <- lapply(bauSeriesOfImpactAgriculture, 
                          function(x){
                            x[[1]]
                          })
  listConsumBAU <- listConsumBAU[-1] #tahun 2015 dihilangkan
  
  listConsumZero <- lapply(listConsumBAU, function(x){
    x[, 3:ncol(bauSeriesOfImpactAgriculture[[1]][[1]])] <- 0 #dari kolom tcons sampai bahan bakar terakhir
    return(x)
  })
  
  #alamat rds untuk menampilkan daftar di ListTableReact
  selectedSektor <- "pertanian"
  alamatFile <- paste0("_DB/skenarioData/", selectedProv, "/", selectedSektor)
  
  agriData <- list(
    faktorEmisi=faktorEmisi,
    listConsumBAU=listConsumBAU,
    listConsumZero=listConsumZero,
    alamatFile=alamatFile
  )
  
  modulData$agriData <- agriData
  ### END : SEKTOR PERTANIAN ####
})

