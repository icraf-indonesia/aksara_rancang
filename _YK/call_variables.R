###BEGIN: initiate all variables & function####
selectedProv <- input$categoryProvince
# datapath <- paste0("data/", selectedProv, "/")
# userFolder <- paste0(datapath, username)
# if(!dir.exists(userFolder)) dir.create(userFolder, mode = 777)

sec <- blackBoxInputs()
analysis <- sec$result

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

### Begin: BAU Results (Old code) ###
populationProjection = bauResults$population
baselineEmission = bauResults$otherEm
resultGDP = bauResults$GDP_table
resultIncomePerCapita = bauResults$income_percapita_table
resultIncome = bauResults$income_table
resultLabour = bauResults$labour_table
resultEnergyConsumption = bauResults$energy_consumption_table 
resultEnergyEmission = bauResults$energy_emission_table
resultWasteDisposal  = bauResults$waste_disposal_table
resultWasteEmission = bauResults$waste_emission_table
resultTotalEmission = bauResults$total_emission_table
bauSeriesOfImpactLabour = bauResults$impactLabour
bauSeriesOfImpactEnergy = bauResults$impactEnergy
bauSeriesOfImpactWaste = bauResults$impactWaste
bauSeriesOfGDP = bauResults$GDPSeries
bauSeriesOfOutput = bauResults$tOutputSeries =
bauSeriesOfFinalDemandComponent = bauResults$FDSeries
bauSeriesOfIntermediateDemand = bauResults$IDSeries
bauSeriesOfAddedValue = bauResults$AVSeries
growthRateSeries = bauResults$GDP_rate
finalYear = bauResults$dateTo
initialYear = bauResults$dateFrom
resultLandCover = bauResults$resultLandCover
bauSeriesOfImpactLand1 = bauResults$bauSeriesOfImpactLand1
bauSeriesOfImpactLand2 = bauResults$bauSeriesOfImpactLand2

###END: initiate all variables & function####