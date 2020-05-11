###BEGIN: initiate all variables & function####


# datapath <- paste0("data/", selectedProv, "/")
# userFolder <- paste0(datapath, username)
# if(!dir.exists(userFolder)) dir.create(userFolder, mode = 777)

# dataHistoris <- blackBoxInputs()
analysis <- dataHistoris$result

selectedProv = dataHistoris$selectedProv
username = dataHistoris$username

### Begin: Main Variables ###
ioSector = dataHistoris$ioSector
ioIntermediateDemand = dataHistoris$ioIntermediateDemand
ioFinalDemand = dataHistoris$ioFinalDemand
ioFinalDemandComponent = dataHistoris$ioFinalDemandComponent
ioAddedValue = dataHistoris$ioAddedValue
ioAddedValueComponent = dataHistoris$ioAddedValueComponent
ioLeontif = dataHistoris$ioLeontif
ioLeontiefInverse = dataHistoris$ioLeontiefInverse
ioPeriod = dataHistoris$ioPeriod
satelliteLabour = dataHistoris$satelliteLabour
satelliteEnergy = dataHistoris$satelliteEnergy
satelliteWaste = dataHistoris$satelliteWaste
satelliteAgriculture = dataHistoris$satelliteAgriculture
emissionFactorEnergy = dataHistoris$emissionFactorEnergy
emissionFactorWaste = dataHistoris$emissionFactorWaste
emissionFactorAgriculture = dataHistoris$emissionFactorAgriculture
population = dataHistoris$population
populationProjection = dataHistoris$populationProjection
baselineEmission = dataHistoris$baselineEmission
analysisIncomePerCapita = dataHistoris$analysisIncomePerCapita
GDPAll = dataHistoris$GDPAll
LDMProp = dataHistoris$LDMProp_his #bedanya 1
LDMProp_his = dataHistoris$LDMProp_his #bedanya 2
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
analysisCPI = dataHistoris$analysisCPI
analysisCT = dataHistoris$analysisCT
analysisLRC = dataHistoris$analysisLRC
carbonStock_his = dataHistoris$carbonStock_his
ioDimention = dataHistoris$ioDimention
landReq_his = dataHistoris$landReq_his
landCover_his = dataHistoris$landCover_his
LDMProp_sektor = dataHistoris$LDMProp_sektor
LRCRate_his = dataHistoris$LRCRate_his
LRCRate_2 = dataHistoris$LRCRate_2
LUTMTemplate_his = dataHistoris$LUTMTemplate_his
LUTM_his = dataHistoris$LUTM_his
proportionFinalDemand = dataHistoris$proportionFinalDemand
TPM = dataHistoris$TPM
linkagesTable = dataHistoris$linkagesTable
multiplierAll = dataHistoris$multiplierAll
rtffile = dataHistoris$rtffile

### BAU Results ###
populationProjection = dataBau$populationProjection
baselineEmission = dataBau$baselineEmission
resultGDP = dataBau$resultGDP
resultIncomePerCapita = dataBau$resultIncomePerCapita
resultIncome = dataBau$resultIncome
resultLabour = dataBau$resultLabour
resultEnergyConsumption = dataBau$resultEnergyConsumption
resultEnergyEmission = dataBau$resultEnergyEmission
resultWasteDisposal = dataBau$resultWasteDisposal
resultWasteEmission = dataBau$resultWasteEmission
resultTotalEmission = dataBau$resultTotalEmission
bauSeriesOfImpactLabour = dataBau$bauSeriesOfImpactLabour
bauSeriesOfImpactEnergy = dataBau$bauSeriesOfImpactEnergy
bauSeriesOfImpactWaste = dataBau$bauSeriesOfImpactWastebauSeriesOfOutput 
bauSeriesOfOutput = dataBau$bauSeriesOfOutput
bauSeriesOfGDP = dataBau$bauSeriesOfGDP
bauSeriesOfFinalDemandComponent = dataBau$bauSeriesOfFinalDemandComponent
bauSeriesOfIntermediateDemand = dataBau$bauSeriesOfIntermediateDemand
bauSeriesOfAddedValue = dataBau$bauSeriesOfAddedValue
growthRateSeries = dataBau$growthRateSeries
finalYear = dataBau$finalYear
initialYear = dataBau$initialYear
resultLandCover = dataBau$resultLandCover
bauSeriesOfImpactLand1 = dataBau$bauSeriesOfImpactLand1
bauSeriesOfImpactLand2 = dataBau$bauSeriesOfImpactLand2
bauSeriesOfFinalDemandTable = dataBau$bauSeriesOfFinalDemandTable
bauSeriesOfImpactAgriculture = dataBau$bauSeriesOfImpactAgriculture

### Begin: BAU Results (Old code) ###
# populationProjection = bauResults$population
# baselineEmission = bauResults$otherEm
# resultGDP = bauResults$GDP_table
# resultIncomePerCapita = bauResults$income_percapita_table
# resultIncome = bauResults$income_table
# resultLabour = bauResults$labour_table
# resultEnergyConsumption = bauResults$energy_consumption_table 
# resultEnergyEmission = bauResults$energy_emission_table
# resultWasteDisposal  = bauResults$waste_disposal_table
# resultWasteEmission = bauResults$waste_emission_table
# resultTotalEmission = bauResults$total_emission_table
# bauSeriesOfImpactLabour = bauResults$impactLabour
# bauSeriesOfImpactEnergy = bauResults$impactEnergy
# bauSeriesOfImpactWaste = bauResults$impactWaste
# bauSeriesOfGDP = bauResults$GDPSeries
# bauSeriesOfOutput = bauResults$tOutputSeries =
# bauSeriesOfFinalDemandComponent = bauResults$FDSeries
# bauSeriesOfIntermediateDemand = bauResults$IDSeries
# bauSeriesOfAddedValue = bauResults$AVSeries
# growthRateSeries = bauResults$GDP_rate
# finalYear = bauResults$dateTo
# initialYear = bauResults$dateFrom
# resultLandCover = bauResults$resultLandCover
# bauSeriesOfImpactLand1 = bauResults$bauSeriesOfImpactLand1
# bauSeriesOfImpactLand2 = bauResults$bauSeriesOfImpactLand2

###END: initiate all variables & function####