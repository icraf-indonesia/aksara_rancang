selectedProv <- input$categoryProvince
datapath <- paste0("data/", selectedProv, "/")
userFolder <- paste0(datapath, username)
if(!dir.exists(userFolder)) dir.create(userFolder, mode = 777)

sec <- blackBoxInputs()
analysis <- sec$result

### Begin: Main Variables ####
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
# LU_tahun=LU_tahun
# landTable_t0=landTable_t0
# landReq=landReq
# tahun=tahun
# landTable_his=landTable_his
### End: Main Variables ####

### Begin: Result ####
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
### End: Result ####

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
### End: Variables in BAU Scenario ###

### Begin: BAU Results ####
bauResults$populationProjection = sec$populationProjection
bauResults$populationProjection = populationProjection
bauResults$baselineEmission = sec$baselineEmission 
bauResults$baselineEmission = baselineEmission 
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
### End: BAU Results ####

bauSeriesOfImpactLand1<-list()
bauSeriesOfImpactLand2<-list()
