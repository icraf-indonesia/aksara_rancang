#### Server: Satelit Energi ####

###*allInput####
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
  
  inAgriculture <- input$agricultureTablle
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

# output$SatelitEnergi <- renderDataTable({
#   if(debugMode){
#     sec <- blackBoxInputs()
#   } else {
#     sec <- allInputs()
#   }
#   satelliteEnergy   <- sec$satelliteEnergy
# }, extensions = "FixedColumns", options=list(paging = FALSE, scrollY='70vh', scrollX=TRUE, fixedColumns=list(leftColumns=3))) 

###* Satellite Account : Energy ####
output$SatelitEnergi <- renderRHandsontable({ 
  if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
  satelliteEnergy <- sec$satelliteEnergy
  satelliteEnergy$ID <- NULL
  rhandsontable(satelliteEnergy)
})
