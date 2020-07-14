observeEvent(input$showInterventionTable, {
  # browser()
  ###### BEGIN: call variables #####
  dataHistoris = blackBoxInputs()
  dataBau = bauResults
  
  analysis = dataHistoris$result
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
  bauSeriesOfFinalDemand = dataBau$bauSeriesOfFinalDemand

  #1 Function for ...
  functionSatelliteImpact <- function(type = "energy",
                                      satellite = data.frame(),
                                      matrix_output = matrix(),
                                      emission_factor = data.frame(),
                                      additional_satellite= NULL,
                                      additional_emission_factor= NULL) {
    impact <- list()
   
    # impact$consumption
    impact$consumption <- satellite
   
    # calculate the proportion
    if(type != "labour"){
      proportionConsumption <- impact$consumption[, 4:ncol(impact$consumption)] / impact$consumption[, 3]
      impact$consumption[, 4:ncol(impact$consumption)] <- proportionConsumption
    }
   
    # calculate the coefficient & the new total satellite consumption
    coefficientConsumption <- as.matrix(impact$consumption[,3]) / matrix_output
    impact$consumption[,3] <- coefficientConsumption * matrix_output
   
    # calculate emission
    if(type != "labour"){
     
      colnames(impact$consumption)[3] <- "Tconsumption"
     
      # get the new satellite consumption for each sector
      # total consumption * proportion
      impact$consumption[,4:ncol(impact$consumption)] <- impact$consumption[,4:ncol(impact$consumption)] * impact$consumption[, 3]
      if(!is.null(additional_satellite)){
        impact$consumption[,4:ncol(impact$consumption)]<-impact$consumption[,4:ncol(impact$consumption)]+additional_satellite[,4:ncol(additional_satellite)]
        impact$consumption[,3]<-rowSums(impact$consumption[,4:ncol(impact$consumption)])
      }
     
      # checking the order of factor emission
      orderEnergyType <- names(impact$consumption)[4:ncol(impact$consumption)]
      emissionFactor <- numeric()
      if (!is.null(additional_emission_factor)){
        emission_factor<-emission_factor[,2]+additional_emission_factor[,2]
      }
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
      impact$LRC <- analysisLRC ###panggil dari blackbox input
      impact$landReq <- landReq_his
      impact$landCover <- landCover_his
    } else{
      # browser()
      if(advanceMode== TRUE){
        impact$LRC<-analysisLRC*LRCRate^(currYear-ioPeriod)
      } else{
        if (runNum == 1 ){
          impact$LRC<-analysisLRC*(LRCRate_his^(currYear-ioPeriod))
        } else if (runNum ==2 ){
          impact$LRC<-analysisLRC*(LRCRate_2^(currYear-ioPeriod))
        }
      }
      # Land Requirement
      impact$landReq<-diag(impact$LRC[,1]) %*% rbind(as.matrix(matrix_output[,1]),0)
      impact$landReq[nrow(as.matrix(impact$landReq)),]<-sum(landCover_his[,1])-sum(as.matrix(impact$landReq[1:nrow(as.matrix(impact$landReq))-1,]))
      # Land Cover
      impact$landCover<-LDMProp_sektor %*% as.matrix(impact$landReq)
      rownames(impact$landCover)<-colnames(LDMProp_his)

    }

    # Rapikan
    impact$landReq <- data.frame(c(rownames(ioSector), nrow(ioSector)+1),
                                 c(as.character(ioSector[,1]), "lainnya (tidak menghasilkan output)"),
                                 impact$landReq, stringsAsFactors = FALSE)

    colnames(impact$landReq)<-c("id.sector", "sector", "land.requirement")


    impact$landCover <- data.frame(as.character(1:23),
                                   colnames(LDMProp_his),
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
      impact$landCover<-landCover_his
      # impact$matrixE<-NULL
      # impact$matrixF<-NULL
      # impact$matrixG<-NULL
      # impact$matrixH<-NULL
      impact$LUTM<-LUTM_his

    } else{

      # set multiiplier for making matrix H
      if(advanceMode==TRUE){
        multiplier <- matrix(percentage, nrow=ncol(TPM), ncol=1)
      } else {
        if(runNum==1){ multiplier = 0.8
        } else if (runNum ==2) {multiplier <- 0.5
        } else if (runNum == 3) {multiplier <- 0.3
        } else if (runNum == 4) {multiplier <- 0.1
        } else if (runNum==5) {multiplier <- 0
        } else if (runNum==6) {
          multiplier <- 0.1
          LUTMTemplate <- matrix(NA, nrow=nrow(LUTMTemplate_his),ncol=ncol(LUTMTemplate_his))
          rownames(LUTMTemplate)<-rownames(LUTMTemplate_his)
          colnames(LUTMTemplate)<-colnames(LUTMTemplate_his)
          for (i in 1:nrow(landCover_his)){
            if (sum(landCover_his[i,])==0){
              LUTMTemplate[i,]<-matrix(0,ncol=ncol(LUTMTemplate_his)) #LUTMTemplate bisa diedit di interface
              LUTMTemplate[,i]<-matrix(0,nrow=ncol(LUTMTemplate_his))
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
      diagTPM<-matrix(NA, ncol=1, nrow=ncol(TPM))
      for (i in 1:ncol(TPM)){
        diagTPM[i,1]<-TPM[i,i]
      }
      diagTPM<-as.matrix(diagTPM[!(diagTPM==0),])

      # matrix H
      diagTPM <- diagTPM*multiplier
      impact$matrixH<-rbind(matrix(0,nrow=jumlahVariabel,ncol=1),as.matrix(diagTPM*landCoverProjectionMin[landCover_his!=0]))

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
        impact$emission[a,b]<-as.numeric(impact$LUTM[a,b])*(carbonStock_his[b,]-carbonStock_his[a,])*3.67*(-1)
      }
    }

    impact$emission<-matrix(sum(impact$emission),nrow=nrow(GDP))
    impact$emission<-impact$emission *GDP/sum(GDP)


    # rapikan
    impact$landCover <- data.frame(as.character(1:23),
                                   colnames(LDMProp_his),
                                   impact$landCover[,1],stringsAsFactors=FALSE)
    colnames(impact$landCover)<-c("id.land.use", "land.use", "luas.land.use")

    impact$LUTM <- data.frame(as.character(1:23),
                              colnames(LDMProp_his),
                              impact$LUTM,stringsAsFactors=FALSE)
    colnames(impact$LUTM)<-c("id.land.use", "land.use", colnames(LDMProp_his))

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

  ##### END : call variables ####
  
  ####  BEGIN: create energyData #####
  LDMProp_his<-allDataProv$LDMProp_his
  selectedProv<-allDataProv$selectedProv
  ### DATA MASTER
  fdBau <- bauSeriesOfFinalDemandTable[,-2] #tabel 2015 nya ga masuk
  fdBau$Sektor <- as.character(fdBau$Sektor) 
  
  ## FD zero
  fdZero <- fdBau
  fdZero[,2:ncol(fdZero)] <- 0
  bauResults$fdZero<-fdZero

  ### BEGIN : SEKTOR LAHAN ####
  # inSatelliteLand <-paste0("data/", selectedProv, "/inputLandCoverZero.csv")
  # satelliteLand <- read.table(inSatelliteLand, header = T, sep = ",")
  satelliteLand <- read.table("data/JaBar/inputLandCoverZero.csv", header = T, sep = ",")
  colSectorLand <- factor(colnames(LDMProp_his),ordered=T)
  
  #alamat rds untuk menampilkan daftar di ListTableReact
  selectedSektor <- "lahan"
  alamatFile <- paste0("skenarioData/", selectedProv, "/", selectedSektor)
  
  landData <- list(
    listConsumZero=satelliteLand,
    alamatFile=alamatFile
  )
  
  bauResults$landData <- landData
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
  alamatFile <- paste0("skenarioData/", selectedProv, "/", selectedSektor)
  
  energyData <- list(
    faktorEmisi=faktorEmisi,
    listConsumBAU=listConsumBAU,
    listConsumZero=listConsumZero,
    alamatFile=alamatFile
  )
  
  bauResults$energyData <- energyData
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
  alamatFile <- paste0("skenarioData/", selectedProv, "/", selectedSektor)
  
  wasteData <- list(
    faktorEmisi=faktorEmisi,
    listConsumBAU=listConsumBAU,
    listConsumZero=listConsumZero,
    alamatFile=alamatFile
  )
  
  bauResults$wasteData <- wasteData
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
  alamatFile <- paste0("skenarioData/", selectedProv, "/", selectedSektor)
  
  agriData <- list(
    faktorEmisi=faktorEmisi,
    listConsumBAU=listConsumBAU,
    listConsumZero=listConsumZero,
    alamatFile=alamatFile
  )
  
  bauResults$agriData <- agriData
  
  callModule(buttonModule, "forEnergy", bauResults$energyData, type="energy", dataBau=bauResults, dataHistoris=blackBoxInputs())
  callModule(buttonModule, "forWaste", bauResults$wasteData, type="waste", dataBau=bauResults, dataHistoris=blackBoxInputs())
  callModule(buttonModule, "forAgri", bauResults$agriData, type="agriculture", dataBau=bauResults, dataHistoris=blackBoxInputs())
  callModule(buttonModule, "forLand", bauResults$landData, type="land", dataBau=bauResults, dataHistoris=blackBoxInputs())
  
  ### END : SEKTOR PERTANIAN ####
})
