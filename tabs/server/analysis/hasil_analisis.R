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
