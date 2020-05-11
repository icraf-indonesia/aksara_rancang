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
    # formatRound(columns=c(1:length(tables)),2) %>%
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