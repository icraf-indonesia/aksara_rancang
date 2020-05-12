##-- Encrypting password ----
# digest(user$password, algo = "md5", serialize = F)
# user$encrypt<-str_rev(sapply(user$password, digest, algo="md5"))
str_rev <- function(x) sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
password_verification <- function(hash, password){
  hash_encrypt <- str_rev(sapply(password, digest, algo="md5"))
  ifelse(hash==hash_encrypt, return(TRUE), return(FALSE))
}

##-- Box ----
box_voronoys <- function(texto, cor){
  HTML(paste('<div class = "box_voronoys" style = "border:1px solid', 
             cor, '; background-color: ', 
             cor, ';">', 
             h3(texto), '</div>'))
}

tab_voronoys <- function(texto, cor, icon, id){
  HTML(paste0('<a id="', id,'" href="#" class="action-button">
                  <div class = "voronoys-block" style = "background-color:', cor, ';"> 
                  <span class = "name">', texto, '</span>
                  <div class="img_block">
                    <div class="img_block_conteiner">
                      <img src="img/',icon,'">
                    </div>
                  </div>
              </div></a>'))
}

build_coligacao <- function(coligacao){
  res <- data.frame()
  
  id <- coligacao$id
  names(id)<- coligacao$SIGLA_PARTIDO
  coli <- strsplit(coligacao$COMPOSICAO_COLIGACAO, split = ",")
  
  for(i in 1:length(coli))
  {
    coli[[i]] <- id[coli[[i]]]
    aux <- data.frame(from = coligacao$SIGLA_PARTIDO[i],
                      to = names(coli[[i]]),
                      UF = coligacao$SIGLA_UF[i])
    res <- rbind(res,aux)
  }
  index <- res$from == res$to
  res <- cbind(res, index)
  res <- res %>% filter(index == "FALSE") %>% select(-index)
  return(res)
}


syncWith <- function(map, groupname) {
  map$dependencies <- c(map$dependencies, minichartDeps())
  
  invokeMethod(map, data = leaflet::getMapData(map), "syncWith", groupname)
}

#1 Function for ...
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
  coefficientConsumption <- as.matrix(impact$consumption[,3]) / matrix_output
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