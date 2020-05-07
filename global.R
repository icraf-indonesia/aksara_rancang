##-- Pacotes ----
library(dplyr)
library(dbplyr)
library(data.table)
library(reshape2)
library(tidyr)
library(lubridate)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(rgeos)
library(shinycssloaders)
library(ggthemes)
library(RColorBrewer)
library(sf)
library(sp)
library(scales)
library(leaflet)
library(plotly)
library(highcharter)
library(DT)
library(mapview)
# devtools::install_github("lgsilvaesilva/mapsBR")
#library(mapsBR)
library(shinyLP)
library(readxl)
library(rhandsontable)
# library(sodium)

library(shinyjs)
library(digest)
library(rintrojs)
library(fmsb)
library(ggplot2)
library(formattable)
library(rtf)
library(stringr)
library(gridExtra)
#library(ggradar)
# library(RColorBrewer)

# ##-- Chamando os shapes do mapsBR ----
# data("regMun")
# data("regUF")

# source skenario aksi ----------------------------------------
# source("_DB/debug_TIN.R")
# source("_DB/global.R")
source("module.R")
# end source skenario aksi ------------------------------------

##-- Chamando as funções criadas ----
source("functions/utils.R")
source("functions/plot_functions.R")
cores <- c("#098ebb", "#fdc23a", "#e96449", "#818286")

##-- Chamando os componentes do header shiny ----
tab_files <- list.files(path = "tabs", full.names = T, recursive = T)
tab_files <- tab_files[-grep(x = tab_files, pattern = "server")]

suppressMessages(lapply(tab_files, source))

###global variable

#ID
username<-"dw"

### nama 52 Sector
Sector<-ioSector[,1]
Sector <- as.character(Sector)

### DATA MASTER
fdBau <- bauSeriesOfFinalDemandTable[,-2] #tabel 2015 nya ga masuk
fdBau$Sektor <- as.character(fdBau$Sektor) 

## FD zero
fdZero <- fdBau
fdZero[,2:16] <- 0

################################################################################
#                                                                              #
#                                sektor lahan                                  #
#                                                                              #
################################################################################
satelliteLand <- read.table("_DB/jabar_in_redcluwe/inputLandCoverZero.csv", header = T, sep = ",")

colSectorLand <- factor(colnames(LDMProp_his),ordered=T)

#alamat rds untuk menampilkan daftar di ListTableReact
selectedSektor <- "lahan"
alamatFile <- paste0("_DB/skenarioData/", selectedProv, "/", selectedSektor)

landData <- list(
  listConsumZero=satelliteLand,
  alamatFile=alamatFile
)

################################################################################
#                                                                              #
#                                sektor energi                                 #
#                                                                              #
################################################################################

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

################################################################################
#                                                                              #
#                                sektor limbah                                 #
#                                                                              #
################################################################################

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

################################################################################
#                                                                              #
#                                sektor pertanian                              #
#                                                                              #
################################################################################

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
