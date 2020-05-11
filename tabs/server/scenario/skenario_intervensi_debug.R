callModule(buttonModule, "forEnergy", energyData, type="energy")
callModule(buttonModule, "forWaste", wasteData, type="waste")
callModule(buttonModule, "forAgri", agriData, type="agriculture")
callModule(buttonModule, "forLand", landData, type="land")

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
### END : SEKTOR PERTANIAN ####
