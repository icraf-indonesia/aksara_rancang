prov = "JaBar"
io_folder <- paste0("data/", prov, "/")
data_folder <- paste0("data/", prov, "/")

unit = "Million IDR"
area_name = prov
I_O_period = 2015
currentPopulation = 46030000

inAgri <- paste0(io_folder, "satellite_pertanian.csv")
inEmissionFactorAgriculture <- paste0(io_folder, "emissionFaktorPertanian.csv")

satelliteAgriculture  <- read.table("D:/YUMNA/ICRAF/icraf-indonesia/aksara_rancang/data/JaBar/satellite_pertanian.csv", header=TRUE, sep=",")
emissionFactorAgriculture <- read.table("D:/YUMNA/ICRAF/icraf-indonesia/aksara_rancang/data/JaBar/emissionFaktorPertanian.csv", header=TRUE, sep=",")

setwd("D:/YUMNA/ICRAF/icraf-indonesia/aksara_rancang/data/JaBar/")
saveRDS(satelliteAgriculture , "agriculture")
saveRDS(emissionFactorAgriculture , "ef_agriculture")

readRDS("agriculture")
readRDS("data/Jabar/","energy")
