prov = "JaBar"
io_folder <- paste0("data/", prov, "/")
data_folder <- paste0("data/", prov, "/")

unit = "Million IDR"
area_name = prov
I_O_period = 2015
currentPopulation = 46030000

inAgri <- paste0(io_folder, "satellite_pertanian.csv")

satelliteAgriculture  <- read.table(inAgri, header=TRUE, sep=",")

saveRDS(satelliteAgriculture , "agriculture")
readRDS("agriculture")
readRDS("data/Jabar/","energy")
