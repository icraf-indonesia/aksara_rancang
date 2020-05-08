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

library(limSolve)

# ##-- Chamando os shapes do mapsBR ----
# data("regMun")
# data("regUF")

# source skenario aksi ----------------------------------------
library(reshape2)
library(limSolve)
library(plotly)
library(rlist)
#source("_DB/debug_TIN.R") #dikomen (data_historis & scenario_bau)
#source("_DB/global.R") #dikomen
#source("module.R")
# end source skenario aksi ------------------------------------

##-- Chamando as funções criadas ----
source("functions/utils.R")
source("functions/plot_functions.R")
cores <- c("#098ebb", "#fdc23a", "#e96449", "#818286")

##-- Chamando os componentes do header shiny ----
tab_files <- list.files(path = "tabs", full.names = T, recursive = T)
tab_files <- tab_files[-grep(x = tab_files, pattern = "server")]

suppressMessages(lapply(tab_files, source))
