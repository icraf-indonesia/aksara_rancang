##-- Pacotes ----
# library(dplyr)
# library(dbplyr)
library(digest)
library(data.table)
library(reshape2)
library(tidyr)
library(lubridate)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
# library(rgeos)
library(shinycssloaders)
library(ggthemes)
library(RColorBrewer)
# library(sf)
# library(sp)
library(scales)
library(leaflet)
library(plotly)
library(highcharter)
library(DT)
# library(mapview)
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

# source skenario aksi ----------------------------------------
source("_DB/debug_TIN.R")
source("_DB/global.R")
# end source skenario aksi ------------------------------------

##-- Function & Module ----
source("functions/utils.R")
source("functions/module.R")
# cores <- c("#098ebb", "#fdc23a", "#e96449", "#818286")
source("login.R")

##-- Shiny tabs header ui ----
tab_files <- list.files(path = "tabs", full.names = T, recursive = T)
tab_files <- tab_files[-grep(x = tab_files, pattern = "server")]

suppressMessages(lapply(tab_files, source))

user_database<-readRDS("data/user_database")
