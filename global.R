##-- Library ----
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(tidyr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(ggthemes)
library(RColorBrewer)
library(plotly)
library(DT)
library(shinyLP)
library(rhandsontable)
library(sodium)
library(V8)
library(shinyjs)
library(digest)
library(rintrojs)
library(fmsb)
library(ggplot2)
library(formattable)
library(rtf)
library(stringr)
library(gridExtra)
library(limSolve)
library(rlist)
library(shinyalert)

# tab trade off -----------------------------------------------------------
library(shinyBS)
library(gtools)

##-- Template function ----
source("functions/module.R")
source("functions/utils.R")
source("functions/plot_functions.R")

rds_user<-"data/user_database"

##-- Template tabs function ----
tab_files <- list.files(path = "tabs", full.names = T, recursive = T)
tab_files <- tab_files[-grep(x = tab_files, pattern = "server")]

suppressMessages(lapply(tab_files, source))

