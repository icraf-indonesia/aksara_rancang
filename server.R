shinyServer(function(input, output, session, data, type){
  user_database<-readRDS(rds_user)
  ##-- LOGIN ----
  source("tabs/server/login.R", local = TRUE)
  ##-- HOME ----
  source("tabs/server/home.R", local = TRUE)
  ##-- ANALYSIS ----
  source("tabs/server/analysis/data_historis.R", local = TRUE)
  source("tabs/server/analysis/hasil_analisis.R", local = TRUE)
  ##-- SATELLITE ----
  source("tabs/server/satellite/energi.R", local = TRUE)
  source("tabs/server/satellite/lahan.R", local = TRUE)
  source("tabs/server/satellite/limbah.R", local = TRUE)
  source("tabs/server/satellite/pertanian.R", local = TRUE)
  ##-- SCENARIO ----
  source("tabs/server/scenario/skenario_bau.R", local = TRUE)
  source("tabs/server/scenario/skenario_intervensi.R", local = TRUE)
  ##-- trade off ----
  source("tabs/server/tradeoff.R", local = TRUE)
  ##-- HELP ----
  source("tabs/server/help.R", local = TRUE)
})