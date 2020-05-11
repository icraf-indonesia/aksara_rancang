shinyServer(function(input, output, session, data, type){
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
  source("tabs/server/scenario/input_LDM.R", local = TRUE)
  source("tabs/server/scenario/skenario_bau_debug.R", local = TRUE)
  # source("module_debug.R",local = TRUE)
  source("tabs/server/scenario/skenario_intervensi_debug.R", local = TRUE)
  ##-- HELP ----
  source("tabs/server/help.R", local = TRUE)
})