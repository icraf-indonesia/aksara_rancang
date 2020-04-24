shinyServer(function(input, output, session){
  ##-- HOME ----
  source("tabs/server/home.R", local = TRUE)
  ##-- ANALYSIS ----
  source("tabs/server/analysis/data_historis.R", local = TRUE)
  # source("tabs/server/analysis/data_historis_debug.R", local = TRUE)
  source("tabs/server/analysis/hasil_analisis.R", local = TRUE)
  ##-- ELEIÇÕES ----
  source("tabs/server/satellite/energi.R", local = TRUE)
  source("tabs/server/satellite/lahan.R", local = TRUE)
  source("tabs/server/satellite/limbah.R", local = TRUE)
  source("tabs/server/satellite/pertanian.R", local = TRUE)
  ##-- PARTIDOS ----
  source("tabs/server/scenario/skenario_bau.R", local = TRUE)
  source("tabs/server/scenario/skenario_intervensi.R", local = TRUE)
  ##-- HELP ----
  source("tabs/server/help.R", local = TRUE)
})