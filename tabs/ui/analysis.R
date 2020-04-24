tab_files <- list.files(path = "tabs/ui/analysis", full.names = T)
suppressMessages(lapply(tab_files, source))

tabAnalysis <- tabPanel(title = "Analisis Deskriptif", 
                     value = "tabAnalysis",
                     hr(),
                     tabsetPanel(
                       data_historis,
                       data_historis_debug,
                       hasil_analisis
                     )
)