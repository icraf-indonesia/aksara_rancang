tab_files <- list.files(path = "tabs/ui/analysis", full.names = T)
suppressMessages(lapply(tab_files, source))

eleicoes <- tabPanel(title = "Analisis Deskriptif", 
                     value = "eleicoes",
                     hr(),
                     tabsetPanel(
                       eleicoes_brasil,
                       eleicoes_uf
                     )
)