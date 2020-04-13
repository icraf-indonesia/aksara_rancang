tab_files <- list.files(path = "tabs/ui/satellite", full.names = T)
suppressMessages(lapply(tab_files, source))

partido <- tabPanel(title = "Akun Satelit", 
                    value = "partidos",
                    hr(),
                     tabsetPanel(
                       partido_geral,
                       partido_analise,
                       partido_analise2,
                       partido_geral2
                     )
)