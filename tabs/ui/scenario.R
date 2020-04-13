tab_files <- list.files(path = "tabs/ui/scenario", full.names = T)
suppressMessages(lapply(tab_files, source))

candidato <- tabPanel(title = "Skenario & Simulasi", 
                      value = "candidatos",
                      hr(),
                      tabsetPanel(
                        perfil_eleitorado,
                        perfil
                      )
)