tab_files <- list.files(path = "tabs/ui/satellite", full.names = T)
suppressMessages(lapply(tab_files, source))

tabSatellite <- tabPanel(title = "Akun Satelit", 
                    value = "tabSatellite",
                    hr(),
                     tabsetPanel(
                       energy,
                       land,
                       waste,
                       agriculture
                     )
)