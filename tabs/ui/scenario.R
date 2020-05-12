tab_files <- list.files(path = "tabs/ui/scenario", full.names = T)
suppressMessages(lapply(tab_files, source))

tabScenario <- tabPanel(
  title = "Skenario & Simulasi", 
  value = "tabScenario",
  hr(),
  tabsetPanel(
    bauScenario,
    intScenario
  )
)