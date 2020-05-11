tab_files <- list.files(path = "tabs/ui/analysis", full.names = T)
suppressMessages(lapply(tab_files, source))

tabDescriptive <- navbarMenu(title = "Analisis Deskriptif", 
  data_historis,
  hasil_analisis
)