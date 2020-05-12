tabTradeoff <- tabPanel(
  title = "Analisis Trade-off", 
  value = "tabTradeoff", 
  hr(),
  titlePanel("ANALISIS TRADE-OFF"),
  h4("Pastikan seluruh skenario aksi yang di definisikan pada modul Skenario & Simulasi (pada tab menu Skenario Intervensi) 
    sudah dilakukan analisis dengan menekan tombol Jalankan Analisis."),
  br(),
  h4("Daftar skenario yang sudah dianalisis pada modul Skenario & Simulasi"),
  dataTableOutput("ListTable"),
  h4("Tekan tombol dibawah ini untuk menjalankan Analisis Trade-Off:"),
  actionButton("tradeOffRunButton","Jalankan Analisis Trade-Off"),
  tags$div(id = 'tradeOffResultPlaceholder')
)