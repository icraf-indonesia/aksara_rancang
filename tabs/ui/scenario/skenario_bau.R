bauScenario <- tabPanel(
  title = "Skenario BAU", 
  value = "bauScenario",
  hr(), 
  tabBox(id="tabPanelBAU", width = 12,
     tabPanel(title = "Input",
      div(
        uiOutput("projTypeEconomyUI"),
        useShinyalert()
        # hr(),
        # tags$div(id="gdpRateUI",
        #   conditionalPanel(
        #     condition = "input.typeIntervention=='Tipe 1'",
        #     h3("Tipe 1: Pertumbuhan ekonomi sama untuk setiap lapangan usaha, namun berbeda setiap tahun"),
        #     selectInput("yearBAUInv", "Pilih tahun:", choices = 2010:2030, selected=2015),
        #     sliderInput("gdpRate", "Laju pertumbuhan ekonomi:", min=0, max=10, post="%", value=5, step=0.01, width = "600px")
        #   ),
        #   conditionalPanel(
        #     condition = "input.typeIntervention=='Tipe 2'",
        #     h3("Tipe 2: Pertumbuhan ekonomi berbeda baik untuk setiap lapangan usaha, maupun setiap tahun proyeksi")
        #   ),
        #   br(),
        #   actionButton("saveTableBauType", "Simpan Tabel"),
        #   actionButton("buttonBAU", "Jalankan Simulasi"),
        #   br(), br(),
        #   rHandsontableOutput('tableBAUType'),
        #   br()
        # )
      )
    ),
    tabPanel(title = "Hasil Analisis",
      uiOutput("resultUI")
    )
  )
)
                                             