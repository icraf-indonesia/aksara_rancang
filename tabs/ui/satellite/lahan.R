land <- tabPanel(
  title = "Lahan", 
  value = "land",
  div(fluidRow(
      h1(style="padding-left: 15px;", "Pilih Akun Satellit Lahan"),
      br(),
      box(width=12,
         #        tabPanel("Tabel LDM Proporsi", id="TabPanelBAUDataLDMProp",
         div(style="overflow-x: scroll", 
             uiOutput("LDMFileOptions"),
             dataTableOutput("LDMListTable"),
             dataTableOutput("LDMTampil"),
             uiOutput('LDMTableTampilUI'),
             uiOutput('modalLDMUI'))
      )
    ),
    # hr(),
    # uiOutput("projTypeLandUI")
  )
)
                         