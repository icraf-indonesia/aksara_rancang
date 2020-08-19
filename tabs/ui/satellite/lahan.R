land <- tabPanel(
  title = "Lahan", 
  value = "land",
  column(width = 12,
         br(),
         wellPanel(
           HTML("<h1>Akun Satelit: Lahan<h1>")
         )
  ),
  div(fluidRow(
      h3(style="padding-left: 15px;", "Pilih Data Input BAU"),
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
    )
    # hr(),
    # uiOutput("projTypeLandUI")
  )
)
                         