home <- tabPanel(title = "Beranda", 
                 value = "home",
                 hr(),
                 # br(), br(),
                 # HTML("<h1><center><b>REDCLUWE</b></center></h1>"),
                 # br(), br(),
                 jumbotron(img(src="homepage.jpg", width="100%"), " ", button = FALSE),
                 # br(), br(), br(), br(),
                 # column(width = 3, align = "center",
                 #        tab_voronoys(texto = "General analysis", cor = cores[1], icon = "brasil.png", id = "analise_geral")
                 #        ),
                 # column(width = 3, align = "center",
                 #        tab_voronoys(texto = "Party analysis", cor = cores[2], icon = "flag.png", id = "analise_partidos")
                 # ),
                 # column(width = 3, align = "center",
                 #        tab_voronoys(texto = "Candidates", cor = cores[3], icon = "person.png", id = "analise_candidatos")
                 # ),
                 # column(width = 3, align = "center",
                 #        tab_voronoys(texto = "About us", cor = cores[4], icon = "sobre.png", id = "sobre")
                 # ),
                 column(width = 12,
                        br(),
                        wellPanel(
                          # HTML("<h1><b>REDCLUWE</b></h1>"),
                          HTML("<h4><b>REDCLUWE</b> adalah alat bantu proses transformasi Rencana Aksi Daerah - Gas Rumah Kaca (RAD-GRK) menjadi Perencanaan Pembangunan Rendah Karbon (PPRK).
                               Pembangunan Rendah Karbon bertujuan Menurunkan emisi Gas Rumah Kaca serta mengintegrasikan pertumbuhan ekonomi, pengentasan kemiskinan dan stabilitas sosial.
                               Pembangunan Rendah Karbon juga merupakan tindak lanjut dari program penurunan emisi Indonesia yang dituangkan dalam Rencana Aksi Nasional maupun Rencana Aksi Daerah Gas Rumah Kaca (RAN/RAD GRK).</h4>")
                        )
                 )
)
