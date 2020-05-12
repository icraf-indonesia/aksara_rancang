shinyUI(
  fluidPage(
    ##-- Favicon ----
    tags$head(
      tags$link(rel = "shortcut icon", href = "img/rancang.ico"),
      #-- biblio js ----
      tags$link(rel="stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"),
      tags$link(rel="stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Open+Sans|Source+Sans+Pro")
    ),
    ##-- Logo ----
    list(tags$head(HTML('<link rel="icon", href="img/rancang.svg", type="image/png" />'))),
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
          title="", windowTitle = "REDCLUWE"
        )
    ),
    ##-- Header ----
    uiOutput("mainbody"),
    ##-- Footer ----
    div(class = "footer",
        includeHTML("html/footer.html")
        # div(includeHTML("html/google_analytics.html"))
    )
  )
)