tabHelp <- tabPanel(
  title=HTML("Help<li><a href='javascript:window.location.reload(true)'>Keluar</li></a>"), 
  hr(), 
  # actionLink("infoLink", "Tutorial Redcluwe", class = "btn-info"),
  # tags$a(href="http://ior.ad/74qb", "Klik disini untuk melihat Tutorial!"),
  tags$iframe(src="http://ior.ad/74qb", height=750, width="100%"),
  value="tabHelp"
)