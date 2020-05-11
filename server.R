shinyServer(function(input, output, session, data, type){
  
  login <- FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$loginButton)) {
        if (input$loginButton > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(user$username==Username))==1) { 
            pasmatch  <- user["hash1"][which(user$username==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$mainbody <- renderUI({
    if(USER$login == TRUE){
      navbarPage(
        title = div(img(src="img/rancang.svg", height = "75px"), style = "padding-left:100px;"),
        id = "navbar",
        selected = "home",
        theme = "styles.css",
        fluid = T,
        home,
        # tabDescriptive,
        tabAnalysis,
        tabSatellite,
        tabScenario,
        tabTradeoff,
        tabHelp
      )
    } else {
      login_page
    }
  })
  
  ##-- HOME ----
  source("tabs/server/home.R", local = TRUE)
  ##-- ANALYSIS ----
  source("tabs/server/analysis/data_historis.R", local = TRUE)
  source("tabs/server/analysis/hasil_analisis.R", local = TRUE)
  ##-- ELEIÇÕES ----
  source("tabs/server/satellite/energi.R", local = TRUE)
  source("tabs/server/satellite/lahan.R", local = TRUE)
  source("tabs/server/satellite/limbah.R", local = TRUE)
  source("tabs/server/satellite/pertanian.R", local = TRUE)
  ##-- PARTIDOS ----
  source("tabs/server/scenario/skenario_bau.R", local = TRUE)
  source("tabs/server/scenario/skenario_intervensi.R", local = TRUE)
  ##-- HELP ----
  source("tabs/server/help.R", local = TRUE)
})