library(shiny)
library(ggplot2)
library(DT)
library(DBI)
library(shinyjs)
library(shinydashboard)
library(data.table)



ui <- fluidPage(
  title = "Examples of DataTables",
  mainPanel(tabsetPanel(
    id = 'dataset',
    tabPanel("tab 1", DT::dataTableOutput("tab1"), verbatimTextOutput('printMsg')),
    tabPanel("tab 2", DT::dataTableOutput("tab2")),
    tabPanel("tab 2", DT::dataTableOutput("tab3"))
  ))
  
)

server <- function(input, output) {
  
  printText <- reactiveValues(brands = '')
  
  buttonInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  vals <- reactiveValues()
  
  vals$Data <- data.table(
    Brands = paste0("Brand", 1:10),
    Forecasted_Growth = sample(1:20, 10),
    Last_Year_Purchase = round(rnorm(10, 1000, 1000) ^ 2),
    Contact = paste0("Brand", 1:10, "@email.com"),
    
    Action = buttonInput(
      FUN = actionButton,
      len = 10,
      id = 'button_',
      label = "Delete",
      onclick = 'Shiny.onInputChange(\"lastClick\",  this.id)'
    )
  )
  
  
  output$tab1 <- DT::renderDataTable({
    DT = vals$Data
    datatable(DT, escape = F)
  })
  
  observeEvent(input$lastClick, {
    selectedRow <- as.numeric(strsplit(input$lastClick, "_")[[1]][2])
    printText$brands <<- paste('clicked on ',vals$Data[selectedRow,1])
  })
  
  output$printMsg <- renderText({
    printText$brands
  })
  
}

shinyApp(ui, server)