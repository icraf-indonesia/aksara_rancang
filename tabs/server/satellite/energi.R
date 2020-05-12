#### Server: Satelit Energi ####

# output$SatelitEnergi <- renderDataTable({
#   if(debugMode){
#     sec <- blackBoxInputs()
#   } else {
#     sec <- allInputs()
#   }
#   satelliteEnergy   <- sec$satelliteEnergy
# }, extensions = "FixedColumns", options=list(paging = FALSE, scrollY='70vh', scrollX=TRUE, fixedColumns=list(leftColumns=3))) 

###* Satellite Account : Energy ####
output$SatelitEnergi <- renderRHandsontable({ 
  if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
  satelliteEnergy <- sec$satelliteEnergy
  satelliteEnergy$ID <- NULL
  rhandsontable(satelliteEnergy)
})

# save edited satellite to folder user