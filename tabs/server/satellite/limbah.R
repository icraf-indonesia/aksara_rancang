#### Server: Satelit Limbah ####

# output$SatelitLimbah <- renderDataTable({
#   if(debugMode){
#     sec <- blackBoxInputs()
#   } else {
#     sec <- allInputs()
#   }
#   satelliteWaste <- sec$satelliteWaste
# }, extensions = "FixedColumns", options=list(paging = FALSE, scrollY='70vh', scrollX=TRUE, fixedColumns=list(leftColumns=3)))

# ###* Satellite Account : Waste ####
output$SatelitLimbah <- renderRHandsontable({
  if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
  satelliteWaste <- sec$satelliteWaste
  satelliteWaste$ID <- NULL
  rhandsontable(satelliteWaste)
})
# }, extensions = "FixedColumns", options=list(paging = FALSE, scrollY='70vh', scrollX=TRUE, fixedColumns=list(leftColumns=3)))

# save edited satellite to folder user