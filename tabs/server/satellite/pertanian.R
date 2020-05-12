#### Server: Satelit Pertanian ####

# output$SatelitPertanian <- renderDataTable({
#   if(debugMode){
#     sec <- blackBoxInputs()
#   } else {
#     sec <- allInputs()
#   }
#   satelliteAgriculture <- sec$satelliteAgriculture
# }, extensions = "FixedColumns", options=list(paging = FALSE, scrollY='70vh', scrollX=TRUE, fixedColumns=list(leftColumns=3)))


###* Satellite Account : Agriculture ####
output$SatelitPertanian <- renderRHandsontable({ 
  if(debugMode){
    sec <- blackBoxInputs()
  } else {
    sec <- allInputs()
  }
  satelliteAgriculture <- sec$satelliteAgriculture
  satelliteAgriculture$ID <- NULL
  rhandsontable(satelliteAgriculture)
})

# save edited satellite to folder user