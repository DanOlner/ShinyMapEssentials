#Miscellaneous functions

#FILTER FRONTIERS LIST BY NORMALISED PHI VALUE
#Keep only frontiers with std diff phi values ABOVE the cutoff
filter.frontiers.by.phi <- function(frontiers.list, phi.cutoff){
  
  lapply(frontiers.list, function(x) {
    
    if(!is.null(x)) {
      
      x <- dplyr::filter(x, std_diff_phi > phi.cutoff)
      
    }
    
  })
  
}


# #FRONTIER LINE THICKNESS / ZOOM LOOKUP
# #Frontiers to be thicker lines when zoomed in closer, so they continue to stand out
# frontierzoom.v.thickness <- function(zoom){
#   
#   case_when(
#     
#     zoom < 12 ~ 1,
#     zoom == 12 ~ 2,
#     zoom == 13 ~ 3,
#     zoom == 14 ~ 4,
#     zoom > 14 ~ 5
#     
#   )
#   
# }