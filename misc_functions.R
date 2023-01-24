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