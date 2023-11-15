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



#Function to take in two parallel frontier elements from the lists
#And combine them with a new column indicating whether from both years or just one
two_frontier_elements_combine <- function(element2011,element2021){
  
  #Null checks 
  
  #If no 2011 data but some for 2021, just return 2021
  if( 
    is.null(element2011) & !is.null(element2021)
    ) {
    return(element2021 %>% mutate(year = '2021'))
  }
  
  #Vice versa
  if(
    is.null(element2021) & !is.null(element2011)
    ) {
    return(element2011 %>% mutate(year = '2011'))
  }
  
  
  
  #Can also just be an empty tibble, not null
  #But if null, can't do length test, so needs doing after
  if( 
    nrow(element2011) == 0 & nrow(element2021) > 0
    ) {
    return(element2021 %>% mutate(year = '2021'))
  }
  
  #Vice versa
  if(
    nrow(element2021) == 0 & nrow(element2011) > 0
    ) {
    return(element2011 %>% mutate(year = '2011'))
  }
  
  #If both null, return null
  if(is.null(element2021) & is.null(element2011)) return(NULL)
  
  #If both empty
  if(nrow(element2011) == 0 & nrow(element2021) == 0) return(NULL)
  
  
  
  
  element2011 <- element2011 %>% mutate(year = '2011')
  element2021 <- element2021 %>% mutate(year = '2021')
  
  #Find which pairs match in both
  #Make sure we have unique zone a and b pairs for joining by ordering them
  element2011 <- element2011 %>% 
    mutate(
      zone1 = ifelse(readr::parse_number(zoneID_a) < readr::parse_number(zoneID_b), zoneID_a, zoneID_b),#order by number
      zone2 = ifelse(readr::parse_number(zoneID_a) > readr::parse_number(zoneID_b), zoneID_a, zoneID_b)#order by number
    )
  
  element2021 <- element2021 %>% 
    mutate(
      zone1 = ifelse(readr::parse_number(zoneID_a) < readr::parse_number(zoneID_b), zoneID_a, zoneID_b),#order by number
      zone2 = ifelse(readr::parse_number(zoneID_a) > readr::parse_number(zoneID_b), zoneID_a, zoneID_b)#order by number
    )
  
  #join them both by the unique pairs
  #These will be frontiers that appeared in both years
  both <- inner_join(
    element2011,element2021 %>% st_set_geometry(NULL) %>% select(zone1,zone2),
    by = c('zone1','zone2')
  ) %>% 
    mutate(year = 'both')
  
  #"anti_join () return an rows from x without a match in y."
  #So let's do this twice
  #Either may be empty
  anti2011 <- anti_join(
    element2011,element2021 %>% st_set_geometry(NULL) %>% select(zone1,zone2),
    by = c('zone1','zone2')
  )
  
  anti2021 <- anti_join(
    element2021,element2011 %>% st_set_geometry(NULL) %>% select(zone1,zone2),
    by = c('zone1','zone2')
  )
  
  #Combine into single df to return as new element
  bind_rows(both,anti2011,anti2021)
  
  
}