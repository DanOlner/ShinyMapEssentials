# setup 
# Create any data needed for app 


# metadata ----------------------------------------------------------------
library(purrr)
library(sf)
library(tidyverse)

data_paths <- 
  list.files('data',full.names = T) 

data_paths[data_paths %>% grepl(pattern = 'metadata')] <- NULL #exclude self


metadata <-
  data_paths %>%
  as.list() %>%
  map(
    .f =  
      safely(
        function(x){
          list(
            colnames = x %>% readRDS() %>% names
          )
    }
  )
  )

## check and output
names(metadata) <- c('la', 'lsoa')

metadata <- metadata %>% transpose

metadata$result %>% saveRDS('data/metadata.rds')
