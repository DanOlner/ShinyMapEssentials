## table function

library(htmlwidgets)
library(tidyverse)
library(DT)

table_widget <-
  function(table){
    datatable(table, options = list(pageLength = 5))
  }



# example -----------------------------------------------------------------

# ttwa_data %>% select(-geometry) %>% table_widget()

