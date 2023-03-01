## ad hoc get the stuff 
library(tidyverse)
library(data.table)
ons_lookup <- '../data/NSPL_MAY_2022_UK.csv' %>%
  fread(
#    nrows = 1, 
    select = c('pcd2', 'ttwa')
    )

ons_lookup <-
  ons_lookup %>%
  mutate(
    pcd_area = (pcd2 %>% str_split(' ') %>% map(.f = function(x) x[1]) %>% unlist())
  )

ons_lookup <-
  ons_lookup %>% 
  group_by(pcd_area) %>%
  summarise(ttwa11cd = ttwa[1])


### join to postcode name 
ttwa <- readRDS('data/ttwa.rds')
st_geometry(ttwa) <- NULL
ttwa <- 
  ttwa %>%
  select(ttwa, ttwa11cd)
## left join

ons_lookup <-
  ons_lookup %>%
  left_join(ttwa)
  
## lots of missing will be scottish postcode 
ons_lookup <- 
  ons_lookup %>%
  filter(!is.na(ttwa))

ons_lookup %>% saveRDS('data/postcode lookup table.rds')
