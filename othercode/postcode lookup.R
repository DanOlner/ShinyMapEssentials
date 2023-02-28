## ad hoc get the stuff 
library(data.table)
ons_lookup <- '../data/NSPL_MAY_2022_UK.csv' %>%
  fread(select = c('pcd', 'ttwa'))

ons_lookup %>% saveRDS('data/postcode lookup table.rds')
