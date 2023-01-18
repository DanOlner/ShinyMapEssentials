#Preparing two geographical datasets;
#All Local authorities, average IMD per LA. Have this in hex format - want in a format that can overlay tile data
#Each local authority. This one we already have the data in the correct format
#Although - those are broken into LAs. I think in shiny it'd be better to subset whole thing and serve
#This is if the full dataset is on the server side. I can check what difference it makes to speed to load each individually.
library(tidyverse)
library(sf)


#From the IMD project, looking in D:\Dropbox\imd2019/imd2019.R
#oK, that's the right one for LSOA level, with all IMD domains in (just for 2019 though, we may have one later with both?)
imd2019.geo <- readRDS('../../imd2019/data/imd2019geo.rds')

#This one is just the main IMD domain, and has both 2015 and 2019 rank and decile, for LSOAs (with LA name included for match; no code, that's maybe silly.) 
#PROB USE THIS FOR LSOA
imd2019.geo.cleannames <- readRDS('../../imd2019/data/imd2015_2019_priorToSeparatingByLA.rds') %>% rename(LSOAcode = code)


#Then we need LA map. We have hexmap version but we want actual geography to overlay over leaflet tiles
#This one has the population weighted means and medians
hex <- st_read('../../imd2019/data/hexmap-lad-england_cleannames_POPWEIGHTEDmeanmedian_deciles_n_diffs_2015_2019.geojson')

#Great. Now just needs unattaching to hex and re-attaching to standard English LA map (old, 326 zones)
LAs <- st_read('../../MapPolygons/England/2011/England_lad_2011_gen_clipped/England_lad_2011_gen_clipped.shp') %>% select(-ALTNAME)

#Check name match (were hex ones 'cleaned'?)
#If I still had codes...!
table(hex$n %in% LAs$NAME)

#Removing punctuation, from the IMD script #173
#Yup, match is now fine
LAs$NAME <- stringr::str_replace_all(LAs$NAME, "[[:punct:]]", " ")

#Might as well use this to add the LA code back in (deleted everything unnecessary for javascript to save client side download time)
#Oh, well it'll keep that when we join.

#Back to plain dataframe
hex <- hex %>% st_set_geometry(NULL)

LAs.w.data <- LAs %>% left_join(hex, by = c('NAME' = 'n'))

#Let's also add the LA code back into the LSOA data
#Check match... tick
table(imd2019.geo.cleannames$LAD17NM %in% LAs$NAME)

imd2019.geo.cleannames <- imd2019.geo.cleannames %>% 
  left_join(LAs %>% st_set_geometry(NULL) %>% rename(LACODE = CODE), by = c('LAD17NM'='NAME'))


#OK, done. Question over which would be the best data format to serve to leaflet, but leaflet itself might take care of that
#Can save both of those here. Might as well use geojson for now?
#This is 103mb. Will have to test client/server rel
# st_write(imd2019.geo.cleannames, 'data/LSOAs_plus_IMD2015_19_plusLAlookup.geojson')

#10mb
# st_write(LAs.w.data, 'data/localauthoritymap_w_IMDsummarydata.geojson')

#Actually, just save both of those as RDS, see how much faster loading can be (LSOA = ~10 seconds loading directly from geojson)

#Both need reprojecting to leaflet's projection to be used on that map
imd2019.geo.cleannames = st_transform(imd2019.geo.cleannames, "EPSG:4326")
LAs.w.data = st_transform(LAs.w.data, "EPSG:4326")


saveRDS(imd2019.geo.cleannames, 'data/LSOAs_plus_IMD2015_19_plusLAlookup.rds')
saveRDS(LAs.w.data, 'data/localauthoritymap_w_IMDsummarydata.rds')



#~~~~~~~~~~~~~~~~~~~~~~~~~~
#CHECKING FILE CONTENTS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~

#400mb
lsoa <- readRDS('data/lsoa layer.rds')

#23mb
lsoa2 <- readRDS('data/LSOAs_plus_IMD2015_19_plusLAlookup.rds')

#Is size diff due to generalisation? Can check by re-attaching 1 to copy of 2
#Oh except 2 is just England, that won't work...

plot(st_geometry(lsoa[lsoa$zoneID=="E01000001",]))
plot(st_geometry(lsoa2[lsoa2$LSOAcode=="E01000001",]))





