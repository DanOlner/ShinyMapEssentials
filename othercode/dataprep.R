#Preparing two geographical datasets;
#All Local authorities, average IMD per LA. Have this in hex format - want in a format that can overlay tile data
#Each local authority. This one we already have the data in the correct format
#Although - those are broken into LAs. I think in shiny it'd be better to subset whole thing and serve
#This is if the full dataset is on the server side. I can check what difference it makes to speed to load each individually.
library(tidyverse)
library(sf)
library(tmap)

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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#ADD TTWA NAMES TO LSOAS MANUALLY----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Using original TTWA file with slightly better matching boundaries to TTWA
ttwa <- st_read('../../MapPolygons/GreatBritain/2001/TTWAs/greatBritainTTWAs.shp')

#LSOAs via https://github.com/life-at-the-frontier/detect-uk-frontiers/tree/test-run/output
#Some NAs and incorrect TTWA names
#Will need fixed for subsetting into groups
lsoa <- readRDS('data/lsoa layer.rds')

#Now what's going on with the LSOAs? We've got duplicate IDs...
#34753 IDs out of 39030 rows
length(unique(lsoa$zoneID))

#OK, I think (see randomDataChecks) there was multiple matching of single LSOAs to different TTWAs
#They're the same poloygon in each case, I'm pretty sure
#So we just need to keep single IDs (any of them, and forget about the current TTWA match)
lsoa <- lsoa %>% distinct(zoneID, .keep_all = T)

#Check none missing visually in QGIS... tick
#st_write(lsoa,'local/lsoa_unique_check.shp')


#Ignore current LSOA ttwa labels - mostly right but some errors.
#Might as well do all fresh

#Get intersection, for keeping only those with largest areas for each LSOA
#Which will then be the correct TTWA
lsoa.intersect <- lsoa %>% st_intersection(ttwa)

#Drop previous TTWA name match
lsoa.intersect <- lsoa.intersect %>% select(-ttwa)

#Add area
lsoa.intersect$area <- st_area(lsoa.intersect)

#Pick max area per group, shouldn't have any duplicated
#Correct, is now same number of rows as original
lsoa.largest <- lsoa.intersect %>% 
  group_by(zoneID) %>% 
  top_n(n=1, wt = area) %>% 
  select(-area, -LABEL) %>% 
  rename(ttwa = NAME)

#That's the LSOA file we need - right number of rows, now with TTWA
#Though it's quite a bit bigger?
# saveRDS(lsoa.largest,'data/lsoa_layer_w_ttwalookup.rds')

#Can we merge into the original to keep smaller?
lsoa.merge <- lsoa %>% select(-ttwa) %>% 
  left_join(lsoa.largest %>% st_set_geometry(NULL) %>% select(zoneID,ttwa),by = 'zoneID')

#Yup, half the size. Huh.
saveRDS(lsoa.merge,'data/lsoa_layer_w_ttwalookup.rds')

#Final check... tick
#st_write(lsoa.merge,'local/lsoa_merge_check.shp')

#Tick
table(unique(lsoa.merge$ttwa) %in% ttwa$NAME)









