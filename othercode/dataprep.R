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

#Convert to long lat so will work with leaflet
lsoa.merge <- st_transform(lsoa.merge, "EPSG:4326")

#Add proportion non UK variable
lsoa.merge <- lsoa.merge %>% 
  mutate(UKborn_percent = (ukBorn/allResidents)*100)


#Yup, half the size. Huh.
saveRDS(lsoa.merge,'data/lsoa_layer_w_ttwalookup.rds')

#Final check... tick
#st_write(lsoa.merge,'local/lsoa_merge_check.shp')

#Tick
table(unique(lsoa.merge$ttwa) %in% ttwa$NAME)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CONVERT TTWAS TO LAT LONG FOR LEAFLET, INTERSECT TO KEEP ONLY THE APPROPRIATE PART OF ENGLAND/SCOTLAND TTWA?----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Using original TTWA file with slightly better matching boundaries to TTWA
# ttwa <- st_read('../../MapPolygons/GreatBritain/2001/TTWAs/greatBritainTTWAs.shp')
# 
# #Or not - this one has 429 obs, that's far too high (many repeats)
# 
# #Merge by name and check if still valid
# #https://stackoverflow.com/a/49354480
# ttwa <- ttwa %>% 
#   group_by(NAME) %>%
#   summarise(geometry = st_union(geometry)) %>%
#   ungroup()
# 
# #Check in QGIS... tick
# #Some were broken overlaps between countries
# st_write(ttwa,'local/ttwaunioncheck.shp')
# 
# ttwa <- st_transform(ttwa, "EPSG:4326")
# 
# #Hah, this one doesn't have labels for different countries.
# #Load in one that does, use to filter
# ttwa.w.countries <- readRDS('data/ttwa 2011 layer.rds')
# 
# #Get letter from code
# ttwa.w.countries$country <- substr(ttwa.w.countries$ttwa11cd,1,1)
# 
# #Check match... newp
# table(ttwa.w.countries$ttwa11nm %in% ttwa$NAME)
# 
# ttwa$NAME[order(ttwa$NAME)]
# ttwa.w.countries$ttwa11nm[order(ttwa.w.countries$ttwa11nm)]



#THIS IS ALL A FAFF - USING MENG LE'S FOR NOW, COME BACK TO
#Difference in number, might be different year, need to return to
#His frontier code will have been run on his TTWA file, so...
ttwa <- readRDS('data/ttwa 2011 layer.rds')

#Pull out single letter indicating country
#K = overlaps two countries - we can keep those
ttwa$country <- substr(ttwa$ttwa11cd,1,1)

#Convert to long lat so will work with leaflet
ttwa <- st_transform(ttwa, "EPSG:4326")

saveRDS(ttwa %>% filter(country %in% c('E','W','K')),'data/ttwa_engwales.rds')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CHECK FOR ANY CHANGES IN TTWA LSOA FILES FROM SOURCE----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lsoa_source <- readRDS(url('https://github.com/life-at-the-frontier/detect-uk-frontiers/raw/main/output/lsoa%20layer.rds'))
ttwa_source <- readRDS(url('https://github.com/life-at-the-frontier/detect-uk-frontiers/raw/main/output/ttwa%202011%20layer.rds'))

lsoa_local <- readRDS('data/lsoa.rds')
ttwa_local <- readRDS('data/ttwa.rds')

glimpse(lsoa_source)
glimpse(lsoa_local)

glimpse(ttwa_source)
glimpse(ttwa_local)

#Do cols match?
table(names(ttwa_source)==names(ttwa_local))

#Does whole thing match? Nope, not quite. Some updates in there.
table(ttwa_source==ttwa_local)


#LSOA cols won't match they were added to in the next section and saved locally.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#LOAD LSOA TTWA DIRECTLY FROM FRONTIERS REPO, TWEAK LOOKUP SO MATCHES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lsoa <- readRDS(url('https://github.com/life-at-the-frontier/detect-uk-frontiers/raw/main/output/lsoa%20layer.rds'))
ttwa <- readRDS(url('https://github.com/life-at-the-frontier/detect-uk-frontiers/raw/main/output/ttwa%202011%20layer.rds'))
frontiers <- readRDS(url('https://github.com/life-at-the-frontier/detect-uk-frontiers/raw/main/output/frontier%20borders%20layer.rds'))

#No NAs in the TTWA names
table(is.na(lsoa$ttwa))

#No dups
length(unique(lsoa$zoneID))

#Do we have a match when 2011 removed? TICK
lsoa$ttwa <- gsub(x = lsoa$ttwa, pattern = " (2011)", replacement = "", fixed = T)
table(ttwa$ttwa11nm %in% lsoa$ttwa)

#Percent non UK born
lsoa <- lsoa %>% mutate(UKborn_percent = (ukBorn/allResidents)*100)

#Frontiers are in list, each element a TTWA...
#Frontiers not currently lon lat. Convert.

#Also, simplify to see if speed can be increased

frontiers.proc <- lapply(frontiers, function(x) {
  
  if(!is.null(x)) {
    
    x <- st_simplify(x, dTolerance = 1000, preserveTopology = F)
    x <- st_transform(x, "EPSG:4326")
    
  }
  
  
})

st_crs(frontiers.proc[[1]])

# bugfix: TTWA ranks not working properly ---------------------------------
ttwa %>% filter(frontier_rank %in% 1:2)
## 1st is called 1nd for some reason 
ttwa <-
  ttwa %>% 
  mutate(frontier_rank_txt = frontier_rank_txt %>% gsub('1nd', '1st', x = .)) %>%
  mutate(frontier_stat = frontier_stat %>% round(3))



#Save for shiny app
saveRDS(lsoa, 'data/lsoa.rds')
saveRDS(ttwa, 'data/ttwa.rds')
saveRDS(frontiers.proc, 'data/frontiers_list.rds')



