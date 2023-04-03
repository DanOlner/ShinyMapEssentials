#random data checks
library(tidyverse)
library(sf)
library(tmap)
library(rgeos)

#~~~~~~~~~~~~~~~~~~~~~~~~~~
#CHECKING FILE CONTENTS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~

#SMALLER VERSION NOW, 15MB
#400mb
lsoa <- readRDS('data/lsoa layer.rds')

plot(st_geometry(lsoa))

#23mb
lsoa2 <- readRDS('data/LSOAs_plus_IMD2015_19_plusLAlookup.rds')

#Is size diff due to generalisation? Can check by re-attaching 1 to copy of 2
#Oh except 2 is just England, that won't work...
#Yes, due to generalisation
plot(st_geometry(lsoa[lsoa$zoneID=="E01000001",]))
plot(st_geometry(lsoa2[lsoa2$LSOAcode=="E01000001",]))

#Check ttwas
#Includes Scotland / NI, we don't want that, right?
ttwa <- readRDS('data/ttwa 2011 layer.rds')

#And country code is in code, right? Err what's K?
ttwa$country <- substr(ttwa$ttwa11cd,1,1)

#Ah -K overlap countries, I think
tmap_mode('view')

qtm(ttwa, fill = 'country')

#Export LSOAs and TTWAs to check tesselating and overlap
st_write(lsoa, 'local/lsoa_ew.shp')
st_write(ttwa, 'local/ttwa_uk.shp')


#~~~~~~~~~~~~~~~~~~~~
#TTWA / LSOA CHECKS----
#~~~~~~~~~~~~~~~~~~~~

#In prep for subsetting, finding some issues

#Subset TTWAs to England / Wales, including intersection where two TTWAs dip into Scotland
#Also, check TTWA lookup match, needs a tweak
#Files via Meng Le frontiers github org: https://github.com/life-at-the-frontier/detect-uk-frontiers/tree/test-run/output
lsoa <- readRDS('data/lsoa layer.rds')
ttwa <- readRDS('data/ttwa 2011 layer.rds')

#add country code to ttwa. K overlap countries
ttwa$country <- substr(ttwa$ttwa11cd,1,1)

#LSOA ttwa lookup needs " (2011)" removing
lsoa$ttwa <- gsub(x = lsoa$ttwa, pattern = " (2011)", replacement = "", fixed = T)

#Scots TTWAs need removing for this check
#Six still not matching
table(ttwa$ttwa11nm[ttwa$country %in% c('E','W','K')] %in% lsoa$ttwa)

#Where?
ttwa$check <- ttwa$ttwa11nm %in% lsoa$ttwa

tmap_mode('view')
qtm(ttwa, fill = 'check')

#not mapping, fine.
st_write(ttwa %>% filter(country!="S"), 'local/ttwa_check.shp')


#Problem, after checking in QGIS: LSOAs have NA in TTWA lookup for the K (two country crossing) TTWAs
#Let's see if my own lookup can do any better

#Hasn't got TTWA. FINE.
lookup <- read_csv('../../SheffieldMethodsInstitute/Misc/CrimeDataProjectAug2018/data/oa_lookup2011.csv')


#Doing from scratch then, based on majority area overlap.

#Check lsoa duplicate IDs. Where/what are they?
#use original
lsoa <- readRDS('data/lsoa layer.rds')

#34753 IDs out of 39030 rows
length(unique(lsoa$zoneID))

lsoa <- lsoa %>% 
  group_by(zoneID) %>% 
  mutate(count = n())


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#TEST SPEED OF RELOADING LSOA DATA VS SUBSETTING----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#For getting a single TTWA's worth of LSOA data (using data processed in dataprep.R/ADD TTWA NAMES TO LSOAS MANUALLY)
#Is it faster to load in a saved subset of a single TTWA's worth 
#Or subset directly from larger file? (Loading in the full file isn't time consuming I don't think)

#Will then use to pick out the single focused TTWA in the app (rather than add all to map and hide/show)

lsoa <- readRDS('data/lsoa_layer_w_ttwalookup.rds')

#Subset a sample and save to test. London has 5541 LSOAs so is reasonable max test
saveRDS(lsoa %>% filter(ttwa=='London'), 'local/londonlsoattwatest.rds')

x <- proc.time()
for(i in 1:100) testload <- readRDS('local/londonlsoattwatest.rds')
proc.time() - x

#vs just subsetting
x <- proc.time()
for(i in 1:100) testload <- lsoa %>% filter(ttwa=='London')
proc.time() - x 

#Not a radical difference - ~2 seconds for loading 100 vs 0.8 seconds for subsetting 100
#dplyr filter is one of the faster methods, from googling (cf. data.table but we can't use that I don't think, and is overkill)



#~~~~~~~~~~~~~~~~~~~~~~~~
#CHECK FRONTIERS DATA----
#~~~~~~~~~~~~~~~~~~~~~~~~

#Checking on social frontiers data. Does subsetting using existing list of TTWAs Meng Le made work faster than just filtering?
frontiers <- readRDS(url('https://github.com/life-at-the-frontier/detect-uk-frontiers/raw/main/output/frontier%20borders%20layer.rds'))
#ttwas to check match
ttwa <- readRDS(url('https://github.com/life-at-the-frontier/detect-uk-frontiers/raw/main/output/ttwa%202011%20layer.rds'))

class(frontiers)
class(frontiers[[1]])

#Tick
table(names(frontiers) %in% ttwa$ttwa11nm)

#Dataframe copy
#This should work, doesn't work with sf
#frontiers.df <- bind_rows(frontiers, .id = "ttwa")
# frontiers.df <- bind_rows(frontiers)

#Do manually
frontiers.df <- frontiers[[1]] %>% 
  mutate(ttwa = names(frontiers[1]))

for(i in names(frontiers)[2:length(names(frontiers))]){
  
  # cat(i,'\n')
  
  #Some have no frontiers found, I think (e.g. Penzance)
  #Look out for no find in the app itself, of course... (no way to add zero row with lookup present)
  if(!is.null(frontiers[[i]])){
  
    x <- frontiers[[i]] %>% mutate(ttwa = i)
  
  } 
  
  frontiers.df <- bind_rows(frontiers.df, x)
  
}

#Speed check. List muuuuuuch faster (no actual searching through each row going on)
#1. List
x <- proc.time()
for(i in 1:10000) y <- frontiers[[ttwa$ttwa11nm[1]]]
proc.time() - x

x <- proc.time()
for(i in 1:10000) y <- frontiers.df %>% filter(ttwa == 'Aberystwyth')
proc.time() - x

#Darnit, isn't lon lat
st_crs(frontiers[[1]])



#~~~~~~~~~~~~~~~~~~~~~~~~
#CHECK FRONTIERS GENERALISATION----
#~~~~~~~~~~~~~~~~~~~~~~~~

#Specifically, how the value of dtolerance effects it
#Load from frontiers repo
frontiers <- readRDS(url('https://github.com/life-at-the-frontier/detect-uk-frontiers/raw/main/output/frontier%20borders%20layer.rds'))

#Pick just one as an example to see how generalisation changes lines
x <- frontiers[['Barnsley']]

plot(st_geometry(x))
#plot(st_geometry(x[1,]))

y <- st_simplify(x, dTolerance = 500000000, preserveTopology = F)

plot(st_geometry(y), add=T, col = 'RED')
plot(st_geometry(y[1,]))

#Try casting to spatialpolgons and using rgeos::gsimplify
z <- as_Spatial(x)
z <- gSimplify(z, tol = 10000000, topologyPreserve = F)
plot(st_geometry(st_as_sf(z)), add=T, col = 'RED')



#Transform AFTER simplifying, see meaning of warning here - latitude units vary with latitude
#https://stackoverflow.com/a/60008553/5023561
x <- st_transform(x, "EPSG:4326")

#Let's just check it's working for polygons... Yup
ttwa <- st_read('../../MapPolygons/GreatBritain/2001/TTWAs/greatBritainTTWAs.shp')
plot(st_geometry(ttwa))
ttwa.s <- st_simplify(ttwa, dTolerance = 5000, preserveTopology = F)
plot(st_geometry(ttwa.s))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CHECK RANGE OF MATCHING VARS IN TTWA AND LSOA (FOR MAKING SURE JOINT SCALE BAR WILL WORK OK)----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ttwa <- readRDS('data/ttwa.rds')
lsoa <- readRDS('data/lsoa.rds')

both <- ttwa %>% 
  st_set_geometry(NULL) %>% 
  select(`UK born %`) %>% 
  mutate(source = 'ttwa') %>% 
  rbind(
    lsoa %>% 
      st_set_geometry(NULL) %>% 
      select(`UK born %`) %>% 
      mutate(source = 'lsoa')
  )

#Same scale should work fine? Make combo of both and use that, to make sure all values used...
ggplot(both, aes(x = `UK born %`, colour = source)) +
  geom_density()

