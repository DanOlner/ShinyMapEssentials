#random data checks
library(tidyverse)
library(sf)
library(tmap)

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
#CHECK FRONTIERS 2021----
#~~~~~~~~~~~~~~~~~~~~~~~~

f <- readRDS('data/frontiers_list_2021.rds')

plot(st_geometry(f[[2]]))


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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CHECK PLOTLY FOR 3D MAP DATA PLOT BEFORE PUTTING INTO SHINY----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(plotly)
# library(terra)
library(sf)
# library(fasterize)

#Both fasterize and raster are crashing R

#Example
# data(volcano)

#This is a small number of points. How would it do with many? Probably not well if e.g. London
# fig <- plot_ly(z = ~volcano)
# fig <- fig %>% add_surface()
# fig

#Let's test
lsoa <- readRDS('data/lsoa.rds')
lsoa <- st_transform(lsoa, crs = 'EPSG:27700')

lsoa <- lsoa %>% 
  mutate(`non UK born %` = (100-`UK born %`)*3)
ttwa <- lsoa %>% filter(ttwa == "London")

#https://rdrr.io/github/rspatial/terra/man/rasterize.html
#r <- rast(xmin=0, ncols=80, nrows=80)

#https://r-spatial.github.io/stars/reference/st_rasterize.html
# ttwa.rast <- stars::st_rasterize(ttwa["non UK born %"])
ttwa.rast <- stars::st_rasterize(ttwa["non UK born %"], stars::st_as_stars(st_bbox(ttwa), nx = 800, ny = 800, values = NA_real_))

#Ah OK, NAs are grid areas with no data, obvs.

#Already contains as matrix:
class(ttwa.rast$`UK born %`)

#So, straight into 3D plotly?
#https://stackoverflow.com/a/69824830
fig <- plot_ly(z = ~ttwa.rast$`non UK born %`)
fig <- fig %>% add_surface() %>%
  layout(scene=list(aspectmode='data',
         xaxis = list(title = '', showgrid = F, showline = F, zeroline = F, showticklabels = FALSE),
         yaxis = list(title = '', showgrid = F, showline = F, zeroline = F, showticklabels = FALSE),
         zaxis = list(title = '', showgrid = F, showline = F, zeroline = F, showticklabels = FALSE))
         ) %>% 
  hide_colorbar()

#Attempt to add frontiers over the top
#(Would need to be raised to height of appropriate LSOA z axis)
#We have zone IDs - would want to pick whichever was highest
frontiers <- readRDS('data/frontiers_list.rds')
ttwa.frontiers <- frontiers[['London']]




x <- proc.time()
fig
proc.time() - x



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CHECK ORIG FILE GOING INTO 2011 FRONTIER CALCS----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#This is what went into 2011 frontier calcs
cleanedcob <- readRDS('local/cleaned cob lsoa ttwa.rds')

#How does that number compare to the number of LSOAs in the dashboard LSOA generalised file?
lsoa.dash <- readRDS('data/lsoa.rds')

#NO!!



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#COMPARE 2011 2021 LSOA COB----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cob11 <- readRDS('local/cleaned cob lsoa ttwa.rds') %>% mutate(censusYear = 2011) %>% mutate(`UK born %` = (ukBorn/allResidents) * 100)
cob21 <- readRDS('local/lsoa2021_aggd_to_2011_geog_via_OA_COBdata2021_readyforfrontiercalcs.rds') %>% mutate(censusYear = 2021)

#Sanity check match... tick
table(cob11$zoneNm %in% cob21$zoneNm)

cobz <- rbind(cob11 %>% st_set_geometry(NULL),cob21 %>% st_set_geometry(NULL))

#Sanity check on COB numbers
#https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/internationalmigration/bulletins/internationalmigrationenglandandwales/census2021
#"One in six usual residents of England and Wales were born outside the UK, an increase of 2.5 million since 2011, from 7.5 million (13.4%) to 10 million (16.8%)."
#13.4% to 16.8%

#simple totals plz... spot on. Phew.
chk <- cobz %>% 
  group_by(censusYear) %>% 
  summarise(
    ukBorn = sum(ukBorn), 
    nonUKBorn = sum(nonUKBorn),
    percentUKborn = ((ukBorn/(nonUKBorn+ukBorn)) * 100),
    percentNonUKborn = 100 - percentUKborn
    )


#That's a lot, can we correlate them all?
#Actually, we want censusYear in its own column for x / y change plot
# cobz_wide <- cobz %>% 
#   select(censusYear,zoneID,`UK born %`,ttwa11nm) %>% 
#   pivot_wider(names_from = censusYear, values_from = `UK born %`)

cobz_wide <- cobz %>% 
  select(censusYear,zoneID,`UK born %`,ukBorn,nonUKBorn,ttwa11nm) %>% 
  pivot_wider(names_from = censusYear, values_from = c(`UK born %`,ukBorn,nonUKBorn))

ggplot(cobz_wide, aes(x = `2011`, y = `2021`)) +
  geom_point() +
  geom_abline(colour='red')

#OK, let's check for major cities
#Pop totals for TTWAs, pick a few top ones and look

#Or actually, find ppt change then save and have a gander in QGIS
#NO!
# cobz_ppt_change <- cobz %>% 
#   group_by(censusYear) %>% 
#   mutate(ppt_change = lag(`UK born %`,order_by=censusYear) - `UK born %`)

#Easier!
cobz_ppt_change <- cobz_wide %>% 
  mutate(
    ppt_change = `UK born %_2021` - `UK born %_2011`,
    ukBorn_change_aspercentof2001total = ((ukBorn_2021 - ukBorn_2011)/ukBorn_2011)*100,
    non_ukBorn_change_aspercentof2001total = ((nonUKBorn_2021 - nonUKBorn_2011)/nonUKBorn_2011)*100,
    isLondon = ifelse(ttwa11nm=='London','London','rest Of Eng/Wales'),
    ppt_change_polarity = ifelse(ppt_change > 0, '% UK born increased','% UK born decreased')
    )

#Add back in to the geography and save
# st_write(
#   cob11 %>% select(zoneID) %>% 
#     left_join(cobz_ppt_change, by = 'zoneID'),
#   'local/qgis/lsoa_cob_pptchange.shp'
# )

#Actually, let's attach that to a less generalised geog
lsoa2011.eng <- st_read('../../MapPolygons/England/2011/England_lsoa_2011_gen_clipped/england_lsoa_2011_gen_clipped.shp')
lsoa2011.wales <- st_read('../../MapPolygons/Wales/2011/Wales_lsoa_2011_gen_clipped/wales_lsoa_2011_gen_clipped.shp')

lsoa2011 <- rbind(lsoa2011.eng,lsoa2011.wales) %>% rename(zoneID = code)

# lsoa2011 <- lsoa2011 %>% st_transform(crs = "EPSG:4326")
st_write(
  lsoa2011 %>% select(zoneID) %>% 
    left_join(cobz_ppt_change, by = 'zoneID'),
  'local/qgis/lsoa_cob_pptchange.shp' 
)


#Get list of  TTWAs in 2021, order by size, use some for facetting
ttwas.pop <- cobz %>% 
  group_by(ttwa11nm) %>% 
  summarise(pop = sum(allResidents)) %>% 
  arrange(-pop)


#Facet some top places 11-21 x y 
ggplot(cobz_wide %>% filter(ttwa11nm %in% ttwas.pop$ttwa11nm[1:12]), 
       aes(x = `UK born %_2011`, y = `UK born %_2021`)) +
  geom_point() +
  geom_abline(colour='red') +
  facet_wrap(~ttwa11nm)



#Check - places that changed the most, how did change size compare to actual UK-born in 2011?
ggplot(cobz_ppt_change, aes(x = `UK born %_2011`, y = ppt_change)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method='loess')

ggplot(cobz_ppt_change%>% filter(ttwa11nm %in% ttwas.pop$ttwa11nm[1:12]), 
       aes(x = `UK born %_2011`, y = ppt_change)) +
  geom_point(alpha=0.5) +
  geom_hline(yintercept=0, colour='red') +
  facet_wrap(~ttwa11nm, scales = 'free_y')
  

#looking at the London map and the clusters...
#Flag if change is pos/neg then compare to % UK born in 2011
#... done above

#Check for London first
ggplot(
  cobz_ppt_change %>% filter(ttwa11nm=='London'),
  aes(x = ppt_change_polarity, y = `UK born %_2011`)
  ) +
  geom_jitter(alpha=0.1) +
  geom_violin(alpha=0.5)

#Looking like foreign born leaving from higher foreign born areas. 
#Gonna have to check relative % change of each, not overall % change.
#Or rather - what we're after: if % UK born increased in a zone,
#What proportion of that increase was due to UK born numbers going up,
#What proportion due to non UK born leaving that zone?
#So yes: we need proportion / ppt change between Censuses for each of UK born and non-UK born
#Or actually, not ppt but actual percentage
#Of course, we can't track actual people with this data, but can compare counts and speculate

#Same for Eng/Wales?
#Flag London, compare
ggplot(cobz_ppt_change, 
       aes(x = ppt_change_polarity, y = `UK born %_2011`)) +
  geom_jitter(alpha=0.1) +
  geom_violin(alpha=0.5) +
  facet_wrap(~isLondon)

#Looking like London's quite different. It's gonna be EU migrants isn't it?
#We already know this data, presumably?
#From the link above:
#Those born in the European Union (EU) made up 3.6 million (36.4% of all non-UK born usual residents) of the population, an increase from 2.5 million (32.7%) in 2011 (including Croatia, who joined the EU in 2013). The remaining 6.4 million (63.6%) were born outside the EU, up from 5.1 million (67.3%) in 2011.
#So EU numbers up overall. What has happened in London??

#OK, have looked in QGIS: % non UK change between 11 and 21 is mainly responsible - the non UK drop, not the UK gain.
#Next check is the ratio of those two...


#OK, I have those values nows. What do I want to see?
ggplot(cobz_ppt_change , 
       aes(x = ppt_change, y = non_ukBorn_change_aspercentof2001total)) +
  geom_point() +
  facet_wrap(~isLondon)

#Err
ggplot(cobz_ppt_change, 
       aes(x = ukBorn_change_aspercentof2001total, y = non_ukBorn_change_aspercentof2001total)) +
  geom_point(alpha=0.3) +
  facet_wrap(~isLondon) +
  geom_vline(xintercept=0, colour='red') +
  geom_hline(yintercept=0, colour='red') +
  coord_fixed(xlim = c(-100,200),ylim = c(-100,200)) 


#Let's mark the four quadrants there, and map
cobz_ppt_change <- cobz_ppt_change %>% 
  mutate(
    percentChangeQuadrants = case_when(
      ukBorn_change_aspercentof2001total > 0 & non_ukBorn_change_aspercentof2001total > 0 ~ "both pos",
      ukBorn_change_aspercentof2001total <= 0 & non_ukBorn_change_aspercentof2001total <= 0 ~ "both neg",
      ukBorn_change_aspercentof2001total > 0 & non_ukBorn_change_aspercentof2001total <= 0 ~ "ukBorn pos/non ukBorn neg",
      ukBorn_change_aspercentof2001total <= 0 & non_ukBorn_change_aspercentof2001total > 0 ~ "ukBorn neg/non ukBorn pos",
    )
)


cobz.geo <- lsoa2011 %>% 
  select(-name) %>% 
  right_join(cobz_ppt_change, by='zoneID')

tm_shape(cobz.geo %>% filter(ttwa11nm=='London')) +
  tm_polygons('percentChangeQuadrants', border.alpha = 0)

#let's save that to look at in context
st_write(cobz.geo, 'local/qgis/lsoa_cob_pptchange_plusotherstuff.shp')



#Think there's some overlap with ppt change
#Except it's mostly tautological... 
ggplot(cobz_ppt_change, aes(x = percentChangeQuadrants, y = ppt_change)) +
  geom_jitter() +
  geom_hline(yintercept = 0, colour='red')





##IMD COMPARISON----


#Add in IMD2019. Any pattern match? Probably purely because more urban, but let's see.
imd <- readRDS('../../imd2019/data/imd2015_2019_priorToSeparatingByLA.rds') %>% rename(zoneID = code)

#Which is just for England, so...
table(imd$zoneID %in% cobz_wide$zoneID)


#Compare just to 2019 deciles, for plotting.
cobz_eng_imd <- cobz_wide %>% 
  right_join(
    imd %>% st_set_geometry(NULL) %>% select(-LAD17NM,-Decile2015,-Rank2015),
    by = 'zoneID'
    ) %>% 
  mutate(ppt_diff = `2021`-`2011`)
  

#Plot again, colour deciles
ggplot(cobz_eng_imd %>% filter(ttwa11nm %in% ttwas.pop$ttwa11nm[1:12]), 
       aes(x = `2011`, y = `2021`, colour = factor(Decile2019))) +
  geom_point() +
  geom_abline(colour='red') +
  facet_wrap(~ttwa11nm)


#OK, just London
lon <- cobz_eng_imd %>% 
  filter(ttwa11nm=="London")

x <- lm(data = lon, formula = ppt_diff ~ factor(Decile2019))
summary(x)

#Let's look
lon <- lon %>% 
  mutate(ppt_diff_polarity = ppt_diff > 0)

#Crap, not spatial. Add back in
lon.geo <- lsoa2011 %>% 
  select(-name) %>% 
  right_join(lon, by='zoneID')

tm_shape(lon.geo) +
  tm_polygons('ppt_diff_polarity') +
  tm_facets('Decile2019')

tm_shape(lon.geo %>% mutate(Decile2019 = factor(Decile2019))) +
  tm_polygons('Decile2019')



#Maybe not helpful. How about...
ggplot(lon, aes(x = factor(Decile2019), y = ppt_diff)) +
  geom_jitter() +
  geom_hline(yintercept = 0, colour='red')

#Actually, let's do that for England...
ggplot(cobz_eng_imd, aes(x = factor(Decile2019), y = ppt_diff)) +
  geom_jitter() +
  geom_hline(yintercept = 0, colour='red', size = 2)

#London different from England?
ggplot(cobz_eng_imd %>% mutate(Londonflag = ttwa11nm=='London'), 
       aes(x = factor(Decile2019), y = ppt_diff)) +
  geom_jitter(alpha=0.2) +
  geom_hline(yintercept = 0, colour='red', size = 2) +
  facet_wrap(~Londonflag)

ggplot(cobz_eng_imd %>% mutate(Londonflag = ttwa11nm=='London'), 
       aes(x = factor(Decile2019), y = ppt_diff)) +
  geom_violin() +
  geom_hline(yintercept = 0, colour='red', size = 2) +
  facet_wrap(~Londonflag)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2011/21 SOCIAL FRONTIER JOIN AND COMPARISON----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#As a prelude to working out what to show on the dashboard

#Job 1: join the two decades, compare frontier diffs (sizes if same place, compare if some don't appear in diff decades)

#They're both currently in lists - delist and combine, mark years
f11 <- readRDS('data/frontiers_list.rds')
f21 <- readRDS('data/frontiers_list_2021.rds')


#Nabbed from above to get TTWA names in the joined dfs
#Do manually
frontiers.list.to.df <- function(frontiers){

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
  
  return(frontiers.df)

}


f11.df <- frontiers.list.to.df(f11) %>% mutate(decade=2011)
f21.df <- frontiers.list.to.df(f21) %>% mutate(decade=2021)

#Get numeric versions of zone labels so we can pick unique pairs by ordering
f11.df <- f11.df %>% 
  mutate(
    zoneA.numeric = as.numeric(stringr::str_sub(zoneID_a,-7)),
    zoneB.numeric = as.numeric(stringr::str_sub(zoneID_b,-7))
         )

f21.df <- f21.df %>% 
  mutate(
    zoneA.numeric = as.numeric(stringr::str_sub(zoneID_a,-7)),
    zoneB.numeric = as.numeric(stringr::str_sub(zoneID_b,-7))
         )


#Matching frontiers are ones where zoneA-zoneB pair are the same. Mark those in both for checking

#Work out separately and can then join on the border-pair, 
#So we can have wide Census where NAs will be "in only one Census", and label which one

f11.dfx <- f11.df %>% mutate(borderpairlabel = paste0(zoneID_a,":",zoneID_b))

#Ah, it's not quite that easy. Need to make sure it's a single unique pair
#1:2 is the same as 2:1 and not sure they all match in both
#So, need to order in some way for each one to make sure (have made numerical above for this purpose)
f11.df <- f11.df %>% 
  mutate(
    borderpairlabel = case_when(
      zoneA.numeric < zoneB.numeric ~ paste0(zoneID_a,":",zoneID_b),
      zoneA.numeric > zoneB.numeric ~ paste0(zoneID_b,":",zoneID_a)
    )
  )

f21.df <- f21.df %>% 
  mutate(
    borderpairlabel = case_when(
      zoneA.numeric < zoneB.numeric ~ paste0(zoneID_a,":",zoneID_b),
      zoneA.numeric > zoneB.numeric ~ paste0(zoneID_b,":",zoneID_a)
    )
  )

#check match
table(f11.df$borderpairlabel %in% f21.df$borderpairlabel)
#A lot more borders in 2021?
table(f21.df$borderpairlabel %in% f11.df$borderpairlabel)

#It shouldn't be possible for that TRUE number to be different between the two
#Unless there are duplicates?
length(unique(f11.df$borderpairlabel))
length(unique(f21.df$borderpairlabel))

#Yes, a small number in f21.
#Let's label the duplicates and look
f21.df <- f21.df %>% 
  group_by(borderpairlabel) %>% 
  mutate(groupsize = n())

View(f21.df %>% filter(groupsize > 1))

#All 2-dups, and all in Berwick for some reason
#MOre importantly, all full duplicates with matching values
#So it's safe to drop them
f21.df <- f21.df %>% 
  filter(groupsize==1) %>% 
  select(-groupsize)

#Now check match... TICK.
table(f11.df$borderpairlabel %in% f21.df$borderpairlabel)
table(f21.df$borderpairlabel %in% f11.df$borderpairlabel)

#Maybe save both of those huh?
saveRDS(f11.df, 'local/frontier_borderpairs2011.rds')
saveRDS(f21.df, 'local/frontier_borderpairs2021.rds')

#So the TRUE in both are frontiers that have been found for both decades
#Though will have differing phi values
#All false in each will be unique to each decade. and that number is large


#So, joining. Some notes:
#1. We only need std_diff_phi and a label of which decade it's from
#2. But for x / y plots to compare changes, that'll have to be made wide
#But but, can do that after joining long I think, as long will have more info

#Actually, remembering that both are spatial...

#At the moment, we've kept only normalised phi > 1.96.
#So if we do that for both decades, how many actual frontiers do we keep that are the same?
f11.df.phicutoff <- f11.df %>% filter(std_diff_phi > 1.96)
f21.df.phicutoff <- f21.df %>% filter(std_diff_phi > 1.96)

#Or test without filter...
# f11.df.phicutoff <- f11.df
# f21.df.phicutoff <- f21.df

#6306 in 2011, 6051 in 2021.
#How many match between the two?
#Make a df that describes that

#2011
f11.df.phicutoff <- f11.df.phicutoff %>% 
  mutate(decadematch = ifelse(borderpairlabel %in% f21.df.phicutoff$borderpairlabel, 'both','2011'))
#2021
f21.df.phicutoff <- f21.df.phicutoff %>% 
  mutate(decadematch = ifelse(borderpairlabel %in% f11.df.phicutoff$borderpairlabel, 'both','2021'))

#Keeping just border pair label and decade marker label
#Can make single column from the two created, working with the NAs
chk <- f11.df.phicutoff %>% 
  st_set_geometry(NULL) %>% 
  select(borderpairlabel, decadematch) %>% 
  full_join(
    f21.df.phicutoff %>%
      st_set_geometry(NULL) %>% 
      select(borderpairlabel, decadematch),
    by = 'borderpairlabel'
  )


chk <- chk %>% 
  mutate(
    decadematch = case_when(
      decadematch.x=='both' ~ 'both',
      is.na(decadematch.x) ~ '2021',
      is.na(decadematch.y) ~ '2011'
    )
  )

# This is with no phi filter:
# 2011  2021  both 
# 8918 15606 26072

# This is phi > 1.96
# 2011 2021 both 
# 2815 2560 3491
table(chk$decadematch)


#Let's do the ol' look-see in QGIS shall we?
st_write(f11.df, 'local/qgis/frontiers2011.shp')
st_write(f21.df, 'local/qgis/frontiers2021.shp')



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CHECK LSOA 2011 / 2021 FILES, FRONTIER FILES, MAKE SAME STRUCTURE----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#What CRSs are they too?

l11 <- readRDS('data/lsoa2011.rds')
l21 <- readRDS('local/lsoa2021_aggd_to_2011_geog_via_OA_COBdata2021_readyforfrontiercalcs.rds')

frontiers.original.list.2011 <- readRDS('data/frontiers_list_2011.rds')
frontiers.original.list.2021 <- readRDS('data/frontiers_list_2021.rds')

st_crs(l11)
st_crs(l21)

st_crs(frontiers.original.list.2011[[1]])
st_crs(frontiers.original.list.2021[[2]])

#So, LSOA needs updating and rewriting to EPSG:4326
#That took longer than usual!
l21 <- l21 %>% st_transform(crs = 'EPSG:4326')

saveRDS(l21, 'data/lsoa2021.rds')


#CHECK PACKAGE DEPENDENCIES, LOOKING FOR RGEOS
avail_pks <- available.packages()

deps <- tools::package_dependencies(packages = avail_pks[c('shiny',
                                                           'tidyverse',
                                                           'sf',
                                                           'leaflet',
                                                           'plotly',
                                                           'bslib',
                                                           'knitr',
                                                           'toOrdinal',
                                                           'shinyWidgets',
                                                           'DT'), "Package"],
                                    recursive = TRUE)

deps <- unlist(deps)
table(deps=='rgeos')




install.packages(c('shiny',
                   'tidyverse',
                   'sf',
                   'leaflet',
                   'plotly',
                   'bslib',
                   'knitr',
                   'toOrdinal',
                   'shinyWidgets'))


