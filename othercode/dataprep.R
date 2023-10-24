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
lsoa <- lsoa %>% mutate('UK born %' = (ukBorn/allResidents)*100)
# lsoa <- lsoa %>% mutate(UKborn_percent| = (ukBorn/allResidents)*100)

#Make ttwa variable with the same name to match
ttwa <- ttwa %>% mutate('UK born %' = (1-prop_foreign_born)*100)



#Frontiers are in list, each element a TTWA...
#Frontiers not currently lon lat. Convert.

#Also, simplify to see if speed can be increased
#Or actually, don't simplify too much, see if it runs OK
#(Otherwise doesn't match LSOA borders)

frontiers.proc <- lapply(frontiers, function(x) {
  
  if(!is.null(x)) {
    
    # x <- st_simplify(x, dTolerance = 1000, preserveTopology = F)
    x <- st_transform(x, "EPSG:4326")
    
  }
  
  
})

st_crs(frontiers.proc[[1]])

#REPEAT FOR 2021 FRONTIERS, TESTING WITHOUT SIMPLIFY FOR NOW
frontiers.proc2 <- lapply(readRDS('data/frontiers_list_2021.rds'), function(x) {
  
  if(!is.null(x)) {
    
    # x <- st_simplify(x, dTolerance = 1000, preserveTopology = F)
    x <- st_transform(x, "EPSG:4326")
    
  }
  
  
})

st_crs(frontiers.proc2[[1]])


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
saveRDS(frontiers.proc, 'data/frontiers_list_2011.rds')
saveRDS(frontiers.proc2, 'data/frontiers_list_2021.rds')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Make 2021 LSOA / 2011 TTWA lookup using the TTWA data we use here----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#geoportal has OA > TTWA and OA > LSOA but let's just do this.
ttwa <- read_rds('data/ttwa.rds') %>% select(ttwa)

lsoa21.geo.eng <- st_read('../../MapPolygons/England/2021/England_lsoa_2021_bgc/england_lsoa_2021_bgc.shp')
lsoa21.geo.wales <- st_read('../../MapPolygons/Wales/2021/Wales_lsoa_2021_bgc/wales_lsoa_2021_bgc.shp')

lsoa21.geo <- rbind(lsoa21.geo.eng,lsoa21.geo.wales) %>% select(lsoa21cd,name)

#Match TTWA CRS used in the dashboard
lsoa21.geo <- lsoa21.geo %>% st_transform(4326)

sf::sf_use_s2(FALSE)

x <- proc.time()
interz <- st_intersection(lsoa21.geo,ttwa)
proc.time() - x

#Won't be perfectly tesselating... this number of 2+ LSOAs in different TTWAs
nrow(interz) - length(unique(interz$lsoa21cd))

#We want to keep only the LSOAs with the largest area in a TTWA.
interz$area <- st_area(interz) %>% as.numeric()

#Group and rank in order of area size
#https://stackoverflow.com/questions/24237399/how-to-select-the-rows-with-maximum-values-in-each-group-with-dplyr
#Selects by last col by default, area in this case
interz <- interz %>% 
  group_by(name) %>% 
  top_n(n = 1)

#Now one lsoa per ttwa match, tick
nrow(interz) - length(unique(interz$lsoa21cd))


#Check sanity in QGIS
#Not writing...
st_write(interz, 'local/qgis/check_lsoa21_ttwamatch.geojson')

#Looking correct. Only need lookup, not geog.
plot(interz['ttwa'])

#save just lookup
saveRDS(
  interz %>% ungroup() %>% st_set_geometry(NULL) %>% select(lsoa21cd, ttwa),
  'local/lsoa2021_ttwa2011_lookup.rds'
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#USING LOOKUP FILES via geoportal: Make 2021 LSOA / 2011 TTWA lookup using the TTWA data we use here----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Grab OA21 > TTWA lookup
#And LSOA21 > OA21 lookup

#Do that way.
#https://geoportal.statistics.gov.uk/datasets/output-area-2021-to-ttwas-2011-to-lad-2022-lookup-for-england-and-wales-1/explore
# oalookup <- read_csv('local/Output_Area_(2021)_to_TTWAs_(2011)_to_LAD_(2022)_Lookup_for_England_and_Wales.csv')

#OA / LSOA lookup
#



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2021 CENSUS DATA: LSOA / MAP DATA LINK USING 2021 LSOA BOUNDARIES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

lsoa21 <- read_csv('local/census2021_CountryOfBirth_LSOA.csv')

#We want England and Wales LSOAs
lsoa21.geo.eng <- st_read('../../MapPolygons/England/2021/England_lsoa_2021_bgc/england_lsoa_2021_bgc.shp')
lsoa21.geo.wales <- st_read('../../MapPolygons/Wales/2021/Wales_lsoa_2021_bgc/wales_lsoa_2021_bgc.shp')

lsoa21.geo <- rbind(lsoa21.geo.eng,lsoa21.geo.wales)

#Add in TTWA 2011 lookup
ttwalookup <- readRDS('local/lsoa2021_ttwa2011_lookup.rds')

#doublecheck match... TICK
table(lsoa21.geo$lsoa21cd %in% ttwalookup$lsoa21cd)

lsoa21.geo <- lsoa21.geo %>% left_join(ttwalookup, by = 'lsoa21cd')

#save and check ttwa match was OK
# st_write(lsoa21.geo, 'local/qgis/lsoa21_ttwa_check.shp')
# #check match with 2011 file
# lsoa2011 <- readRDS('data/lsoa.rds')
# st_write(lsoa2011, 'local/qgis/lsoa11_ttwa_check.shp')

#Checked - only TTWA changes are where LSOAs changed shape in 2021. One changed which was the most appropriate TTWA

#cob in long form
unique(lsoa21$`Country of birth (22 categories)`)

lsoa21wide <- lsoa21 %>% 
  select(-`Country of birth (22 categories) Code`) %>% 
  pivot_wider(names_from = `Country of birth (22 categories)`, values_from = Observation)

#Does 'does not apply' have any values? Newp.
sum(lsoa21wide$`Does not apply`)
sum(lsoa21wide$Other)

#Check geo / data lsoa ID match... TICK
table(lsoa21.geo$lsoa21cd %in% lsoa21wide$`Lower layer Super Output Areas Code`)


#GET UK / NON UK BORN SUMS
lsoa21wide <- lsoa21wide %>% 
  mutate(
    ukBorn = rowSums(across(`Europe: United Kingdom: England`:`Europe: United Kingdom: United Kingdom not otherwise specified`)),
    nonUKBorn = rowSums(across(`Europe: Ireland`:Other)),
    `UK born %` = ((ukBorn /(ukBorn + nonUKBorn)))*100
  )

#Sensible? Yup.
plot(density(lsoa21wide$`UK born %`))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#PRIOR TO AGGREGATING 2021 OAs to 2011 LSOAs, make a bespoke 2011 LSOA file that matches the (missing 10 LSOA) dashboard LSOA data----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Why are we not just using the dashboard LSOA data directly to aggregate the OA data into?
#Cos it's highly generalised and I'm pretty sure there'd be far more incorrect OA >> LSOA assignments

#What does this need?
#1. Original 2011 LSOAs:
lsoa2011.eng <- st_read('../../MapPolygons/England/2011/England_lsoa_2011_gen_clipped/england_lsoa_2011_gen_clipped.shp')
lsoa2011.wales <- st_read('../../MapPolygons/Wales/2011/Wales_lsoa_2011_gen_clipped/wales_lsoa_2011_gen_clipped.shp')

lsoa2011 <- rbind(lsoa2011.eng,lsoa2011.wales)

lsoa2011 <- lsoa2011 %>% st_transform(crs = "EPSG:4326")

#2. The dashboard LSOAs, that are highly generalised to load quickly, and have 10 missing
lsoa.dash <- readRDS('data/lsoa.rds')

#The latter will have ten missing...
table(lsoa2011$code %in% lsoa.dash$zoneID)

#The missing ones are almost surrounded by larger LSOAs.
#If we can find what missing LSOAs overlap the most in the dashboard version
#We can merge those ones together with the larger one, getting the correct name to match
#Let's see if that works...
missings <- lsoa2011[!(lsoa2011$code %in% lsoa.dash$zoneID),]

#Intersect them both (with some helpful labelling)
interz <- st_intersection(
  lsoa.dash %>% select(dashID = zoneID, dashname = zoneNm), 
  missings %>% select(missingsname = name, missingsID = code)
  )

#Good ol' "what's biggest area overlap" trick
interz$area <- st_area(interz) %>% as.numeric()



#Check the distribution of area sizes. How many are anywhere close to e.g. 30-50% split over different LSOAs?
#Find area proportions, keep only maximum one
interz.check <- interz %>% 
  group_by(missingsID) %>% 
  mutate(
    areasum = sum(area),
    areaproportions = area / sum(area)
  ) %>% 
  top_n(n = 1)

#Plots the single highest OA area proportion.
plot(hist(interz.check$areaproportions))
#Very small number of highly split OAs, good to go
plot(hist(interz.check$areaproportions[interz.check$areaproportions < 0.9]))



#Pick largest area OA chunk
interz.largest <- interz %>% 
  group_by(missingsID) %>% 
  top_n(n = 1)

#TICK. Only unique left
nrow(interz.largest) - length(unique(interz.largest$missingsID))


#For interz.largest, dashID and missingsID are the ones we want to combine into single IDs
#There is a change the Nuneaton one is wrong (only 58% area match) but let's see
#I think the only way to check is going to be looking in QGIS again
#There's no way to tell if it matches exactly just from IDs, once merged

#Create a list of the joined sfs we'll then drop in to replace the ones in the orig lsoa2011 sf
#NOTE: THE UNIONS ARE CREATING SOME ARTIFACTS
#But it shouldn't matter for frontier calcs, which will use border intersects?
joinz <- lapply(1:nrow(interz.largest), function(x) st_union(lsoa2011 %>% filter(code==interz.largest$dashID[x]), lsoa2011 %>% filter(code==interz.largest$missingsID[x])))
# joinz <- bind_rows(joinz) %>% select(code = code.x, name = name.x)
joinz <- bind_rows(joinz) %>% select(code, name)

#Check code.x got all the lsoa.dash names we want... tick
joinz$code %in% interz.largest$dashID

#Now... need to DROP ALL THOSE from the orig lsoa file before we can re-rbind these joins to it
lsoa2011.minus.onestoreplace <- lsoa2011 %>% 
  filter(!code %in% c(interz.largest$dashID,interz.largest$missingsID))

#Names all match, should just be able to...
lsoa2011.bespoke <- rbind(lsoa2011.minus.onestoreplace,joinz)

#Matching IDs to dashboard version? TICK
table(lsoa2011.bespoke$code %in% lsoa.dash$zoneID)

#Last thing: eyeball in QGIS that we've got the right assignments
st_write(lsoa2011.bespoke, 'local/qgis/checkbespokeLSOA2011.shp')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#INSTEAD OF USING 2021 LSOA BOUNDARIES, AGGREGATE 2021 OAS TO 2011 BOUNDARIES----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#That way matching geographies as we compare the two. 


#GET 2021 OA COB VALUES AND REDUCE TO UK / NON UK
CoB.OA.21 <- read_csv('local/custom-filtered-OA2021_plus_countryOfBirth.csv')

#Sum for UK / non UK born
#cob in long form
#These seem like slightly odd cats, might want to check nothing missing!
unique(CoB.OA.21$`Country of birth (13 categories)`)

CoB.OA.21.wide <- CoB.OA.21 %>% 
  select(-`Country of birth (13 categories) Code`) %>% 
  pivot_wider(names_from = `Country of birth (13 categories)`, values_from = Observation)

#Does 'does not apply' have any values? Newp.
sum(CoB.OA.21.wide$`Does not apply`)


#GET UK / NON UK BORN SUMS
CoB.OA.21.wide <- CoB.OA.21.wide %>% 
  mutate(
    ukBorn = rowSums(across(`Europe: United Kingdom: England`:`Europe: United Kingdom: United Kingdom not otherwise specified`)),
    nonUKBorn = rowSums(across(`Europe: Ireland`:`Antarctica and Oceania (including Australasia)`)),
    `UK born %` = ((ukBorn /(ukBorn + nonUKBorn)))*100
  )

#Sensible? Yup. Looking pretty close to LSOA dist.
plot(density(CoB.OA.21.wide$`UK born %`))

#Have just loaded above, let's check totals...
#Good enough for jazz. Very close.
#Though a puzzle why there'd be a difference at all - LSOA values rounded, rather than using disclosure perturbing?
sum(lsoa21wide$ukBorn)
sum(CoB.OA.21.wide$ukBorn)
sum(lsoa21wide$nonUKBorn)
sum(CoB.OA.21.wide$nonUKBorn)



#GET THE 2021 OA GEOGRAPHIES, ASSIGN SINGLE OAS TO SINGLE 2011 LSOA

#Combine England and Wales OAs
oa.geo21.eng <- st_read('../../MapPolygons/England/2021/England_oa_2021_bgc/england_oa_2021_bgc.shp')
oa.geo21.wales <- st_read('../../MapPolygons/Wales/2021/Wales_oa_2021_bgc/wales_oa_2021_bgc.shp')

oa.geo21 <- rbind(oa.geo21.eng,oa.geo21.wales)

#Check code match with data. TICK.
table(CoB.OA.21$`Output Areas Code` %in% oa.geo21$oa21cd)

#Get 2011 LSOA geo data
#Data used in the dashboard is quite generalised, use orig for better OA assignment
lsoa2011.eng <- st_read('../../MapPolygons/England/2011/England_lsoa_2011_gen_clipped/england_lsoa_2011_gen_clipped.shp')
lsoa2011.wales <- st_read('../../MapPolygons/Wales/2011/Wales_lsoa_2011_gen_clipped/wales_lsoa_2011_gen_clipped.shp')

lsoa2011 <- rbind(lsoa2011.eng,lsoa2011.wales)

#intersect to check where most of each OA2021 sits in older 2011 LSOAs
x <- proc.time()
interz <- st_intersection(lsoa2011, oa.geo21)
proc.time() - x

#Won't be perfectly tesselating... this number of 2+ LSOAs in different TTWAs
nrow(interz) - length(unique(interz$oa21cd))

#I think most of the overlaps are just generalisation differences, not real 11/21 diffs

#Get area to keep single OA with LSOA11 label
interz$area <- st_area(interz) %>% as.numeric()

#Check the distribution of area sizes. How many are anywhere close to e.g. 30-50% split over different LSOAs?
#Find area proportions, keep only maximum one
interz.check <- interz %>% 
  group_by(oa21cd) %>% 
  mutate(
    areasum = sum(area),
    areaproportions = area / sum(area)
    ) %>% 
  top_n(n = 1)

#Plots the single highest OA area proportion.
plot(hist(interz.check$areaproportions))
#Very small number of highly split OAs, good to go
plot(hist(interz.check$areaproportions[interz.check$areaproportions < 0.9]))

#Pick largest area OA chunk
interz <- interz %>% 
  group_by(oa21cd) %>% 
  top_n(n = 1)

#TICK. Only unique OAs left, with unique LSOA match
nrow(interz) - length(unique(interz$oa21cd))

#Next part of plan:
#Join CoB data
#Sum up for each LSOA 2011


#Join OA 21 / LSOA 2011 lookup to CoB data (don't need geography now)
CoB.OA.21.wide <- CoB.OA.21.wide %>% 
  select(`Output Areas Code`,ukBorn,nonUKBorn,`UK born %`) %>% 
  left_join(
    interz %>% st_set_geometry(NULL) %>% select(name,lsoacode = code,oa21cd),
    by = c("Output Areas Code" = "oa21cd")
  )

#Correct number of LSOAs
length(unique(CoB.OA.21.wide$lsoacode))


#MAKE LSOA 2011 sums of CoB FROM 2021 OA COB VALUES
COB.lsoa2011.fromOA2021 <- CoB.OA.21.wide %>% 
  select(-`UK born %`) %>% 
  group_by(lsoacode) %>% 
  summarise(
    ukBorn = sum(ukBorn),
    nonUKBorn = sum(nonUKBorn),
    )

#Add back in new % for lsoa2011 values
COB.lsoa2011.fromOA2021 <- COB.lsoa2011.fromOA2021 %>% 
  mutate(`UK born %` = ((ukBorn /(ukBorn + nonUKBorn)))*100)





#Add in TTWA labels, which we should now be able to do directly from the data we already have
#DONE BELOW USING ORIGINAL FILE WITH CORRECT LSOA NUMBER
# lsoa <- readRDS('data/lsoa.rds')
# 
# #Ten missing, suspect that might be the isles of scilly (cos only a single LSOA, no frontiers)
# table(COB.lsoa2011.fromOA2021$lsoacode %in% lsoa$zoneID)
# 
# #All match in the dashboard lsoa data
# table(lsoa$zoneID %in% COB.lsoa2011.fromOA2021$lsoacode)
# 
# #Check I'm right about the location
# x <- COB.lsoa2011.fromOA2021[!(COB.lsoa2011.fromOA2021$lsoacode %in% lsoa$zoneID),]
# 
# #Doh, need orig file
# #Hmm, no, those are random locations. What gives?
# lsoa2011$name[lsoa2011$code %in% x$lsoacode]
# 
# #OK, some checks on what's going on.
# #First note that the orig lsoa 2011 file has ten fewer rows.
# #Save that to viz in QGIS - are they missing?
# 
# #Actually, let's save lsoa2011 flagging the missing ones so we can see easily
# lsoa2011 <- lsoa2011 %>% 
#   mutate(missing = lsoa2011$code %in% x$lsoacode)
# 
# #CHECK IN QGIS
# st_write(lsoa2011, 'local/qgis/checkLSOA2011missings.shp')
# #Save dashboard LSOA file to compare
# st_write(lsoa, 'local/qgis/dashboardlsoas2011.shp')



#Orig file that went into the 2011 frontier calcs, has correct LSOA number
lsoa.orig <- readRDS('local/cleaned cob lsoa ttwa.rds')

#check lsoa code match... TICK.
table(lsoa.orig$zoneID %in% COB.lsoa2011.fromOA2021$lsoacode)

#Merge in TTWA codes.
#KEEP lsoa.orig as BASE because it has the geography in already.
#All we need to do is replace the UK born counts (and make an "allResidents" sum for the frontier calc denom)
final2021into2011 <- lsoa.orig %>% 
  select(-allResidents,-ukBorn,-nonUKBorn) %>% 
  left_join(
    COB.lsoa2011.fromOA2021 %>% rename(zoneID = lsoacode),
    by = "zoneID"
  ) %>%
  mutate(allResidents = ukBorn + nonUKBorn)

#No NAs? Tick. That took weirdly long.
table(is.na(final2021into2011$ukBorn))

#SAVE!!!
saveRDS(final2021into2011,'local/lsoa2021_aggd_to_2011_geog_via_OA_COBdata2021_readyforfrontiercalcs.rds')








