
# Data and methods

We use the proportion of Foreign-born residents in a Lower Super Output Area (LSOA) to calculate frontiers. There is a frontier if two neighbouring LSOAs have drastically different proportions of foreign-born residents (after statistically adjusting for error and small sample sizes). Our information comes from the 2011 census for England and Wales. 

For calculating segregation, we use the dissimilarity index (DI) which goes from 0 to 1. A value of 0 indicates no segregation (i.e. all LSOAs have the same proportion of foreign-born) whilst 1 indicates perfect segregation (i.e LSOAs either have 0% or 100% foreign-born with no mixing in between). 

For measuring frontier density, we first count how many pairs of LSOAs are bordering each other. For example, LSOA 1 and LSOA 2 might make one border pair and LSOA 1 and LSOA 3 might make another border pair.  A subset of these border pairs are social frontiers. Frontier density is simply the number of frontier border pairs divided by the total number of border pairs in a region. This goes from 0 (no frontiers) to 1 (100% of borders are frontiers). 

Our method for calculating frontiers is outlined in [Dean et al 2019](https://doi.org/10.1111/tesg.12316) and available as an R package on [GitHub](https://github.com/life-at-the-frontier/socialFrontiers). The code used to create the frontiers in this dashboard is also available on [GitHub](https://github.com/life-at-the-frontier/detect-uk-frontiers).  
