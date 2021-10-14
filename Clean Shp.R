a <- read.csv("Raleigh_Police_Incidents_(NIBRS)_with_zipcodes.csv")

b <- unique(a$zips)

tl_2019_us_zcta510 <- 
  
zips <- readOGR("tl_2019_us_zcta510/tl_2019_us_zcta510.shp")

raleigh_zips <- zips[zips$GEOID10 %in% b, ]


saveRDS(raleigh_zips, "raleigh_zipcode_shp.rds")


