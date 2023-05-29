###### read in shape file for Germany with county borders from : https://gadm.org/download_country.html ####
germany_sf <- st_as_sf(read_sf("data/gadm41_DEU_shp", "gadm41_DEU_0")) # no county borders 
germany_county <- read_sf("data/gadm41_DEU_shp", "gadm41_DEU_2") # Germany with county borders
germany_gemeinde <- read_sf("data/gadm41_DEU_shp", "gadm41_DEU_3") # For Gemeinde hannover, get gemeinde borders

hannover_shape <- germany_gemeinde %>% filter(NAME_3 == "Hannover") %>% dplyr::select(-"GID_3", -"VARNAME_3", -"NL_NAME_3") %>% 
  mutate(NAME_2 = NAME_3) 

colnames(hannover_shape) <- colnames(germany_county) # Incorporate Hanover shape 

germany_county <- rbind(germany_county, hannover_shape)


#### Cut Neuwerk part for Hamburg and Bremerhaven for Bremen ####

#Hamburg

hamburg_temp <- germany_county %>% filter(germany_county$NAME_2 == "Hamburg")
#remove everything below 9 xmin
bbox_ha <- st_bbox(c(xmin = 9, ymin = 53.39499, xmax = 10.32596, ymax = 53.9632), crs = st_crs(hamburg_temp))
hamburg_temp <- st_crop(hamburg_temp, bbox_ha)
germany_county$NAME_2
germany_county[163, ]$geometry <- hamburg_temp$geometry
hamburg_temp <- germany_county %>% filter(germany_county$NAME_2 == "Hamburg") # check if everything worked
rm(hamburg_temp, bbox_ha)

# Bremen

bremen_temp <- germany_county %>% filter(germany_county$NAME_2 == "Bremen")
#remove everything above 53.45 ymax
bbox_bremen <- st_bbox(c(xmin = 8.481735, ymin = 53.01102, xmax = 8.990781, ymax = 53.45), crs = st_crs(bremen_temp))
bremen_temp <- st_crop(bremen_temp, bbox_bremen)
germany_county$NAME_2
germany_county[161, ]$geometry <- bremen_temp$geometry
bremen_temp <- germany_county %>% filter(NAME_2 == "Bremen")
rm(bremen_temp, bbox_bremen)
