# Get locations of the respondents so far (maybe delete)
locations <- database_all %>% group_by(Id) %>% 
  dplyr::select(Latitude, Longitude, City) %>% 
  summarize(Lat = mean(Latitude), Lon = mean(Longitude), City = unique(City))

dir.create("Figures/City_plots")

##### get coordinates for all respondents via open street map #####
library(tmaptools)
library(ggmap)

  
##### Extract addressees of respondents ######
coordinates_geo <- database_merged %>%  group_by(id) %>% 
  dplyr::select(Latitude, Longitude, City, Street, Number, Sample) %>% 
  summarize(Lat = mean(Latitude), Lon = mean(Longitude), City = unique(City), Street = unique(Street),
            Number = unique(Number), Sample =unique(Sample))

coordinates_geo$Address <- paste(coordinates_geo$Street, coordinates_geo$Number, coordinates_geo$City, sep = ", ")

# Geocode the addresses, takes ages, only do this one time and load in the data

# geocoded500 <- geocode_OSM(coordinates_geo[1:500, ]$Address)
# geocoded1000 <- geocode_OSM(coordinates_geo[501:1000, ]$Address)
# geocoded1500 <- geocode_OSM(coordinates_geo[1001:1500, ]$Address)
# geocoded2000a <- geocode_OSM(coordinates_geo[1501:2000, ]$Address) 
# geocoded2500 <- geocode_OSM(coordinates_geo[2001:2500, ]$Address)
# geocoded3000 <- geocode_OSM(coordinates_geo[2501:3000, ]$Address)
# geocoded3500 <- geocode_OSM(coordinates_geo[3001:3500, ]$Address)
# geocoded4000 <- geocode_OSM(coordinates_geo[3501:4000, ]$Address)
# geocoded4500 <- geocode_OSM(coordinates_geo[4001:4500, ]$Address)
# geocoded5000 <- geocode_OSM(coordinates_geo[4501:5000, ]$Address)
# geocoded5500 <- geocode_OSM(coordinates_geo[5001:5500, ]$Address)
# geocoded6000 <- geocode_OSM(coordinates_geo[5501:5812, ]$Address) # problem with höherhofstraße in Düsseldorf, skip
# geocoded6500 <- geocode_OSM(coordinates_geo[5814:nrow(coordinates_geo), ]$Address)
# 
# geo_df <- rbind(geocoded500, geocoded1000, geocoded1500, geocoded2000a, geocoded2500, geocoded3000, geocoded3500,
#       geocoded4000, geocoded4500, geocoded5000, geocoded5500, geocoded6000, geocoded6500)
# 
# saveRDS(geo_df, "Data/coordinates.rds")

### read in data with preprocessed coordinates
geo_df <- readRDS("Data/coordinates.rds") # contains till outliers which are "identified" below and removed for analysis
colnames(geo_df) <- c("Address", "Lat", "Lon")
geo_df <- geo_df[ , 1:3]

coordinates_merged <- left_join(coordinates_geo, geo_df, by="Address") 
coordinates_merged <- distinct(coordinates_merged, id, .keep_all = TRUE) #clean this up
coordinates_merged <- coordinates_merged %>%  dplyr::filter(Lat.y > 20)

ggplot(data=germany_sf) +
  geom_sf() +
  geom_point(data=coordinates_merged, aes(x=Lon.y, y=Lat.y))

county_shapes_trans <- county_shapes
names(county_shapes_trans) <- county_names

z=1
for (city in city_names) {
temp_county <- germany_county %>% filter(germany_county$NAME_2 == county_names[z])
temp_county <- st_transform(temp_county$geometry, crs=crs(germany_sf))
county_shapes_trans[[z]] <- temp_county
z= z+1
}


cities <- c("Berlin", "Bremen", "Cologne", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt", 
            "Hamburg", "Hanover", "Leipzig", "Munich", "Nuremberg", "Stuttgart")
names(county_shapes_trans) <- cities

citylist_geo <- map(set_names(cities), ~ filter(coordinates_merged, City == .x))

ggplot() +
  geom_sf(data=county_shapes_trans["Leipzig"]$Leipzig) +
  geom_point(data=citylist_geo$Leipzig, aes(x=Lon.y, y=Lat.y)) 


##### identify respondents that live outside the city borders #####

cities <- c("Berlin", "Bremen", "Cologne", "Dortmund", "Dresden", "Düsseldorf", 
            "Essen", "Frankfurt", "Hamburg", "Hanover", "Leipzig", "Munich", "Nuremberg", "Stuttgart")

citylist_geo_cleaned <- list()

for (city in cities) {

object <- citylist_geo[[city]]  
shape <- county_shapes_trans[[city]]
coords_sf <- st_as_sf(object, coords = c("Lon.y", "Lat.y"), crs = st_crs(4326))
polygon_sf <- st_as_sf(shape)

within_polygon <- st_intersection(coords_sf, polygon_sf)
coordinates <- st_coordinates(within_polygon)
within_polygon$Lon.y <- coordinates[ , 1]
within_polygon$Lat.y <- coordinates[ , 2]
citylist_geo_cleaned[[city]] <- as.data.frame(within_polygon)
}


outliers <- lapply(citylist_geo, function(df) {
  df[!df$id %in% unlist(lapply(citylist_geo_cleaned, `[[`, "id")), ]
})

outliers <- do.call(rbind, outliers)
citylist_geo <- citylist_geo_cleaned



##### Check clustering/distribution of respondents for the cities #####

# Set up list of cities
cities <- c("Berlin", "Bremen", "Cologne", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt", 
            "Hamburg", "Hanover", "Leipzig", "Munich", "Nuremberg", "Stuttgart")

# Loop over cities and perform clustering analysis
for (city in cities) {
  # Get location data for current city
  location_data <- citylist_geo[[city]]
  locations <- st_as_sf(x = location_data, coords = c("Lon.y", "Lat.y"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  locations_repro <- st_transform(locations, crs =  3857)
  locations_PPP <- as.ppp(locations_repro)
  
  # Get window data for current city
  window_data <- county_shapes_trans[[city]]
  window_repro <- st_transform(window_data, crs =  3857)
  window <- as.owin(window_repro)
  locations_PPP$window <- window
  
  # Compute clustering analysis for current city
  # K_locations <- Kest(locations_PPP, correction = "Ripley")
  # plot(K_locations, main = paste0("Test of degree of clustering at different levels in ", city))
  
  density_filename <- paste0("Figures/city_plots/", tolower(city), "_density.png")
  png(file = density_filename, width = 648, height = 496)
  plot(density.ppp(locations_PPP, sigma = 300))
  dev.off()
}


### compute stamen distribution maps for appendix###
stamen_list <- list()

for (city in cities) {
  current_city <- citylist_geo[[city]]
  current_shape <- county_shapes_trans[[city]]
  bbox <- make_bbox(lon=current_city$Lon.y, lat=current_city$Lat.y, f = 0.25)
  map <- get_stamenmap(bbox=bbox, maptype = "terrain-background")
  
  stamen_list[[city]] <- ggmap(map)+
    geom_sf(data=current_shape, inherit.aes = FALSE, fill="transparent") +
    geom_point(data=current_city, aes(x=Lon.y, y=Lat.y), shape=16, size=0.8)
  
  ggsave(paste0("Figures/city_plots/", tolower(city), "_stamen.png"), dpi="print", width=7, height = 5)
}

ggarrange(plotlist = stamen_list, ncol=5, nrow=3, labels = cities)
ggsave("Figures/City_plots/stamen_all.png", dpi=200, width = 24, height = 13.5)

##### Check for spatial differences between the samples for city with significant differences #####
ggplot() +
  geom_sf(data = county_shapes_trans["Munich"]$Munich) +
  geom_point(data = subset(citylist_geo$Munich, Sample == "IMUG"), aes(x = Lon.y, y = Lat.y)) +
  geom_point(data = subset(citylist_geo$Munich, Sample == "Respondi"), aes(x = Lon.y, y = Lat.y), col="red")

check_clustering <- c("Bremen", "Dresden", "Munich")
rip_k_ls <- list()

for (city in check_clustering) {
  # Get location data for current city
  location_data <- citylist_geo[[city]]
  locations <- st_as_sf(x = location_data, coords = c("Lon.y", "Lat.y"),
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  locations_repro <- st_transform(locations, crs =  3857)
  locations_respondi <- locations_repro %>% dplyr::filter(Sample == "Respondi")
  locations_imug <- locations_repro %>% dplyr::filter(Sample == "IMUG")
  locations_PPP_respondi <- as.ppp(locations_respondi)
  locations_PPP_imug <- as.ppp(locations_imug)
  
  # Get window data for current city
  window_data <- county_shapes_trans[[city]]
  window_repro <- st_transform(window_data, crs =  3857)
  window <- as.owin(window_repro)
  locations_PPP_respondi$window <- window
  locations_PPP_imug$window <- window
  
  
  K_locations_respondi <- Kest(locations_PPP_respondi, correction = "Ripley")
  K_locations_imug <- Kest(locations_PPP_imug, correction = "Ripley")
  K_boot_respondi <- lohboot(locations_PPP_respondi, fun="Kest", nsim=1000)
  K_boot_imug <- lohboot(locations_PPP_imug, fun="Kest", nsim=1000)
  
  rip_k_ls[[city]] <- ggplot(data=K_locations_respondi) +
    geom_line(aes(x=r/1000, y=iso, col="A: Respondi"), col="#66c2a5") +
    geom_line(data=K_locations_imug, aes(x=r/1000, y=iso, col="B: IMUG"), col="#fc8d62") +
    geom_ribbon(data=K_boot_respondi, aes(x=r/1000, ymin = lo, ymax = hi), fill="#66c2a5", alpha = 0.2) +
    geom_ribbon(data=K_boot_imug, aes(x=r/1000, ymin = lo, ymax = hi), fill="#fc8d62", alpha = 0.2) +
    xlab("Distance (km)") +
    ylab("K(r)")
  ggsave(paste0("Figures/city_plots/", tolower(city), "_clustering.png"), dpi="print", width=7, height = 5)
  
}

# Plot Ripley's K-functions for outlier cities
ggarrange(plotlist =rip_k_ls, labels = c("Bremen", "Dresden", "Munich"), label.x = 0.2)
ggsave("Figures/City_plots/clust3.jpeg", width=7, height = 5, dpi = "print")
  