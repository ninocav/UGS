#### Read in urban atlas vector data for every city ####

berlin_ua <- read_sf("Data/Urban atlas/DE001L1_BERLIN_UA2018_v013/Data/DE001L1_BERLIN_UA2018_v013.gpkg")
bremen_ua <- read_sf("Data/Urban atlas/DE012L1_BREMEN_UA2018_v013/Data/DE012L1_BREMEN_UA2018_v013.gpkg")
cologne_ua <- read_sf("Data/Urban atlas/DE004L1_KOLN_UA2018_v013/Data/DE004L1_KOLN_UA2018_v013.gpkg")
dortmund_essen_ua <- read_sf("Data/Urban atlas/DE038L1_RUHRGEBIET_UA2018_v013/Data/DE038L1_RUHRGEBIET_UA2018_v013.gpkg")
dresden_ua <- read_sf("Data/Urban atlas/DE009L2_DRESDEN_UA2018_v013/Data/DE009L2_DRESDEN_UA2018_v013.gpkg")
düsseldorf_ua <- read_sf("Data/Urban atlas/DE011L1_DUSSELDORF_UA2018_v013/Data/DE011L1_DUSSELDORF_UA2018_v013.gpkg")

frankfurt_ua <- read_sf("Data/Urban atlas/DE005L1_FRANKFURT_AM_MAIN_UA2018_v013/Data/DE005L1_FRANKFURT_AM_MAIN_UA2018_v013.gpkg")
hamburg_ua <- read_sf("Data/Urban atlas/DE002L1_HAMBURG_UA2018_v013/Data/DE002L1_HAMBURG_UA2018_v013.gpkg")
hannover_ua <- read_sf("Data/Urban atlas/DE013L1_HANNOVER_UA2018_v013/Data/DE013L1_HANNOVER_UA2018_v013.gpkg")
leipzig_ua <- read_sf("Data/Urban atlas/DE008L2_LEIPZIG_UA2018_v013/Data/DE008L2_LEIPZIG_UA2018_v013.gpkg")
munich_ua <- read_sf("Data/Urban atlas/DE003L1_MUNCHEN_UA2018_v013/Data/DE003L1_MUNCHEN_UA2018_v013.gpkg")
nuremberg_ua <- read_sf("Data/Urban atlas/DE014L1_NURNBERG_UA2018_v013/Data/DE014L1_NURNBERG_UA2018_v013.gpkg")
stuttgart_ua <- read_sf("Data/Urban atlas/DE007L1_STUTTGART_UA2018_v013/Data/DE007L1_STUTTGART_UA2018_v013.gpkg")


city_df <- list(berlin_ua, bremen_ua, cologne_ua, dortmund_essen_ua, dresden_ua, düsseldorf_ua, dortmund_essen_ua,
                frankfurt_ua, hamburg_ua, hannover_ua, leipzig_ua, munich_ua, nuremberg_ua, stuttgart_ua)

county_names <- c("Berlin", "Bremen", "Köln", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt am Main",
                "Hamburg", "Hannover", "Leipzig (Kreisfreie Stadt)", "München (Kreisfreie Stadt)", "Nürnberg", "Stuttgart")

city_names <- c("Berlin", "Bremen", "Cologne", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt",
                "Hamburg", "Hanover", "Leipzig", "Munich", "Nuremberg", "Stuttgart")
z=1 

for(city in city_df) {
  
  temp_data <- city
  temp_data$code_2018 <- as.factor(temp_data$code_2018)
  
  temp_vector <- vect(temp_data)
  temp_raster <- rast(temp_vector, resolution(1,1))
  res(temp_raster) <- c(10,10)
  county_raster <- rasterize(temp_vector, temp_raster, "class_2018")
  
  temp_county <- germany_county %>% filter(germany_county$NAME_2 == county_names[z])
  temp_county <- st_transform(temp_county$geometry, crs=crs(county_raster))
  county_shapes[[z]] <- temp_county
  
  county_raster <- crop(county_raster, vect(temp_county))
  county_raster <- mask(county_raster, vect(temp_county))
  
  saveRDS(county_raster, (paste0("Data/Urban atlas/", city_names[z], "_raster.rds")))
  
  z= z+1
}