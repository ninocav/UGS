# Read in Urban atlas vector data 
aachen_ua <- read_sf("Data/Urban atlas/DE507L1_AACHEN_UA2018_v013/Data/DE507L1_AACHEN_UA2018_v013.gpkg")
augsburg_ua <- read_sf("Data/Urban atlas/DE033L1_AUGSBURG_UA2018_v013/Data/DE033L1_AUGSBURG_UA2018_v013.gpkg")
bielefeld_ua <- read_sf("Data/Urban atlas/DE017L0_BIELEFELD_UA2018_v013/Data/DE017L0_BIELEFELD_UA2018_v013.gpkg")
bochum_ua <- read_sf("Data/Urban atlas/DE038L1_RUHRGEBIET_UA2018_v013/Data/DE038L1_RUHRGEBIET_UA2018_v013.gpkg")
bonn_ua <- read_sf("Data/Urban atlas/DE034L1_BONN_UA2018_v013/Data/DE034L1_BONN_UA2018_v013.gpkg")
braunschweig_ua <- read_sf("Data/Urban atlas/DE083L1_BRAUNSCHWEIG_SALZGITTER_WOLFSBURG_UA2018_v013/Data/DE083L1_BRAUNSCHWEIG_SALZGITTER_WOLFSBURG_UA2018_v013.gpkg")
chemnitz_ua <- read_sf("Data/Urban atlas/DE505L0_CHEMNITZ_UA2018_v013/Data/DE505L0_CHEMNITZ_UA2018_v013.gpkg")
duisburg_ua <- read_sf("Data/Urban atlas/DE038L1_RUHRGEBIET_UA2018_v013/Data/DE038L1_RUHRGEBIET_UA2018_v013.gpkg")
erfurt_ua <- read_sf("Data/Urban atlas/DE032L1_ERFURT_UA2018_v013/Data/DE032L1_ERFURT_UA2018_v013.gpkg")
freiburg_ua <- read_sf("Data/Urban atlas/DE027L1_FREIBURG_IM_BREISGAU_UA2018_v013/Data/DE027L1_FREIBURG_IM_BREISGAU_UA2018_v013.gpkg")
gelsenkirchen_ua <- read_sf("Data/Urban atlas/DE038L1_RUHRGEBIET_UA2018_v013/Data/DE038L1_RUHRGEBIET_UA2018_v013.gpkg")
halle_ua <- read_sf("Data/Urban atlas/DE018L1_HALLE_AN_DER_SAALE_UA2018_v013/Data/DE018L1_HALLE_AN_DER_SAALE_UA2018_v013.gpkg")
karlsruhe_ua <- read_sf("Data/Urban atlas/DE035L1_KARLSRUHE_UA2018_v013/Data/DE035L1_KARLSRUHE_UA2018_v013.gpkg")
kassel_ua <- read_sf("Data/Urban atlas/DE513L1_KASSEL_UA2018_v013/Data/DE513L1_KASSEL_UA2018_v013.gpkg")
kiel_ua <- read_sf("Data/Urban atlas/DE039L1_KIEL_UA2018_v013/Data/DE039L1_KIEL_UA2018_v013.gpkg")
krefeld_ua <- read_sf("Data/Urban atlas/DE508L0_KREFELD_UA2018_v013/Data/DE508L0_KREFELD_UA2018_v013.gpkg")
lübeck_ua <- read_sf("Data/Urban atlas/DE510L1_LUBECK_UA2018_v013/Data/DE510L1_LUBECK_UA2018_v013.gpkg")
magdeburg_ua <- read_sf("Data/Urban atlas/DE019L2_MAGDEBURG_UA2018_v013/Data/DE019L2_MAGDEBURG_UA2018_v013.gpkg")
mainz_ua <- read_sf("Data/Urban atlas/DE037L1_MAINZ_UA2018_v013/Data/DE037L1_MAINZ_UA2018_v013.gpkg")
mannheim_ua <- read_sf("Data/Urban atlas/DE084L1_MANNHEIM_LUDWIGSHAFEN_UA2018_v013/Data/DE084L1_MANNHEIM_LUDWIGSHAFEN_UA2018_v013.gpkg")
mönchengladbach_ua <- read_sf("Data/Urban atlas/DE036L0_MONCHENGLADBACH_UA2018_v013/Data/DE036L0_MONCHENGLADBACH_UA2018_v013.gpkg")
münster_ua <- read_sf("Data/Urban atlas/DE504L1_MUNSTER_UA2018_v013/Data/DE504L1_MUNSTER_UA2018_v013.gpkg")
oberhausen_ua <- read_sf("Data/Urban atlas/DE038L1_RUHRGEBIET_UA2018_v013/Data/DE038L1_RUHRGEBIET_UA2018_v013.gpkg")
rostock_ua <- read_sf("Data/Urban atlas/DE043L2_ROSTOCK_UA2018_v013/Data/DE043L2_ROSTOCK_UA2018_v013.gpkg")
wiesbaden_ua <- read_sf("Data/Urban atlas/DE020L1_WIESBADEN_UA2018_v013/Data/DE020L1_WIESBADEN_UA2018_v013.gpkg")
wuppertal_ua <- read_sf("Data/Urban atlas/DE546L0_WUPPERTAL_UA2018_v013/Data/DE546L0_WUPPERTAL_UA2018_v013.gpkg")

# Create list of dataframes containing ua data 
city_df <- list(aachen_ua, augsburg_ua, bielefeld_ua, bochum_ua, bonn_ua, braunschweig_ua,
            chemnitz_ua, duisburg_ua, erfurt_ua, freiburg_ua, gelsenkirchen_ua, halle_ua,
            karlsruhe_ua, kassel_ua, kiel_ua, krefeld_ua, lübeck_ua, magdeburg_ua, mainz_ua,
            mannheim_ua, mönchengladbach_ua, münster_ua, oberhausen_ua, rostock_ua, wiesbaden_ua,
            wuppertal_ua)

city_names <- c("Aachen", "Augsburg", "Bielefeld", "Bochum", "Bonn", "Braunschweig",
                "Chemnitz", "Duisburg", "Erfurt", "Freiburg", "Gelsenkirchen", "Halle",
                "Karlsruhe", "Kassel", "Kiel", "Krefeld", "Lübeck", "Magdeburg", "Mainz",
                "Mannheim", "Mönchengladbach", "Münster", "Oberhausen", "Rostock", "Wiesbaden",
                "Wuppertal")


county_names_200 <- c("Städteregion Aachen", "Augsburg (Kreisfreie Stadt)", "Bielefeld", "Bochum", "Bonn", "Braunschweig", 
                   "Chemnitz", "Duisburg", "Erfurt", "Freiburg im Breisgau", "Gelsenkirchen", 
                   "Halle (Saale)", "Karlsruhe (Stadtkreis)", "Kassel (Kreisfreie Stadt)", 
                   "Kiel", "Krefeld", "Lübeck", "Magdeburg", "Mainz", "Mannheim", "Mönchengladbach", 
                   "Münster", "Oberhausen", "Rostock (Kreisfreie Stadt)", "Wiesbaden", "Wuppertal")
z=1 
dir.create("Data/Urban atlas/BT")

for(city in city_df) {
  
  temp_data <- city
  temp_data$code_2018 <- as.factor(temp_data$code_2018)
  
  temp_vector <- vect(temp_data)
  temp_raster <- rast(temp_vector, resolution(1,1))
  res(temp_raster) <- c(10,10)
  county_raster <- rasterize(temp_vector, temp_raster, "class_2018")
  
  temp_county <- germany_county %>% filter(germany_county$NAME_2 == county_names_200[z])
  temp_county <- st_transform(temp_county$geometry, crs=crs(county_raster))
  county_shapes[[z]] <- temp_county
  
  county_raster <- crop(county_raster, vect(temp_county))
  county_raster <- mask(county_raster, vect(temp_county))
  
  saveRDS(county_raster, (paste0("Data/Urban atlas/BT/", city_names[z], "_raster.rds")))
  
  z= z+1
}
