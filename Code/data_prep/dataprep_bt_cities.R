#### Filter pop data for cities over 200.000

pop_data_200 <- pop_data %>% filter(Total >= 200000) %>% mutate(City = str_extract(City, "^[^,]+"))

pop_data_200only <- pop_data_200 %>% filter(Total < 500000)

cities200 <- pop_data_200only$City

### Get city borders ###
county200 <- germany_county %>% filter(NAME_2 %in% cities200 | 
                                       NAME_2 %in% c("Städteregion Aachen", "Karlsruhe (Stadtkreis)",
                                                       "Kassel (Kreisfreie Stadt)", "Augsburg (Kreisfreie Stadt)",
                                                       "Rostock (Kreisfreie Stadt)")) %>% 
  filter(TYPE_2 != "Landkreis")

plot(county200$geometry)

#### Rasterize ua polygons (only once needed!) #####
# Default is that the preprocessed data is read in 

#source("Code/GIS/rasterize_ua_bt_cities.R")