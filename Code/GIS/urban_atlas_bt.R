#### Function for calculating shares ####

sum_cover <- function(x){
  list(x %>%
         group_by(value) %>%
         summarize(total_area = sum(coverage_area)) %>%
         mutate(proportion = total_area/sum(total_area)))
  
}

city_names_200 <- c("Aachen", "Augsburg", "Bielefeld", "Bochum", "Bonn", "Braunschweig",
                "Chemnitz", "Duisburg", "Erfurt", "Freiburg", "Gelsenkirchen", "Halle",
                "Karlsruhe", "Kassel", "Kiel", "Krefeld", "Lübeck", "Magdeburg", "Mainz",
                "Mannheim", "Mönchengladbach", "Münster", "Oberhausen", "Rostock", "Wiesbaden",
                "Wuppertal")


county_names_200 <- c("Städteregion Aachen", "Augsburg (Kreisfreie Stadt)", "Bielefeld", "Bochum", "Bonn", "Braunschweig", 
                      "Chemnitz", "Duisburg", "Erfurt", "Freiburg im Breisgau", "Gelsenkirchen", 
                      "Halle (Saale)", "Karlsruhe (Stadtkreis)", "Kassel (Kreisfreie Stadt)", 
                      "Kiel", "Krefeld", "Lübeck", "Magdeburg", "Mainz", "Mannheim", "Mönchengladbach", 
                      "Münster", "Oberhausen", "Rostock (Kreisfreie Stadt)", "Wiesbaden", "Wuppertal")

county_raster_list <- list()
county_shapes_200 <- list()
ugs_share_list <- list()
forest_share_list <- list()
sport_leisure_list <- list()
industrial_commercial_list <- list()
urban_fabric_list <- list()
water_list <- list()
natural_green_list <- list()
pastures_list <- list()
all_green_list <- list()
sealed_list <- list()

z=1

for(city in city_names_200) {
  
  county_raster <- readRDS(paste0("Data/Urban atlas/BT/", city, "_raster.rds"))
  county_raster_list[[city]] <- county_raster
  
  temp_county <- germany_county %>% filter(germany_county$NAME_2 == county_names_200[z])
  temp_county <- st_transform(temp_county$geometry, crs=crs(county_raster))
  county_shapes_200[[z]] <- temp_county
  
  forest_ua <- county_raster == "Forests"
  ugs_ua <- county_raster == "Green urban areas"
  sport_leisure_ua <- county_raster == "Sports and leisure facilities"
  industrial_commercial_ua <- county_raster == "Industrial, commercial, public, military and private units"
  urban_fabric_ua <- county_raster %in% c("Continuous urban fabric (S.L. : > 80%)", "Discontinuous dense urban fabric (S.L. : 50% -  80%)",
                                          "Discontinuous low density urban fabric (S.L. : 10% - 30%)", "Discontinuous medium density urban fabric (S.L. : 30% - 50%)",
                                          "Discontinuous very low density urban fabric (S.L. : < 10%)")
  water_ua <- county_raster == "Water"
  natural_green_ua <- county_raster == "Herbaceous vegetation associations (natural grassland, moors...)"
  pastures_ua <- county_raster == "Pastures"
  all_green_ua <- county_raster %in% c("Forests", "Green urban areas", "Herbaceous vegetation associations (natural grassland, moors...)",
                                       "Pastures", "Open spaces with little or no vegetation (beaches, dunes, bare rocks, glaciers)",
                                       "Land without current use")
  sealed_ua <- county_raster %in% c("Continuous urban fabric (S.L. : > 80%)", "Discontinuous dense urban fabric (S.L. : 50% -  80%)",
                                    "Discontinuous low density urban fabric (S.L. : 10% - 30%)", "Discontinuous medium density urban fabric (S.L. : 30% - 50%)",
                                    "Discontinuous very low density urban fabric (S.L. : < 10%)", "Industrial, commercial, public, military and private units",
                                    "Fast transit roads and associated land", "Railways and associated land", "Isolated structures", 
                                    "Other roads and associated land", "Airports", "Port areas") 
  
  forest_share_temp <- as.data.frame(
    exact_extract(forest_ua, temp_county, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover))
  forest_share_list[[city]] <- forest_share_temp
  
  ugs_share_temp <- as.data.frame(
    exact_extract(ugs_ua, temp_county, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover))
  ugs_share_list[[city]] <- ugs_share_temp
  
  sport_leisure_temp <- as.data.frame(
    exact_extract(sport_leisure_ua, temp_county, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover))
  sport_leisure_list[[city]] <- sport_leisure_temp
  
  industrial_commercial_temp <- as.data.frame(
    exact_extract(industrial_commercial_ua, temp_county, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover))
  industrial_commercial_list[[city]] <- industrial_commercial_temp
  
  urban_fabric_temp <- as.data.frame(
    exact_extract(urban_fabric_ua, temp_county, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover))
  urban_fabric_list[[city]] <- urban_fabric_temp
  
  water_temp <- as.data.frame(
    exact_extract(water_ua, temp_county, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover))
  water_list[[city]] <- water_temp
  
  natural_green_temp <- as.data.frame(
    exact_extract(natural_green_ua, temp_county, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover))
  natural_green_list[[city]] <- natural_green_temp
  
  pastures_temp <- as.data.frame(
    exact_extract(pastures_ua, temp_county, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover))
  pastures_list[[city]] <- pastures_temp
  
  all_green_temp <- as.data.frame(
    exact_extract(all_green_ua, temp_county, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover))
  all_green_list[[city]] <- all_green_temp
  
  sealed_temp <- as.data.frame(
    exact_extract(sealed_ua, temp_county, coverage_area = TRUE, summarize_df = TRUE, fun = sum_cover))
  sealed_list[[city]] <- sealed_temp
  
  z= z+1
}



### Store shares in one data frame ###
ugs_share <- sapply(ugs_share_list, function(x) x$proportion[2])
ugs_area <- sapply(ugs_share_list, function(x) x$total_area[2])
forest_share <- sapply(forest_share_list, function(x) x$proportion[2])
forest_area <- sapply(forest_share_list, function(x) x$total_area[2])
sport_leisure_share <- sapply(sport_leisure_list, function(x) x$proportion[2])
sport_leisure_area <- sapply(sport_leisure_list, function(x) x$total_area[2])
industrial_commercial_share <- sapply(industrial_commercial_list, function(x) x$proportion[2])
urban_fabric_share <- sapply(urban_fabric_list, function(x) x$proportion[2])
urban_fabric_area <- sapply(urban_fabric_list, function(x) x$total_area[2])
water_share <- sapply(water_list, function(x) x$proportion[2])
natural_green_share <- sapply(natural_green_list, function(x) x$proportion[2])
natural_green_area <- sapply(natural_green_list, function(x) x$total_area[2])
pastures_share <- sapply(pastures_list, function(x) x$proportion[2])
all_green_share <- sapply(all_green_list, function(x) x$proportion[2])
all_green_area <- sapply(all_green_list, function(x) x$total_area[2])
sealed_share <- sapply(sealed_list, function (x) x$proportion[2])
sealed_area <- sapply(sealed_list, function (x) x$total_area[2])

urban_atlas_shares <- data.frame(urban_green_share = ugs_share, urban_green_area= ugs_area, forest_share = forest_share,
                                 forest_area = forest_area, sport_leisure = sport_leisure_share, 
                                 sport_leisure_area = sport_leisure_area,
                                 industrial_commercial = industrial_commercial_share,
                                 urban_fabric = urban_fabric_share, urban_fabric_area = urban_fabric_area, water_share = water_share,
                                 natural_green_share = natural_green_share, natural_green_area = natural_green_area,
                                 pastures_share = pastures_share,
                                 all_green = all_green_share, all_green_area = all_green_area,
                                 sealed_share = sealed_share, sealed_area = sealed_area)


urban_atlas_shares <- cbind(city_names_200, urban_atlas_shares)
urban_atlas_shares <- urban_atlas_shares %>% rename("City" = city_names_200)

names(county_shapes_200) <- city_names_200

#### Transform income data for 200 cities ####

income_cities200 <- income_cities %>% filter(City %in% city_names_200 | 
                                              City %in% c("Halle (Saale)", "Freiburg im Breisgau",
                                                          "Städteregion Aachen (einschl. Stadt Aachen)")) %>% 
  mutate(Dis_Income_ca = as.numeric(Dis_Income_ca)) %>% filter(!is.na(Dis_Income_ca)) %>% 
  filter(Type != "Landkreis" & Type != "Stat. Region" & Type != "Regierungsbezirk") %>% 
  mutate(City = case_when(City =="Städteregion Aachen (einschl. Stadt Aachen)" ~ "Aachen",
                          City =="Freiburg im Breisgau" ~ "Freiburg",
                          City == "Halle (Saale)" ~ "Halle", TRUE~ City)) %>% 
  mutate(Income = Dis_Income_ca /12)

urban_atlas_shares <- left_join(urban_atlas_shares, income_cities200, by="City")
urban_atlas_shares <- urban_atlas_shares %>% mutate(NaturalnessUGS = 0)



ggsave("Figures/shares_bt_cities.png", dpi="print", width=7, height =5)
### mean centering quick

benefit_cities <- urban_atlas_shares %>% mutate(all_green_mean = as.numeric(scale(all_green, scale=FALSE)*100),
                                water_share_mean = as.numeric(scale(water_share, scale=FALSE)*100),
                                sport_leisure_mean = as.numeric(scale(sport_leisure, scale=FALSE)*100),
                                Income_mean = as.numeric(scale(Income, scale=FALSE))) 




#### Combine data sets ####

full_city_set <- bind_rows(all_city, urban_atlas_shares)

full_city_set <- full_city_set %>% mutate(all_green_mean = as.numeric(scale(all_green, scale=FALSE)*100),
                                                water_share_mean = as.numeric(scale(water_share, scale=FALSE)*100),
                                                sport_leisure_mean = as.numeric(scale(sport_leisure, scale=FALSE)*100),
                                                Income_mean = as.numeric(scale(Income, scale=FALSE)),
                                          Naturalness_mean = 0) %>% mutate(Income_destatis_mean = as.numeric(Income_mean)) 

full_city_set$WTP_pred <- predict(model_nat, full_city_set)

#full_city_set$WTP_pred_nat <- predict(model_nat, full_city_set)

full_city_set$WTP_income <- full_city_set$Income * 0.026 * 0.4

### Merge county shapes to data frame ###
county_shapes_200_df <- as.data.frame(county_shapes_200)
colnames(county_shapes_200_df) <- city_names_200
county_shapes_200_df <- county_shapes_200_df %>% 
  mutate(row = row_number()) %>% 
  pivot_longer(-row, names_to = "City", values_to = "Shape") %>% select(-row)

names(county_shapes) <- city_names
county_shapes_df <- as.data.frame(county_shapes)
colnames(county_shapes_df) <- city_names
county_shapes_df <- county_shapes_df %>% 
  mutate(row = row_number()) %>% 
  pivot_longer(-row, names_to = "City", values_to = "Shape") %>% select(-row)

merge_shapes <- rbind(county_shapes_df, county_shapes_200_df)

full_city_set <- left_join(full_city_set, merge_shapes, by="City")

# BT estimates for appendix 
ggplot() +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  geom_bar(data=full_city_set, aes(x=City, y=WTP_pred), stat='identity', position='dodge', fill="darkseagreen") +
  xlab("City") +
  ylab("WTP Naturalness (€/month)") +
  labs(title = "") +
  scale_color_manual(values = colors) 
  #theme(axis.text.x = element_text(face = rep(c("bold", "plain"), 20)))

ggsave("Figures/benefit_all_wtp.png", dpi="print", width = 7, height =5)


### Plot all cities on a map of germany
germany_geo <- st_transform(germany_sf, crs = crs(county_raster))
plot(germany_geo$geometry)


##### Do maps for the paper #####
ggplot(data=germany_geo) + 
  geom_sf(fill="gray98", alpha=1) +
  geom_sf(data=full_city_set, aes(fill=WTP_pred, geometry=Shape), size=2, col="black") +
  annotation_scale(location = "bl", width_hint = 0.1,plot_unit="m", line_width = 0.1) +
  annotation_north_arrow(location = "br" ,pad_x = unit(0, "in"), pad_y = unit(0, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(fill= "WTP (€/month)") +
  scale_fill_gradientn(colours = c("cornsilk1", "darkgreen"))
  #scale_fill_viridis(option="B")

ggsave("Figures/map_con.png", dpi="print", width=7, height=5)

### Discontinuous map used in the paper 
quantile(full_city_set$WTP_pred)
full_city_set$cuts <- cut(full_city_set$WTP_pred, breaks = c(14, 18, 20, 23, 31))


ggplot(data=germany_geo) + 
  geom_sf(fill="gray98") +
  geom_sf(data=full_city_set, aes(fill=cuts, geometry=Shape), size=2, col="black") +
  annotation_scale(location = "bl", width_hint = 0.12,plot_unit="m", line_width = 0.5) +
  annotation_north_arrow(location = "br" ,pad_x = unit(0, "in"), pad_y = unit(0, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(fill= "WTP (€/month)") +
  scale_fill_manual(values = c("orange", "gold1", "chartreuse3", "forestgreen"),
                    labels = c("14 - 18", "18 - 20", "20 - 23", "23 - 31"))

ggsave("Figures/map_disc.png", dpi="print", width=7, height=5)


#### Urban atlas shares for all cities ####

ggplot(data=full_city_set) +
  geom_point(aes(x= City, y=all_green), col="darkgreen", shape=17, size=2) +
  geom_point(aes(x= City, y=water_share), col="blue", shape=19, size=2) +
  geom_point(aes(x= City, y=sport_leisure), col="orange", shape=15, size=2) +
  geom_point(aes(x= City, y=sealed_share), col="red", shape=18, size=2) +
  scale_x_discrete(guide = guide_axis(angle = 45)) 

ggsave("Figures/ua_200.png", width = 7, height = 5)
