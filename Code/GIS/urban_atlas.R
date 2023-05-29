#### Function for calculating shares ####

sum_cover <- function(x){
  list(x %>%
         group_by(value) %>%
         summarize(total_area = sum(coverage_area)) %>%
         mutate(proportion = total_area/sum(total_area)))
  
}

# Set list for county names and city names 
county_names <- c("Berlin", "Bremen", "Köln", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt am Main",
                  "Hamburg", "Hannover", "Leipzig (Kreisfreie Stadt)", "München (Kreisfreie Stadt)", "Nürnberg", "Stuttgart")

city_names <- c("Berlin", "Bremen", "Cologne", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt",
                "Hamburg", "Hanover", "Leipzig", "Munich", "Nuremberg", "Stuttgart")

county_raster_list <- list()
county_shapes <- list()
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

for(city in city_names) {

county_raster <- readRDS(paste0("Data/Urban atlas/", city, "_raster.rds"))
county_raster_list[[city]] <- county_raster

temp_county <- germany_county %>% filter(germany_county$NAME_2 == county_names[z])
temp_county <- st_transform(temp_county$geometry, crs=crs(county_raster))
county_shapes[[z]] <- temp_county

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


City <- c("Berlin", "Bremen", "Cologne", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt",
                "Hamburg", "Hanover", "Leipzig", "Munich", "Nuremberg", "Stuttgart")

urban_atlas_shares <- cbind(City, urban_atlas_shares)


# Join data #

all_city <- left_join(urban_atlas_shares, wtp_wide)
all_city <- left_join(all_city, city_means)
all_city <- left_join(all_city, pop_data_500)
all_city <- all_city %>% mutate(ugs_capita = all_green_area/Total, ugs_ratio = ugs_area/all_green_area, forest_capita = forest_area/Total,
                                sport_capita = sport_leisure_area/Total, forest_ratio = forest_area/all_green_area,
                                natural_green_capita = natural_green_area/Total, urban_fabric_capita = urban_fabric_area/Total,
                                sealed_capita = sealed_area/Total, aggregated_WTP = Total*WTP_Naturalness, sealed_ugs = ugs_area/sealed_area,
                                WTP_Rent_median = -exp(WTP_Rent))


#### One plot for Leipzig with colors for paper appendix ####
leipzig_raster_gg <- as.data.frame(county_raster_list[["Leipzig"]], xy=TRUE)

naturalness_map <- c("Forests" = 1,
                     "Green urban areas" = 2,
                     "Herbaceous vegetation associations (natural grassland, moors...)" = 3,
                     "Pastures" = 4,
                     "Open spaces with little or no vegetation (beaches, dunes, bare rocks, glaciers)" = 5,
                     "Sports and leisure facilities" = 6,
                     "Arable land (annual crops)" = 7,
                     "Construction sites" = 8,
                     "Mineral extraction and dump sites" = 9,
                     "Isolated structures" = 10,
                     "Fast transit roads and associated land" = 11,
                     "Railways and associated land" = 12,
                     "Other roads and associated land" = 13,
                     "Discontinuous very low density urban fabric (S.L. : < 10%)" = 14,
                     "Discontinuous low density urban fabric (S.L. : 10% - 30%)" = 15,
                     "Discontinuous medium density urban fabric (S.L. : 30% - 50%)" = 16,
                     "Discontinuous dense urban fabric (S.L. : 50% -  80%)" = 17,
                     "Continuous urban fabric (S.L. : > 80%)" = 18,
                     "Industrial, commercial, public, military and private units" = 19,
                     "Water" = 20,
                     "Wetlands" = 21,
                     "Airports" = 22,
                     "Land without current use" = 23)

# Create a new variable with naturalness levels
leipzig_raster_gg$naturalness <- naturalness_map[as.character(leipzig_raster_gg$class_2018)]
leipzig_raster_gg$naturalness <- as.factor(leipzig_raster_gg$naturalness)

col_plot <- c(terrain.colors(30))
col_plot2 <- c(terrain.colors(12, alpha= 0.8))
col_grey <- gray.colors(6, start =0.7, end = 0.2)
col_red <- brewer.pal(8, "OrRd")
col_red <- add.alpha(col_red,alpha=0.8)
col_brown <- brewer.pal(8,"BrBG")
col_brown <- add.alpha(col_brown, alpha=0.8)

## Plot example for Leipzig for the appendix ##
ggplot(data=leipzig_raster_gg) +
  geom_raster(aes(x = x, y = y, fill = naturalness))+
  scale_fill_manual(values = c(col_plot2[1:6], "khaki1", col_brown[1:3], col_grey[1:3], col_red[3:7], "grey45",
                               "skyblue", "dodgerblue", "orangered", "grey90"),
                               labels=names(naturalness_map), name="Classification") + 
  coord_quickmap() +
  theme(legend.text = element_text(size=12),legend.title = element_text(size=12), legend.key.size = unit(0.8, 'cm'), legend.spacing = unit(0.2, "cm")) +
  guides(fill=guide_legend(nrow=23,byrow=F)) +
  annotation_scale(location = "br", width_hint = 0.1,plot_unit="m", line_width = 1) +
  annotation_north_arrow(location = "br" ,pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  xlab("") + ylab("")

ggsave("Figures/leipzig_raster.png", dpi="print", width=16, height=9)
ggsave("Figures/leipzig_raster.svg", dpi="print", width=16, height=9)

#### Spatial correlation plots ####

ggplot(data=all_city) +
  geom_bar(aes(x=City, y=all_green), stat="identity", fill="chartreuse4", alpha=0.8) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  xlab("City") +
  ylab("Green Space Share") +
  theme_bw() +
  labs(title = "")

ggsave("Figures/green_space_share.png", dpi = "print",  width = 7, height = 5)

ggplot(all_city, aes(x = WTP_Naturalness, y = all_green)) + 
  geom_point()+
  geom_smooth(method=lm, formula= y ~ x, col="chartreuse4") +
  geom_text(label=City, vjust=-0.5, hjust=0.5, size=3.0) +
  xlab("WTP Naturalness (€/month)") +
  ylab("UGS Share") 

ggsave("Figures/ugs_share_corr.png", dpi = "print",  width = 7, height = 5)

ggplot(all_city, aes(x = WTP_Naturalness, y = ugs_capita)) + 
  geom_point()+
  geom_smooth(method=lm, formula= y ~ x, col="forestgreen") +
  geom_text(label=City, vjust=-0.5, hjust=0.5, size=3.0) +
  xlab("WTP Naturalness (€/month)") +
  ylab(expression("UGS per Capita"~(m^2)))

ggsave("Figures/ugs_per_capita.png", dpi = "print",  width = 7, height = 5)

forest_capita_pl <- ggplot(all_city, aes(x = WTP_Naturalness, y = forest_capita)) + 
  geom_point()+
  geom_smooth(aes(x = WTP_Naturalness, y = forest_capita), method = lm, formula= y ~ x, col="forestgreen") +
  geom_text(label=City, vjust=-0.5, hjust=0.5, size=3.0) +
  xlab("WTP Naturalness (€/month)") +
  ylab(expression("Forest Area Per Capita"~(m^2))) 

ggsave("Figures/forest_per_capita.png", dpi = "print",  width = 7, height = 5)

ggplot(all_city, aes(x = WTP_Naturalness, y = forest_share)) + 
  geom_point()+
  geom_smooth(aes(x = WTP_Naturalness, y = forest_share), method = lm, formula= y ~ x, col="forestgreen") +
  geom_text(label=City, vjust=-0.5, hjust=0.5, size=3.0) +
  xlab("WTP Naturalness (€/month)") +
  ylab("Forest Share") 

ggsave("Figures/forest_share_corr.png", dpi = "print",  width = 7, height = 5)

ggplot(all_city, aes(x = WTP_Naturalness, y = forest_ratio)) + 
  geom_point()+
  geom_smooth(aes(x = WTP_Naturalness, y = forest_ratio), method = lm, formula= y ~ x, col="forestgreen") +
  geom_text(label=City, vjust=-0.5, hjust=0.5, size=3.0) +
  xlab("WTP Naturalness (€/month)") +
  ylab("Forest Share of UGS") 

ggsave("Figures/forest_ugs_share.png", dpi = "print",  width = 7, height = 5)


ggplot(all_city, aes(x = WTP_Naturalness, y = Park)) +
  geom_point()+
  geom_smooth(aes(x = WTP_Naturalness, y = Park), method = lm, col="forestgreen") +
  geom_text(label=City, vjust=-0.5, hjust=0.5, size=3.0) +
  xlab("WTP Naturalness (€/month)") +
  ylab("Park as favourite UGS")

ggsave("Figures/park_favourite.png", dpi = "print",  width = 7, height = 5)

## Check correlation population density WTP_Naturalness 
ggplot(all_city, aes(x = WTP_Naturalness, y = Density)) + 
  geom_point()+
  geom_smooth(method = lm, col="red2") +
  geom_text(label=City, vjust=-0.5, hjust=0.5, size=3.0) +
  ylab(expression("Population Density (people per"~km^2~")")) +
  xlab("WTP Naturalness (€/month)") 

ggsave("Figures/pop_density.png", dpi="print", width =7, height = 5)

ggplot(all_city, aes(x = Density, y = ugs_capita)) + 
  geom_point()+
  geom_smooth(method = lm, col="red2") +
  geom_text(label=City, vjust=-0.5, hjust=0.5, size=3.0) +
  ylab("UGS per Capita") +
  xlab("Population Density") 

ggsave("Figures/pop_density_ugs_corr.png", dpi="print", width =7, height = 5)



urban_fabric <- ggplot(all_city, aes(x = WTP_Naturalness, y = urban_fabric)) + 
  geom_point()+
  geom_smooth(method=lm, col="red") +
  geom_text(label=City, vjust=-0.5, hjust=0.5, size=3.0) +
  xlab("WTP Naturalness (€/month)") +
  ylab("Urban Fabric Share")

ggsave("Figures/urban_fabric.png", dpi="print", width=7, height=5)

sealed <- ggplot(all_city, aes(x = WTP_Naturalness, y = sealed_share)) + 
  geom_point()+
  geom_smooth(method=lm, col="red") +
  xlab("WTP Naturalness (€/month)") +
  ylab("Sealed Area Share")

ggsave("Figures/sealed.png", dpi="print", width=7, height=5)

ggarrange(forest_capita_pl, sealed)
ggsave("Figures/double_forest_urban.png", dpi="print", width=10, height = 7)

forest <- ggplot(all_city, aes(x = WTP_Naturalness, y = forest_share)) + 
  geom_point()+
  geom_smooth(method=lm, col="forestgreen") +
  xlab("WTP Naturalness") +
  ylab("Forest Share")

i_c <- ggplot(all_city, aes(x = WTP_Naturalness, y = industrial_commercial)) + 
  geom_point()+
  geom_smooth(method=lm, col="magenta") +
  xlab("WTP Naturalness") +
  ylab("Industrial & Commercial Share")

water <- ggplot(all_city, aes(x = WTP_Naturalness, y = water_share)) + 
  geom_point()+
  geom_smooth(method=lm) +
  xlab("WTP Naturalness") +
  ylab("Water Bodies Share")

sport <- ggplot(all_city, aes(x = WTP_Naturalness, y = sport_leisure)) + 
  geom_point()+
  geom_smooth(method=lm, col="orange") +
  xlab("WTP Naturalness") +
  ylab("Sport & Leisure Share")


ggarrange(sealed, forest, sport, water, nrow =2, ncol = 2, align ="hv")
ggsave("Figures/shares_correlations.png", dpi = "print",  width = 7, height = 5)

ggplot(all_city, aes(x = urban_green_share, y = forest_share)) + 
  geom_point()+
  geom_smooth(method=lm) +
  xlab("UGS Share") +
  ylab("Forest Share")

#### Sociodemographic correlation plots ####

age <- ggplot(all_city, aes(x = WTP_Naturalness, y = Age)) + 
  geom_point()+
  geom_smooth(method=lm, col="red") +
  xlab("WTP Naturalness") +
  ylab("Age")

kids <- ggplot(all_city, aes(x = WTP_Naturalness, y = KidsDummy)) + 
  geom_point()+
  geom_smooth(method=lm, col="yellow") +
  xlab("WTP Naturalness") +
  ylab("Share of Parents")

garden <- ggplot(all_city, aes(x = WTP_Naturalness, y = garden)) +
  geom_point()+
  geom_smooth(method=lm, col="green") +
  xlab("WTP Naturalness") +
  ylab("Garden")

income <- ggplot(all_city, aes(x = WTP_Naturalness, y = Income_Dis_Present)) + 
  geom_point()+
  geom_smooth(method=lm, col="black") +
  xlab("WTP Naturalness") +
  ylab("Disposable Income")

gender <- ggplot(all_city, aes(x = WTP_Naturalness, y = gender_female)) + 
  geom_point()+
  geom_smooth(method=lm, col="blue") +
  xlab("WTP Naturalness") +
  ylab("Share of Females")

education <- ggplot(all_city, aes(x = WTP_Naturalness, y = education)) +
  geom_point()+
  geom_smooth(method=lm, col="magenta") +
  xlab("WTP Naturalness") +
  ylab("Share of Higher Education")

rent <- ggplot(all_city, aes(x = WTP_Naturalness, y = Kaltmiete)) +
  geom_point()+
  geom_smooth(method=lm, col="cyan") +
  xlab("WTP Naturalness") +
  ylab("Mean Monthly Rent")


ggarrange(age, kids, income, gender, education, rent, nrow =2, ncol = 3, align ="hv")
ggsave("Figures/socio_demo_correlations.png", dpi = "print",  width = 7, height = 5)

#### Test correlation, regressions #####
ggplot(all_city, aes(x = WTP_Naturalness, y = NaturalnessUGS)) + 
  geom_point() +
  geom_text(label=City, vjust=-0.5, hjust=0.5, size=3.0) +
  geom_smooth(method=lm, col="red") +
  xlab("WTP Naturalness (€/month)") +
  ylab("Mean Naturalness UGS")

ggsave("Figures/av_natural_ugs.png", dpi="print", width=7, height=5)

ggplot(all_city, aes(x = WTP_Naturalness, y = SatUGS-1)) + 
  geom_point() +
  geom_text(label=City, vjust=-0.5, hjust=0.5, size=3.0) +
  xlab("WTP Naturalness (€/month)") +
  ylab("Mean Satisfaction with UGS")

ggsave("Figures/satUGS.png", dpi="print", width=7, height=5)

ggplot(all_city, aes(x = WTP_Naturalness, y = WalkingDistance)) + 
  geom_point() +
  geom_text(label=City, vjust=-0.5, hjust=0.5, size=3.0) +
  geom_smooth(method=lm, col="blue") +
  xlab("WTP Naturalness (€/month)") +
  ylab("Mean Walking Distance (minutes)")

ggsave("Figures/nat_wd.png", dpi="print", width=7, height=5)



#### Check correlation walking distance and WTP WD ####

ggplot(all_city, aes(x = WTP_Walking_Distance, y = WalkingDistance)) + 
  geom_point() +
  geom_text(label=City, vjust=-0.5, hjust=0.5, size=3.0) +
  geom_smooth(method=lm, col="red") +
  xlab("WTP Walking Distance") +
  ylab("Average Walking Distance")

##### Check Aggregation Area and WTP as in Bronmann for appendix ######

ggplot(all_city, aes(x = aggregated_WTP, y = all_green_area)) + 
  geom_point() +
  geom_text(label=City, vjust=-0.5, hjust=0.5, size=3, check_overlap = TRUE) +
  geom_smooth(method=lm, col="blue") +
  xlab("Aggregated WTP (€)") +
  ylab(expression("Aggregated Area ("~m^2~")"))

ggsave("Figures/aggregation.png", dpi="print", width=7, height=5)

