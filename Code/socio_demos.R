#### Compute socio-demographics for every city #####
cities <- c("Berlin", "Bremen", "Cologne", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt", 
            "Hamburg", "Hanover", "Leipzig", "Munich", "Nuremberg", "Stuttgart")

database_merged <- database_merged %>% arrange(id)
database_merged <- database_merged %>% mutate(TypeUGSfactor = case_when(TypeUGS == 1 ~ "Park", TypeUGS == 2 ~ "Forest",
                                                                        TypeUGS == 3 ~ "Meadow", TypeUGS == 4 ~ "Littoral zone lake",
                                                                        TypeUGS == 5 ~ "Littoral zone river", TypeUGS==6 ~ "Garden",
                                                                        TypeUGS == 7 ~ "Allotment", TypeUGS == 8 ~ "Fallow land",
                                                                        TypeUGS == 9 ~ "Playground", TypeUGS == 10 ~ "Backyard",
                                                                        TypeUGS == 11 ~ "Graveyard", TRUE ~ NA_character_))
citylist <- map(set_names(cities), ~ filter(database_merged, City == .x))
names(citylist)

city_means <- map(citylist, ~summarize_at(.x, vars(Age, KidsNo, gender_female, NaturalnessUGS, garden, SatUGS, SatUGSgeneral, education,
                                                   Park, Income_Dis_Present, WalkingDistance, KidsDummy, Kaltmiete, Miete_3, FlatSize), 
                                                function(x) round(mean(x, na.rm = TRUE), 2)))

#maybe include other variables here 

city_means <- bind_rows(city_means)

# Count observations per city 
observations <- list("numeric", 14)
z <- 1
for (i in citylist) {
  observations[z] <- n_distinct(i$id)
  z <- z + 1
}

city_means$observations <- observations


# Add the city column to the dataframe
city_name_new <- c("Berlin","Bremen", "Cologne", "Dortmund","Dresden", "Düsseldorf", "Essen",
                   "Frankfurt", "Hamburg", "Hanover", "Leipzig", "Munich", "Nuremberg", "Stuttgart")
city_means$City <- city_name_new

city_means <- city_means[,c(ncol(city_means), 1:(ncol(city_means) - 1))]

city_means_mod <- city_means %>% select(-SatUGS, -SatUGSgeneral, -Park, -WalkingDistance, -Kaltmiete, -KidsDummy,
                                    -NaturalnessUGS)

# Create table with mean city values for the appendix #
print(xtable(city_means_mod, type ="latex", digits = c(0, 0, 1, 2, 2, 2, 2, 0, 0, 0, 0)), 
      include.rownames = F, file ="Tables/city_means.tex")

#### Check type of most visited greenspace ####
ugs_type_list <- list()
i= 1
for (df in citylist) {
  z <-ggplot(data= df[!is.na(df$TypeUGSfactor),], aes(x=TypeUGSfactor)) +
    geom_bar(aes(y = (..count..)/sum(..count..), fill=TypeUGSfactor), show.legend = F) +
    xlab("Type of UGS") +
    ylab("Share") +
    coord_cartesian(ylim = c(0, 0.6)) +
    scale_x_discrete(guide = guide_axis(angle = 45))
    
  
  ugs_type_list[[i]] <- z
  i= i+1
}

### Make figure for munich and hannover ###
ggarrange(ugs_type_list[[12]], ugs_type_list[[10]],
          labels = c("Munich", "Hanover"), label.x=0.3, font.label = list(size=10, face="plain", col="black"), 
          nrow=2, ncol=1, align = "h")

ggsave("Figures/ugs_type.png", dpi="print", width=7, height=5)

#### Compute boxplots ####

ggplot(data=database_merged) +
  geom_boxplot(aes(x=City, group=City, y=SizeUGS/10000), outlier.shape = NA, fill="red2", alpha=0.8) +
  coord_cartesian(ylim = c(0, 10)) +
  ylab("Size UGS (ha)") +
  xlab("City") +
  scale_x_discrete(guide = guide_axis(angle = 45))

ggsave("Figures/size_ugs_bxpl.png", dpi="print", width = 7, height=5)

ggplot(data=database_merged) +
  geom_boxplot(aes(x=City, group=City, y=Age), outlier.shape = NA, fill="skyblue2", alpha=0.8) +
  ylab("Age") +
  xlab("City") +
  scale_x_discrete(guide = guide_axis(angle = 45))  

ggsave("Figures/age_bxpl.png", dpi="print", width = 7, height=5)

ggplot(data=database_merged) +
  geom_boxplot(aes(x=City, group=City, y=Income_Dis_Present), outlier.shape = NA, fill="magenta2", alpha=0.8) +
  ylab("Disposable Income") +
  xlab("City") +
  scale_x_discrete(guide = guide_axis(angle = 45))

ggsave("Figures/income_bxpl.png", dpi="print", width = 7, height=5)

ggplot(data=database_merged) +
  geom_boxplot(aes(x=City, group=City, y=FlatSize), outlier.shape = NA, fill="gold3", alpha=0.8) +
  ylab("Flat size (m2)") +
  xlab("City") +
  coord_cartesian(ylim = c(0, 130)) +
  scale_x_discrete(guide = guide_axis(angle = 45))  

ggsave("Figures/flatsize_bxpl.png", dpi="print", width = 7, height=5)

ggplot(data=database_merged) +
  geom_boxplot(aes(x=City, group=City, y=MietePerSqm), outlier.shape = NA, fill="gray62", alpha=0.8) +
  ylab("Kaltmiete (€/m2)") +
  xlab("City") +
  coord_cartesian(ylim = c(0, 25)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) 

ggsave("Figures/rent_bxpl.png", dpi="print", width = 7, height=5)

ggplot(data=database_merged) +
  geom_boxplot(aes(x=City, group=City, y=WalkingDistance), outlier.shape = NA, fill="brown3", alpha=0.8) +
  ylab("Walking Distance") +
  xlab("City") +
  coord_cartesian(ylim = c(0, 32)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) 

ggsave("Figures/walking_bxpl.png", dpi="print", width = 7, height=5)

##### Compute socio-demographics for whole sample ####

# Select columns of interest
socios <- c("Age", "gender_female", "education", "Income_Dis_Present", "KidsNo", "NaturalnessUGS", "WalkingDistance")

# Use map to calculate summary statistics for each column
socio_demos <- database_merged %>%
  dplyr::select(socios) %>% 
  map_df(~ tibble(Mean = round(mean(.x, na.rm = TRUE), 2), Median = median(.x, na.rm = TRUE),
                  SD = round(sd(.x, na.rm=T), 2), Min = min(.x, na.rm=T), Max = max(.x, na.rm = T))) %>% 
  mutate(Variable = socios) 
  

#### Export as table ###

socio_demos <- socio_demos[,c(ncol(socio_demos), 1:(ncol(socio_demos) - 1))] # change column order 

print(xtable(socio_demos, type ="latex"), 
      include.rownames = F, file ="Tables/socio_demos.tex")

##### Compute statistical tests table in the paper #####

# one-way ANOVA test for continuous variables 
anova_test(Age~ City, data = database_grouped) 
anova_test(FlatSize~ City, data = database_grouped)
anova_test(Kaltmiete~ City, data = database_grouped)
anova_test(KidsNo~ City, data = database_grouped)
anova_test(LifeSatisfaction~ City, data = database_grouped)

# Kruskal test for ordinal variables
kruskal.test(LifeSatisfaction ~ City, data=database_grouped)
kruskal.test(Income_Dis_Present ~ City, data=database_grouped)

# Chi-squared test for dummy-coded variables 
chisq.test(database_grouped$gender_female, database_grouped$City)
chisq.test(database_grouped$education, database_grouped$City)
chisq.test(database_grouped$Garden, database_grouped$City)




##### Tests for UGS variables ####
chisq.test(database_grouped$Park, database_grouped$City)
kruskal.test(SatUGS ~ City, data=database_grouped)
kruskal.test(NaturalnessUGS ~ City, data=database_grouped)

#### Check WTP values for log model ####

wtp_cities_log_croped <- wtp_cities_log[1:14, ]
wtp_cities_log_croped$City <- wtp_cities_log_croped$city_names
wtp_cities_log_compare <- city_means %>% select(City, Miete_3) %>% left_join(wtp_cities_log_croped, by="City")
wtp_cities_log_compare <- wtp_cities_log_compare %>% mutate(WTP = city_estimates * Miete_3)

log_vs_ws <- wtp_cities_log_compare %>% select(City, WTP) %>% left_join(wtp_wide) %>% 
  select(City, WTP, WTP_Naturalness)

log_vs_ws <- melt(log_vs_ws)

# Compare log and wtp space (baseline) model for paper appendix
ggplot(data=log_vs_ws, aes(x=City, y=value, fill=variable)) +
  geom_bar(stat="identity",  position='dodge', width = 0.8) +
  ylab("WTP Naturalness (€/month)") +
  xlab("City") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values = c("grey40", "darkseagreen"), labels = c("Log Model", "WTP Space"), name="Specification") +
  theme(legend.position = "bottom") 

ggsave("Figures/WTP_log.png", dpi="print", width = 7, height = 5)

# Check WTP log with CI's
ggplot() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  geom_bar(data=wtp_cities_log_compare, aes(x=City, y=WTP), stat='identity', position='dodge', fill="darkseagreen") +
  geom_errorbar(data = wtp_cities_log_compare, aes(x= City, 
                                              ymin=(abs(city_estimates)-margin_of_error)*Miete_3, 
                                              ymax=(abs(city_estimates)+margin_of_error)*Miete_3), width=.2,position=position_dodge(.85)) +
  xlab("City") +
  ylab("WTP Naturalness (€/month)") +
  ylim(c(0,40)) +
  labs(title = "") +
  scale_color_manual(values = colors)

