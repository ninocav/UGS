### Compare the different city samples ###

#### Compute Naturalness and CI's for all cities ####
city_names <- c("Bremen", "Hanover", "Cologne", "Berlin", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt",
                "Hamburg", "Leipzig", "Munich", "Nuremberg", "Stuttgart")
alpha = 0.05 # set confidence level 
wtp_cities_3 <- data.frame(city_names=character(0), city_estimates=numeric(0), margin_of_error=numeric(0))

for(city_name in city_names){
  model <- apollo_loadModel(paste0("Estimation_results/MXL_3_", city_name))
  city_estimates <- model$estimate[3]
  city_se <- model$se[3]
  margin_of_error <- qnorm(1-alpha/2)*city_se # margin of error  
  
  wtp_cities_3 <- rbind(wtp_cities_3, 
                      data.frame(city_names=city_name, city_estimates=city_estimates, margin_of_error=margin_of_error))
}

### values from other wave ###
#### Compute Naturalness and CI's for all cities ####

alpha = 0.05 # set confidence level 
wtp_cities <- data.frame(city_names=character(0), city_estimates=numeric(0), margin_of_error=numeric(0))

for(city_name in city_names){
  model <- apollo_loadModel(paste0("Estimation_results/MXL_", city_name))
  city_estimates <- model$estimate[3]
  city_se <- model$se[3]
  margin_of_error <- qnorm(1-alpha/2)*city_se # margin of error  
  
  wtp_cities <- rbind(wtp_cities, 
                      data.frame(city_names=city_name, city_estimates=city_estimates, margin_of_error=margin_of_error))
}

## Absolute values of coefficients and paper coefficients with 95% CIs

# Order  by size
ggplot() +
  geom_point(data = wtp_cities_3, aes(x = reorder(city_names, +city_estimates), y=city_estimates), col="forestgreen") +
  geom_errorbar(data = wtp_cities_3, aes(x= city_names, 
                                       ymin=abs(city_estimates)-margin_of_error, 
                                       ymax=abs(city_estimates)+margin_of_error), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  xlab("City") +
  ylab("WTP (€/m)") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/wtp_cities_ci_ordered_3.png", dpi = "print",  width = 7, height = 5)

# Alphabetic order 
ggplot() +
  geom_errorbar(data = wtp_cities_3, aes(x= city_names, 
                                       ymin=abs(city_estimates)-margin_of_error, 
                                       ymax=abs(city_estimates)+margin_of_error), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  geom_bar(data=wtp_cities_3, aes(x=city_names, y=city_estimates), stat='identity', position='dodge', fill="red2", alpha=0.3) +
  xlab("City") +
  ylab("WTP (€/month)") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/wtp_cities_ci_3.png", dpi = "print",  width = 7, height = 5)



wtp_cities_melted <- rbind(wtp_cities, wtp_cities_3)
wtp_cities_melted$variable <- "12"
wtp_cities_melted$variable[15:28] <- "3"

# Compare WTP between the cities for the two samples Figure in the paper #
ggplot(data=wtp_cities_melted[1:28, ], aes(x=city_names, y=abs(city_estimates), fill=variable)) +
  geom_bar(stat="identity",  position='dodge', width = 0.8) +
  geom_errorbar(aes(x=city_names, ymin=abs(city_estimates)-margin_of_error, ymax=abs(city_estimates)+margin_of_error), width=0.3, position=position_dodge(0.8)) +
  ylab("WTP Naturalness (€/month)") +
  xlab("City") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_brewer(palette = "Set2", labels = c("Respondi", "IMUG"), name="Sample") +
  theme(legend.position = c(0.9, 0.85))

ggsave("Figures/wtp_cities_ci_compare.png", dpi = "print",  width = 7, height = 5)

#### Compute same for walking distance ####

alpha = 0.05 # set confidence level 

for(city_name in city_names){
  model <- apollo_loadModel(paste0("Estimation_results/MXL_3_", city_name))
  city_estimates <- model$estimate[4]
  city_se <- model$se[4]
  margin_of_error <- qnorm(1-alpha/2)*city_se # margin of error  
  
  wtp_cities_3 <- rbind(wtp_cities_3, 
                      data.frame(city_names=city_name, city_estimates=city_estimates, margin_of_error=margin_of_error))
}

ggplot() +
  geom_point(data = wtp_cities_3[15:28, ], aes(x=reorder(city_names, -city_estimates), y=city_estimates), col="red") +
  geom_errorbar(data = wtp_cities_3[15:28, ], aes(x= city_names, 
                                                ymin=city_estimates-margin_of_error, 
                                                ymax=city_estimates+margin_of_error), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  xlab("City") +
  ylab("WTP (€/month)") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/walking_dist_cities_ci_ordered_3.png", dpi = "print",  width = 7, height = 5)

ggplot() +
  geom_point(data = wtp_cities_3[15:28, ], aes(x=city_names, y=city_estimates), col="red") +
  geom_errorbar(data = wtp_cities_3[15:28, ], aes(x= city_names, 
                                                ymin=city_estimates-margin_of_error, 
                                                ymax=city_estimates+margin_of_error), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  xlab("City") +
  ylab("WTP (€/month)") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/walking_dist_cities_ci_3.png", dpi = "print",  width = 7, height = 5)


#### Examine correlation between walking distance and naturalness

wtp_wide <- data.frame(
  City = wtp_cities_3$city_names[1:14],
  WTP_Naturalness = wtp_cities_3$city_estimates[1:14],
  WTP_Walking_Distance = wtp_cities_3$city_estimates[15:28]
)

ggplot(wtp_wide, aes(x = WTP_Naturalness, y = WTP_Walking_Distance)) + 
  geom_point()+
  geom_smooth(method=lm) +
  xlab("WTP Naturalness") +
  ylab("WTP Walking Distance")

ggsave("Figures/corr_natural_distance_3.png", dpi = "print",  width = 7, height = 5)


#### Load WTP models for waves #####

mxl_12 <- apollo_loadModel("Estimation_results/MXL_12")
mxl_3 <- apollo_loadModel("Estimation_results/MXL_3")


mxl_compare <- as.data.frame(mxl_12$estimate)
mxl_compare[2] <- as.data.frame(mxl_3$estimate)

alpha = 0.05
mxl_compare$margin_of_error12 <- qnorm(1-alpha/2)*mxl_12$robse
mxl_compare$margin_of_error3 <- qnorm(1-alpha/2)*mxl_3$robse
mxl_compare <- rownames_to_column(mxl_compare, "Coefficent")
colnames(mxl_compare) <- c("Coefficent", "Estimate12", "Estimate3", "Margin_of_error12", "Margin_of_error3")
mxl_compare <- mxl_compare %>% mutate(Deviation = abs(Estimate3) - abs(Estimate12), Deviation_percent = Deviation/Estimate12)

ggplot() +
  geom_point(data = mxl_compare, aes(x = Coefficent, y = Deviation), col= "red", shape=4, size =2.5) +
  geom_errorbar(data = mxl_compare, aes(x= Coefficent, ymin=0-Margin_of_error12, ymax=0+Margin_of_error12), 
                width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  xlab("Coefficent") +
  ylab("Deviation") +
  theme_bw() +
  scale_color_manual(values = colors)

ggsave("Figures/zero_centered_deviations.png", dpi = "print",  width = 7, height = 5)

ggplot() +
  geom_bar(data = mxl_compare, aes(x = Coefficent, y = abs(Deviation_percent)), stat="identity", alpha=0.8) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  xlab("Coefficent") +
  ylab("Deviation (%)") +
  theme_bw() 

ggsave("Figures/percent_deviations.png", dpi = "print",  width = 7, height = 5)

mxl_melt <- melt(mxl_compare[1:3], id = "Coefficent")
mxl_melt$ME <- mxl_compare$Margin_of_error12
mxl_melt$ME[11:20] <- mxl_compare$Margin_of_error3

# Figure paper Respondi vs. IMUG pooled models
ggplot(data=mxl_melt, aes(x=Coefficent, y=abs(value), fill=variable)) +
  geom_bar(stat="identity",  position='dodge', width = 0.9) +
  geom_errorbar(aes(x=Coefficent, ymin=abs(value)-ME, ymax=abs(value)+ME), width=0.3, position=position_dodge(0.8)) +
  ylab("Absolute Value") +
  xlab("Coefficient") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_brewer(palette = "Set2", labels = c("Respondi", "IMUG"), name="Sample") +
  theme(legend.position = c(0.9, 0.85))

ggsave("Figures/compare_full_mxls.png", dpi = "print",  width = 7, height = 5)


#### Compare socio means for outliers Bremen, Dresden, Munich ####

city_out <- c("Bremen", "Dresden", "Munich")


cityls_out12 <- map(set_names(city_out), ~ filter(database_all, City_id == .x))
cityls_out3 <- map(set_names(city_out), ~ filter(database_third, City == .x))


city_out_m12 <- map(cityls_out12, ~summarize_at(.x, vars(Age, KidsNo, gender_female, garden, education,
                                                   Income_Dis_Present, Miete_3, FlatSize), 
                                          function(x) round(mean(x, na.rm = TRUE), 3)))
city_out_m12 <- bind_rows(city_out_m12)


city_out_m3 <- map(cityls_out3, ~summarize_at(.x, vars(Age, KidsNo, gender_female, garden, education,
                                                         Income_Dis_Present, Miete_3, FlatSize), 
                                                function(x) round(mean(x, na.rm = TRUE), 3)))
city_out_m3 <- bind_rows(city_out_m3)


# Compute tests for Munich #
test_munich <- database_grouped %>% filter(City == "Munich")

anova_test(Age~ Sample, data = test_munich ) 
anova_test(FlatSize~ Sample, data = test_munich)
anova_test(Kaltmiete~ Sample, data = test_munich)
anova_test(KidsNo~ Sample, data = test_munich)
kruskal.test(Income_Dis_Present ~ Sample, data=test_munich)


chisq.test(test_munich$gender_female, test_munich$Sample)
chisq.test(test_munich$education, test_munich$Sample)
