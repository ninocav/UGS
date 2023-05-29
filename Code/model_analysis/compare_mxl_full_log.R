### Compare the different city samples for the full sample with log rent ###


#### Compute Naturalness and CI's for all cities ####
city_names <- c("Bremen", "Hanover", "Cologne", "Berlin", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt",
                "Hamburg", "Leipzig", "Munich", "Nuremberg", "Stuttgart")
alpha = 0.05 # set confidence level 
wtp_cities_log <- data.frame(city_names=character(0), city_estimates=numeric(0), margin_of_error=numeric(0))

for(city_name in city_names){
  model <- apollo_loadModel(paste0("Estimation_results/MXL_log_rent/MXL_full_log_", city_name))
  city_estimates <- model$estimate[3]
  city_se <- model$se[3]
  margin_of_error <- qnorm(1-alpha/2)*city_se # margin of error  
  
  wtp_cities_log <- rbind(wtp_cities_log, 
                      data.frame(city_names=city_name, city_estimates=city_estimates, margin_of_error=margin_of_error))
}


## Absolute values of coefficients and paper coefficients with 95% CIs

# Alphabetic order -> Figure paper Naturalness WTP % of monthly rent 
ggplot() +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  geom_bar(data=wtp_cities_log, aes(x=city_names, y=city_estimates*100), stat='identity', position='dodge', fill="darkseagreen") +
  geom_errorbar(data = wtp_cities_log, aes(x= city_names, 
                                       ymin=(abs(city_estimates)-margin_of_error)*100, 
                                       ymax=(abs(city_estimates)+margin_of_error)*100), width=.2,position=position_dodge(.85)) +
  xlab("City") +
  ylab("WTP Naturalness (% of Rent)") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/full_wtp_cities_ci_log.png", dpi = "print",  width = 7, height = 5)

#### Compute same for walking distance ####

alpha = 0.05 # set confidence level 

for(city_name in city_names){
  model <- apollo_loadModel(paste0("Estimation_results/MXL_log_rent/MXL_full_log_", city_name))
  city_estimates <- model$estimate[4]
  city_se <- model$se[4]
  margin_of_error <- qnorm(1-alpha/2)*city_se # margin of error  
  
  wtp_cities_log <- rbind(wtp_cities_log, 
                      data.frame(city_names=city_name, city_estimates=city_estimates, margin_of_error=margin_of_error))
}


ggplot() +
  geom_point(data = wtp_cities_log[15:28, ], aes(x=city_names, y=city_estimates*100), col="red") +
  geom_errorbar(data = wtp_cities_log[15:28, ], aes(x= city_names, 
                                                ymin=(city_estimates-margin_of_error)*100, 
                                                ymax=(city_estimates+margin_of_error)*100), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  xlab("City") +
  ylab("WTP (% of Rent)") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/full_walking_dist_cities_ci_log.png", dpi = "print",  width = 7, height = 5)

#### Compute same for rent ####

alpha = 0.05 # set confidence level 

for(city_name in city_names){
  model <- apollo_loadModel(paste0("Estimation_results/MXL_log_rent/MXL_full_log_", city_name))
  city_estimates <- model$estimate[5]
  city_se <- model$se[5]
  margin_of_error <- qnorm(1-alpha/2)*city_se # margin of error  
  
  wtp_cities_log <- rbind(wtp_cities_log, 
                      data.frame(city_names=city_name, city_estimates=city_estimates, margin_of_error=margin_of_error))
}

# Figure appendix rent coefficients in log model 
ggplot() +
  geom_point(data = wtp_cities_log[29:42, ], aes(x=city_names, y=city_estimates), col="red") +
  geom_errorbar(data = wtp_cities_log[29:42, ], aes(x= city_names, 
                                                ymin=city_estimates-margin_of_error, 
                                                ymax=city_estimates+margin_of_error), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  xlab("City") +
  ylab("Rent Coefficient") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/full_rent_ci_log.png", dpi = "print",  width = 7, height = 5)


