### Compare the different city samples for the full sample ###
# Check conditional logit models log(rent) #

#### Compute Naturalness and CI's for all cities ####
city_names <- c("Bremen", "Hanover", "Cologne", "Berlin", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt",
                "Hamburg", "Leipzig", "Munich", "Nuremberg", "Stuttgart")
alpha = 0.05 # set confidence level 
wtp_cities_clogit <- data.frame(city_names=character(0), city_estimates=numeric(0), margin_of_error=numeric(0))

for(city_name in city_names){
  model <- apollo_loadModel(paste0("Estimation_results/c_logit/log_rent/Clogit_full_log_", city_name))
  city_estimates <- model$estimate[3]
  city_se <- model$se[3]
  margin_of_error <- qnorm(1-alpha/2)*city_se # margin of error  
  
  wtp_cities_clogit <- rbind(wtp_cities_clogit, 
                      data.frame(city_names=city_name, city_estimates=city_estimates, margin_of_error=margin_of_error))
}


## Absolute values of coefficients and paper coefficients with 95% CIs


# Alphabetic order 
ggplot() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  geom_bar(data=wtp_cities_clogit, aes(x=city_names, y=city_estimates), stat='identity', position='dodge', fill="darkseagreen") +
  geom_errorbar(data = wtp_cities_clogit, aes(x= city_names, 
                                       ymin=abs(city_estimates)-margin_of_error, 
                                       ymax=abs(city_estimates)+margin_of_error), width=.2,position=position_dodge(.85)) +
  xlab("City") +
  ylab("WTP Naturalness (€/month)") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/clogit/clogit_wtp_cities_ci_log.png", dpi = "print",  width = 7, height = 5)

#### Compute same for walking distance ####

alpha = 0.05 # set confidence level 

for(city_name in city_names){
  model <- apollo_loadModel(paste0("Estimation_results/c_logit/log_rent/Clogit_full_log_", city_name))
  city_estimates <- model$estimate[4]
  city_se <- model$se[4]
  margin_of_error <- qnorm(1-alpha/2)*city_se # margin of error  
  
  wtp_cities_clogit <- rbind(wtp_cities_clogit, 
                      data.frame(city_names=city_name, city_estimates=city_estimates, margin_of_error=margin_of_error))
}

ggplot() +
  geom_point(data = wtp_cities_clogit[15:28, ], aes(x=reorder(city_names, -city_estimates), y=city_estimates), col="red") +
  geom_errorbar(data = wtp_cities_clogit[15:28, ], aes(x= city_names, 
                                                ymin=city_estimates-margin_of_error, 
                                                ymax=city_estimates+margin_of_error), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  xlab("City") +
  ylab("WTP (€/month)") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/clogit/clogit_walking_dist_cities_ci_ordered_log.png", dpi = "print",  width = 7, height = 5)

ggplot() +
  geom_point(data = wtp_cities_clogit[15:28, ], aes(x=city_names, y=city_estimates), col="red") +
  geom_errorbar(data = wtp_cities_clogit[15:28, ], aes(x= city_names, 
                                                ymin=city_estimates-margin_of_error, 
                                                ymax=city_estimates+margin_of_error), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  xlab("City") +
  ylab("WTP (€/month)") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/clogit/clogit__walking_dist_cities_ci_log.png", dpi = "print",  width = 7, height = 5)

#### Compute same for rent ####

alpha = 0.05 # set confidence level 

for(city_name in city_names){
  model <- apollo_loadModel(paste0("Estimation_results/c_logit/log_rent/Clogit_full_log_", city_name))
  city_estimates <- model$estimate[5]
  city_se <- model$se[5]
  margin_of_error <- qnorm(1-alpha/2)*city_se # margin of error  
  
  wtp_cities_clogit <- rbind(wtp_cities_clogit, 
                      data.frame(city_names=city_name, city_estimates=city_estimates, margin_of_error=margin_of_error))
}

ggplot() +
  geom_point(data = wtp_cities_clogit[29:42, ], aes(x=reorder(city_names, -city_estimates), y=city_estimates), col="red") +
  geom_errorbar(data = wtp_cities_clogit[29:42, ], aes(x= city_names, 
                                                ymin=city_estimates-margin_of_error, 
                                                ymax=city_estimates+margin_of_error), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  xlab("City") +
  ylab("WTP (€/month)") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/clogit/clogit_rent_ci_ordered_log.png", dpi = "print",  width = 7, height = 5)

ggplot() +
  geom_point(data = wtp_cities_clogit[29:42, ], aes(x=city_names, y=city_estimates), col="red") +
  geom_errorbar(data = wtp_cities_clogit[29:42, ], aes(x= city_names, 
                                                ymin=city_estimates-margin_of_error, 
                                                ymax=city_estimates+margin_of_error), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  xlab("City") +
  ylab("WTP (€/month)") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/clogit/clogit_rent_ci_log.png", dpi = "print",  width = 7, height = 5)


#### Examine correlation between walking distance and naturalness

wtp_wide_clogit <- data.frame(
  City = wtp_cities_clogit$city_names[1:14],
  WTP_Naturalness = wtp_cities_clogit$city_estimates[1:14],
  WTP_Walking_Distance = wtp_cities_clogit$city_estimates[15:28],
  WTP_Rent = wtp_cities_clogit$city_estimates[29:42]
)

ggplot(data=wtp_wide_clogit, aes(x = WTP_Naturalness, y = WTP_Walking_Distance)) + 
  geom_point()+
  geom_smooth(method=lm) +
  geom_text(aes(label=City), vjust=-0.5, hjust=0.5, size=3.0) +
  xlab("WTP Naturalness") +
  ylab("WTP Walking Distance")

ggsave("Figures/clogit/clogit_corr_natural_distance_log.png", dpi = "print",  width = 7, height = 5)

ggplot(data=wtp_wide_clogit, aes(x = WTP_Naturalness, y = WTP_Rent)) + 
  geom_point()+
  geom_smooth(method=lm) +
  geom_text(aes(label=City), vjust=-0.5, hjust=0.5, size=3.0) +
  xlab("WTP Naturalness") +
  ylab("Rent Coefficient")

ggsave("Figures/clogit/clogit_corr_natural_rent_log.png", dpi = "print",  width = 7, height = 5)
