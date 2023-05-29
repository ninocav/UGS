### Compare the different city samples of the first and second wave ###

#### Compute Naturalness and CI's for all cities ####
city_names <- c("Bremen", "Hanover", "Cologne", "Berlin", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt",
                "Hamburg", "Leipzig", "Munich", "Nuremberg", "Stuttgart")
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
  geom_point(data = wtp_cities, aes(x = reorder(city_names, +city_estimates), y=city_estimates), col="forestgreen") +
  geom_errorbar(data = wtp_cities, aes(x= city_names, 
                    ymin=abs(city_estimates)-margin_of_error, 
                    ymax=abs(city_estimates)+margin_of_error), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  xlab("City") +
  ylab("WTP (€/m)") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/wtp_cities_ci_ordered.png", dpi = "print",  width = 7, height = 5)

# Alphabetic order Figure WTP Naturalness city first and second wave in the paper 
ggplot() +
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  geom_bar(data=wtp_cities, aes(x=city_names, y=city_estimates), stat='identity', position='dodge', fill="darkseagreen") +
  geom_errorbar(data = wtp_cities, aes(x= city_names, 
                                       ymin=abs(city_estimates)-margin_of_error, 
                                       ymax=abs(city_estimates)+margin_of_error), width=.2,position=position_dodge(.85)) +
  xlab("City") +
  ylab("WTP Naturalness (€/month)") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/wtp_cities_ci.png", dpi = "print",  width = 7, height = 5)

#### Compute same for walking distance ####

alpha = 0.05 # set confidence level 

for(city_name in city_names){
  model <- apollo_loadModel(paste0("Estimation_results/MXL_", city_name))
  city_estimates <- model$estimate[4]
  city_se <- model$se[4]
  margin_of_error <- qnorm(1-alpha/2)*city_se # margin of error  
  
  wtp_cities <- rbind(wtp_cities, 
                      data.frame(city_names=city_name, city_estimates=city_estimates, margin_of_error=margin_of_error))
}

ggplot() +
  geom_point(data = wtp_cities[15:28, ], aes(x=reorder(city_names, -city_estimates), y=city_estimates), col="red") +
  geom_errorbar(data = wtp_cities[15:28, ], aes(x= city_names, 
                                       ymin=city_estimates-margin_of_error, 
                                       ymax=city_estimates+margin_of_error), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  xlab("City") +
  ylab("WTP (€/month)") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/walking_dist_cities_ci_ordered.png", dpi = "print",  width = 7, height = 5)

ggplot() +
  geom_point(data = wtp_cities[15:28, ], aes(x=city_names, y=city_estimates), col="red") +
  geom_errorbar(data = wtp_cities[15:28, ], aes(x= city_names, 
                                       ymin=city_estimates-margin_of_error, 
                                       ymax=city_estimates+margin_of_error), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  xlab("City") +
  ylab("WTP (€/month)") +
  theme_bw() +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/walking_dist_cities_ci.png", dpi = "print",  width = 7, height = 5)


#### Examine correlation between walking distance and naturalness ####

wtp_wide <- data.frame(
  City = wtp_cities$city_names[1:14],
  WTP_Naturalness = wtp_cities$city_estimates[1:14],
  WTP_Walking_Distance = wtp_cities$city_estimates[15:28]
)

ggplot(wtp_wide, aes(x = WTP_Naturalness, y = WTP_Walking_Distance)) + 
  geom_point()+
  geom_smooth(method=lm) +
  xlab("WTP Naturalness") +
  ylab("WTP Walking Distance")

ggsave("Figures/corr_natural_distance.png", dpi = "print",  width = 7, height = 5)


#### Make model estimates table #####

city_names <- c("Bremen", "Hanover", "Cologne", "Berlin", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt",
                "Hamburg", "Leipzig", "Munich", "Nuremberg", "Stuttgart")
model_list <- list()

for(city_name in city_names){
  model_list[[city_name]] <- apollo_loadModel(paste0("Estimation_results/MXL_", city_name))
}

texreg(model_list, "Tables/model12.tex", stars = c(0.01, 0.05, 0.1))
