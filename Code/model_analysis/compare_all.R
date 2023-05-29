### Compare the different city samples for the full sample ###
# Create figures for the paper #

#### Compute Naturalness and CI's for all cities ####
city_names <- c("Bremen", "Hanover", "Cologne", "Berlin", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt",
                "Hamburg", "Leipzig", "Munich", "Nuremberg", "Stuttgart")
alpha = 0.05 # set confidence level 
wtp_cities <- data.frame(city_names=character(0), city_estimates=numeric(0), margin_of_error=numeric(0))

for(city_name in city_names){
  model <- apollo_loadModel(paste0("Estimation_results/MXL_full_", city_name))
  city_estimates <- model$estimate[3]
  city_se <- model$se[3]
  margin_of_error <- qnorm(1-alpha/2)*city_se # margin of error  
  
  wtp_cities <- rbind(wtp_cities, 
                      data.frame(city_names=city_name, city_estimates=city_estimates, margin_of_error=margin_of_error))
}


## Absolute values of coefficients and paper coefficients with 95% CIs

# Order by size
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

ggsave("Figures/full_wtp_cities_ci_ordered.png", dpi = "print",  width = 7, height = 5)

# Alphabetic order -> Figure Naturalness WTP every city full sample 
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

ggsave("Figures/full_wtp_cities_ci.png", dpi = "print",  width = 7, height = 5)

#### Compute same for walking distance ####

alpha = 0.05 # set confidence level 

for(city_name in city_names){
  model <- apollo_loadModel(paste0("Estimation_results/MXL_full_", city_name))
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
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  xlab("City") +
  ylab("WTP (€/month)") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/full_walking_dist_cities_ci_ordered.png", dpi = "print",  width = 7, height = 5)

# Figure walking distance WTP in the appendix 
ggplot() +
  geom_point(data = wtp_cities[15:28, ], aes(x=city_names, y=city_estimates), col="red") +
  geom_errorbar(data = wtp_cities[15:28, ], aes(x= city_names, 
                                                ymin=city_estimates-margin_of_error, 
                                                ymax=city_estimates+margin_of_error), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  xlab("City") +
  ylab("WTP (€/month)") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/full_walking_dist_cities_ci.png", dpi = "print",  width = 7, height = 5)

#### Compute same for rent ####

alpha = 0.05 # set confidence level 

for(city_name in city_names){
  model <- apollo_loadModel(paste0("Estimation_results/MXL_full_", city_name))
  city_estimates <- model$estimate[5]
  city_se <- model$se[5]
  margin_of_error <- qnorm(1-alpha/2)*city_se # margin of error  
  
  wtp_cities <- rbind(wtp_cities, 
                      data.frame(city_names=city_name, city_estimates=city_estimates, margin_of_error=margin_of_error))
}

ggplot() +
  geom_point(data = wtp_cities[29:42, ], aes(x=reorder(city_names, -city_estimates), y=city_estimates), col="red") +
  geom_errorbar(data = wtp_cities[29:42, ], aes(x= city_names, 
                                                ymin=city_estimates-margin_of_error, 
                                                ymax=city_estimates+margin_of_error), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  xlab("City") +
  ylab("Rent Coefficient") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/full_rent_ci_ordered.png", dpi = "print",  width = 7, height = 5)

# Figure Rent coefficient mxl model appendix in the paper 
ggplot() +
  geom_point(data = wtp_cities[29:42, ], aes(x=city_names, y=-exp(city_estimates)), col="red") +
  geom_errorbar(data = wtp_cities[29:42, ], aes(x= city_names, 
                                                ymin=-exp(city_estimates-margin_of_error), 
                                                ymax=-exp(city_estimates+margin_of_error)), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  xlab("City") +
  ylab("Rent Coefficient") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/full_rent_ci.png", dpi = "print",  width = 7, height = 5)


#### Examine correlation between walking distance and naturalness

wtp_wide <- data.frame(
  City = wtp_cities$city_names[1:14],
  WTP_Naturalness = wtp_cities$city_estimates[1:14],
  WTP_Walking_Distance = wtp_cities$city_estimates[15:28],
  WTP_Rent = wtp_cities$city_estimates[29:42]
)

# Figure in the appendix correlatino Naturalness and Walking Distance 
ggplot(data=wtp_wide, aes(x = WTP_Naturalness, y = WTP_Walking_Distance)) + 
  geom_point()+
  geom_smooth(method=lm) +
  geom_text(aes(label=City), vjust=-0.5, hjust=0.5, size=3.0) +
  xlab("WTP Naturalness") +
  ylab("WTP Walking Distance")

ggsave("Figures/full_corr_natural_distance.png", dpi = "print",  width = 7, height = 5)

### Rent and WTP Naturalness 
ggplot(data=wtp_wide, aes(x = WTP_Naturalness, y = -exp(WTP_Rent))) + 
  geom_point()+
  geom_smooth(method=lm) +
  geom_text(aes(label=City), vjust=-0.5, hjust=0.5, size=3.0) +
  xlab("WTP Naturalness") +
  ylab("Rent Coefficient")

ggsave("Figures/full_corr_natural_rent.png", dpi = "print",  width = 7, height = 5)


#### Compare the standard deviation of Naturalness ####
city_names <- c("Bremen", "Hanover", "Cologne", "Berlin", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt",
                "Hamburg", "Leipzig", "Munich", "Nuremberg", "Stuttgart")
alpha = 0.05 # set confidence level 
wtp_cities_sig <- data.frame(city_names=character(0), city_estimates=numeric(0), margin_of_error=numeric(0))

for(city_name in city_names){
  model <- apollo_loadModel(paste0("Estimation_results/MXL_full_", city_name))
  city_estimates <- model$estimate[8]
  city_se <- model$se[8]
  margin_of_error <- qnorm(1-alpha/2)*city_se # margin of error  
  
  wtp_cities_sig <- rbind(wtp_cities_sig, 
                      data.frame(city_names=city_name, city_estimates=city_estimates, margin_of_error=margin_of_error))
}


## Check standard deviation Naturalness Attribute ##

# Alphabetic order 
ggplot() +
  geom_errorbar(data = wtp_cities_sig, aes(x= city_names, 
                                       ymin=abs(city_estimates)-margin_of_error, 
                                       ymax=abs(city_estimates)+margin_of_error), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  geom_bar(data=wtp_cities_sig, aes(x=city_names, y=city_estimates), stat='identity', position='dodge', fill="darkgreen", alpha=0.5) +
  xlab("City") +
  ylab("SD of WTP (€/month)") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/full_wtp_cities_ci_sig.png", dpi = "print",  width = 7, height = 5)


#### Compare distributions of two cities as an example ####

ggplot() +
  stat_function(fun = dnorm, geom = "area", args= list(mean = wtp_cities$city_estimates[9], 
                                        sd= wtp_cities_sig$city_estimates[9]), aes(fill="Frankfurt"), alpha=0.4) +
  stat_function(fun = dnorm, geom = "area", args= list(mean = wtp_cities$city_estimates[14], 
                                        sd= wtp_cities_sig$city_estimates[14]), aes(fill="Stutgart"), alpha=0.4) +
  scale_x_continuous(limits = c(-30, 75)) +
  scale_fill_brewer(palette = "Set1", labels = c("Frankfurt", "Stuttgart"), name="") +
  theme(legend.position = c(0.85, 0.85)) +
  ylab("Density") +
  xlab("WTP Naturalness (€/month)")

ggsave("Figures/density_fra_stutt.png", dpi ="print", width=7, height = 5)


### Check correlation between Mean's and SD's
ggplot() + 
  geom_point(aes(x=wtp_cities$city_estimates[1:14], y=wtp_cities_sig$city_estimates))+
  geom_smooth(aes(x=wtp_cities$city_estimates[1:14], y=wtp_cities_sig$city_estimates), method=lm) +
  xlab("Mean") +
  ylab("Standard Deviation")

ggsave("Figures/mean_sd.png", dpi = "print",  width = 7, height = 5)

#### Check preference parameter for naturalness (approximation) ####

wtp_wide <- wtp_wide %>% mutate(Pref_Naturalness = (-exp(WTP_Rent) * WTP_Naturalness)*-1)
ggplot(data=wtp_wide) + geom_bar(aes(x=City, y=Pref_Naturalness), stat="identity", fill="darkseagreen") +
  scale_x_discrete(guide = guide_axis(angle = 45)) 


#### Make model estimates table for the appendix #####

city_names <- c("Bremen", "Hanover", "Cologne", "Berlin", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt",
                "Hamburg", "Leipzig", "Munich", "Nuremberg", "Stuttgart")
model_list <- list()

for(city_name in city_names){
  model_list[[city_name]] <- apollo_loadModel(paste0("Estimation_results/MXL_full_", city_name))
}

texreg(model_list, "Tables/model_full.tex", stars = c(0.01, 0.05, 0.1))
