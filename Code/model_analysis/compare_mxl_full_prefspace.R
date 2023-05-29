### Compare the different city samples for the full sample ###
# Mixed logit model in preference space #
dir.create("Figures/MXL_pref")

#### Compute Naturalness and CI's for all cities ####
city_names <- c("Bremen", "Hanover", "Cologne", "Berlin", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt",
                "Hamburg", "Leipzig", "Munich", "Nuremberg", "Stuttgart")
alpha = 0.05 # set confidence level 
wtp_cities_ps<- data.frame(city_names=character(0), city_estimates=numeric(0), margin_of_error=numeric(0))

for(city_name in city_names){
  model <- apollo_loadModel(paste0("Estimation_results/MXL_ps/MXL_full_ps_", city_name))
  city_estimates <- model$estimate[3]
  city_se <- model$se[3]
  margin_of_error <- qnorm(1-alpha/2)*city_se # margin of error  
  
  wtp_cities_ps <- rbind(wtp_cities_ps, 
                      data.frame(city_names=city_name, city_estimates=city_estimates, margin_of_error=margin_of_error))
}


## Absolute values of coefficients and paper coefficients with 95% CIs


# Alphabetic order 
nat_coef <- ggplot() +
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  geom_point(data=wtp_cities_ps, aes(x=city_names, y=city_estimates), col="darkseagreen") +
  geom_errorbar(data = wtp_cities_ps, aes(x= city_names, 
                                       ymin=abs(city_estimates)-margin_of_error, 
                                       ymax=abs(city_estimates)+margin_of_error), width=.2,position=position_dodge(.85)) +
  xlab("City") +
  ylab("Naturalness Coefficient") +
  labs(title = "") +
  labs(x = NULL) + 
  guides(x = "none") +
  scale_color_manual(values = colors)

ggsave("Figures/MXL_pref/ps_cities_ci.png", dpi = "print",  width = 7, height = 5)

#### Compute same for walking distance ####

alpha = 0.05 # set confidence level 

for(city_name in city_names){
  model <- apollo_loadModel(paste0("Estimation_results/MXL_ps/MXL_full_ps_", city_name))
  city_estimates <- model$estimate[4]
  city_se <- model$se[4]
  margin_of_error <- qnorm(1-alpha/2)*city_se # margin of error  
  
  wtp_cities_ps <- rbind(wtp_cities_ps, 
                      data.frame(city_names=city_name, city_estimates=city_estimates, margin_of_error=margin_of_error))
}

ggplot() +
  geom_point(data = wtp_cities_ps[15:28, ], aes(x=reorder(city_names, -city_estimates), y=city_estimates), col="red") +
  geom_errorbar(data = wtp_cities_ps[15:28, ], aes(x= city_names, 
                                                ymin=city_estimates-margin_of_error, 
                                                ymax=city_estimates+margin_of_error), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  xlab("City") +
  ylab("WTP (€/month)") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/MXL_pref/ps_walking_dist_cities_ci_ordered.png", dpi = "print",  width = 7, height = 5)

ggplot() +
  geom_point(data = wtp_cities_ps[15:28, ], aes(x=city_names, y=city_estimates), col="red") +
  geom_errorbar(data = wtp_cities_ps[15:28, ], aes(x= city_names, 
                                                ymin=city_estimates-margin_of_error, 
                                                ymax=city_estimates+margin_of_error), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + 
  xlab("City") +
  ylab("WTP (€/month)") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/MXL_pref/ps_walking_dist_cities_ci.png", dpi = "print",  width = 7, height = 5)

#### Compute same for rent ####

alpha = 0.05 # set confidence level 

for(city_name in city_names){
  model <- apollo_loadModel(paste0("Estimation_results/MXL_ps/MXL_full_ps_", city_name))
  city_estimates <- model$estimate[5]
  city_se <- model$se[5]
  margin_of_error <- qnorm(1-alpha/2)*city_se # margin of error  
  
  wtp_cities_ps <- rbind(wtp_cities_ps, 
                      data.frame(city_names=city_name, city_estimates=city_estimates, margin_of_error=margin_of_error))
}

ggplot() +
  geom_point(data = wtp_cities_ps[29:42, ], aes(x=reorder(city_names, -city_estimates), y=city_estimates), col="red") +
  geom_errorbar(data = wtp_cities_ps[29:42, ], aes(x= city_names, 
                                                ymin=city_estimates-margin_of_error, 
                                                ymax=city_estimates+margin_of_error), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  xlab("City") +
  ylab("WTP (€/month)") +
  labs(title = "") +
  scale_color_manual(values = colors)

ggsave("Figures/MXL_pref/ps_rent_ci_ordered.png", dpi = "print",  width = 7, height = 5)

rent_coef <- ggplot() +
  geom_point(data = wtp_cities_ps[29:42, ], aes(x=city_names, y=-exp(city_estimates)), col="red") +
  geom_errorbar(data = wtp_cities_ps[29:42, ], aes(x= city_names, 
                                                ymin=-exp(city_estimates-margin_of_error), 
                                                ymax=-exp(city_estimates+margin_of_error)), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  xlab("City") +
  ylab("Rent Coefficient") +
  labs(title = "") + 
  scale_color_manual(values = colors)

ggsave("Figures/MXL_pref/ps_rent_ci.png", dpi = "print",  width = 7, height = 5)


#### Examine correlation between walking distance and naturalness

wtp_wide_ps <- data.frame(
  City = wtp_cities_ps$city_names[1:14],
  Naturalness_coeff = wtp_cities_ps$city_estimates[1:14],
  Walking_Distance_coeff = wtp_cities_ps$city_estimates[15:28],
  Rent_coeff = wtp_cities_ps$city_estimates[29:42]
)

ggplot(data=wtp_wide_ps, aes(x = Naturalness_coeff, y = Walking_Distance_coeff)) + 
  geom_point()+
  geom_smooth(method=lm) +
  geom_text(aes(label=City), vjust=-0.5, hjust=0.5, size=3.0) +
  xlab("Naturalness Coefficient") +
  ylab("Walking Distance Coefficient")

ggsave("Figures/MXL_pref/ps_corr_natural_distance.png", dpi = "print",  width = 7, height = 5)

ggplot(data=wtp_wide_ps, aes(x = Naturalness_coeff, y = Rent_coeff)) + 
  geom_point()+
  geom_smooth(method=lm) +
  geom_text(aes(label=City), vjust=-0.5, hjust=0.5, size=3.0) +
  xlab("Naturalness Coefficient") +
  ylab("Rent Coefficient")

ggsave("Figures/MXL_pref/ps_corr_natural_rent.png", dpi = "print",  width = 7, height = 5)

##### Calculate WTP #####

wtp_wide_ps <- wtp_wide_ps %>% mutate(WTP_Naturalness_ps = -Naturalness_coeff/-exp(Rent_coeff))

ps_vs_ws <- wtp_wide_ps %>% select(City, WTP_Naturalness_ps) %>% left_join(wtp_wide, by="City") %>% 
  select(City, WTP_Naturalness_ps, WTP_Naturalness)

ps_vs_ws <- melt(ps_vs_ws)

# Figure compare WTP pref space wtp space appendix 
ggplot(data=ps_vs_ws, aes(x=City, y=value, fill=variable)) +
  geom_bar(stat="identity",  position='dodge', width = 0.8) +
  ylab("WTP Naturalness (€/month)") +
  xlab("City") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values = c("grey40", "darkseagreen"), labels = c("Preference Space", "WTP Space"), name="Specification") +
  theme(legend.position = "bottom") 

ggsave("Figures/MXL_pref/wtp_compare.png", dpi = "print",  width = 7, height = 5)


ggarrange(plotlist=list(nat_coef, rent_coef), nrow=2, common.legend = T, align = "v")
ggsave("Figures/MXL_pref/double_coef.png", dpi="print", width = 7, height = 5)



ggplot() +
  geom_point(data = wtp_cities_ps[29:42, ], aes(x=city_names, y=-exp(city_estimates)), col="red") +
  geom_errorbar(data = wtp_cities_ps[29:42, ], aes(x= city_names, 
                                                   ymin=-exp(city_estimates-margin_of_error), 
                                                   ymax=-exp(city_estimates+margin_of_error)), width=.2,position=position_dodge(.85)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL +
  xlab("City") +
  ylab("Rent Coefficient") +
  labs(title = "") +
  ylim(c(-0.5, 1.5)) +
  scale_color_manual(values = colors) +
  geom_point(data=wtp_cities_ps[1:14, ], aes(x=city_names, y=city_estimates), col="darkseagreen") +
  geom_errorbar(data = wtp_cities_ps[1:14, ], aes(x= city_names, 
                                          ymin=abs(city_estimates)-margin_of_error, 
                                          ymax=abs(city_estimates)+margin_of_error), width=.2,position=position_dodge(.85))
  

wtp_wide <- left_join(wtp_wide, wtp_wide_ps, by="City")
