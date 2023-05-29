# Load additional packages
library(olsrr)
library(car)

# Add per city mean values to data set for interaction effects 
all_city <- all_city %>% mutate(Income_mean = scale(Income_Dis_Present, scale=FALSE),
                                Age_mean = scale(Age, scale=FALSE), FlatSize_mean = scale(FlatSize, scale=FALSE),
                                Education_mean = scale(education, scale=FALSE), RentSQM = Miete_3/FlatSize,
                                Ratio_inc_rent = Miete_3/Income_Dis_Present, Park_mean = scale(Park, scale = FALSE),
                                Naturalness_mean = scale(NaturalnessUGS, scale=FALSE), 
                                ugs_capita_mean = scale(ugs_capita, scale=FALSE), 
                                sport_leisure_mean = scale(sport_leisure, scale=FALSE),
                                pollution_mean = scale(Year_pm10_avg, scale=FALSE),
                                water_share_mean = scale(water_share, scale=FALSE),
                                all_green_mean = scale(all_green, scale=FALSE)) %>% 
  mutate(Income_mean = as.numeric(Income_mean), Age_mean = as.numeric(Age_mean), FlatSize_mean = as.numeric(FlatSize_mean),
         Education_mean = as.numeric(Education_mean), Park_mean = as.numeric(Park_mean),
         Naturalness_mean = as.numeric(Naturalness_mean)*10, ugs_capita_mean = as.numeric(ugs_capita_mean),
         sport_leisure_mean = as.numeric(sport_leisure_mean)*100, pollution_mean = as.numeric(pollution_mean),
         water_share_mean = as.numeric(water_share_mean)*100, all_green_mean = as.numeric(all_green_mean)*100,
         Income_city = Income_Dis_Present, sealed_share_mean = as.numeric(scale(sealed_share, scale=FALSE))*100,
         forest_share_mean = as.numeric(scale(forest_share, scale=FALSE))*100, 
         garden_mean = as.numeric(scale(garden, scale=FALSE))*100, Income_destatis_mean = as.numeric(scale(Income, scale=FALSE)),
         gender_mean = as.numeric(scale(gender_female, scale=FALSE))*100, kids_mean = as.numeric(scale(KidsDummy, scale=FALSE)*100))



all_city_subset <- all_city %>% select(City, sealed_share, Density, ugs_capita, forest_capita, Year_pm10_avg,
                                       hot_days_city, Income_mean, Education_mean, Age_mean, FlatSize_mean,
                                       Park_mean, Naturalness_mean, all_green_mean, water_share_mean,
                                       sport_leisure_mean, all_green, water_share, sport_leisure, Income_city)

income_transfer <- all_city %>% select(City, Income_Dis_Present, WTP_Naturalness) %>% left_join(wtp_cities_log_compare,
                                                                               by = "City") %>% 
  mutate(WTP_Income = 0.026 * 0.3* Income_Dis_Present, Error = (WTP_Income - WTP_Naturalness)/WTP_Naturalness)

database_merged <- database_merged %>% mutate(Ratio_inc_rent = Miete_3/Income_Dis_Present)

database_bt <- left_join(database_merged, all_city_subset, by="City")


#### Estimate over identified model ####
over_model <- lm(data=all_city, WTP_Naturalness ~ all_green_mean + sport_leisure_mean + Income_mean +
                   water_share_mean + pollution_mean + Naturalness_mean +
                   Park_mean)

#### Lasso regression to test for relevant variables #####
library(glmnet)
#define response variable
y <- all_city$WTP_Naturalness

#define matrix of predictor variables
x <- data.matrix(all_city[ , c('all_green_mean', 'sport_leisure_mean' ,'Income_mean',
                               'water_share_mean',  'pollution_mean',
                               'Naturalness_mean', 'Park_mean')])

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1, type.measure = "deviance")

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda


#produce plot of test MSE by lambda value
plot(cv_model) 

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)


#### Linear Regression model reported in the paper ####

model_nat <- lm(data=all_city, WTP_Naturalness ~  sport_leisure_mean + all_green_mean + Income_mean + water_share_mean +
                  Naturalness_mean)
stargazer(model_nat, type ="latex", out ="Tables/regression_nat.tex")

# Test log specification of naturalness values #
model_nat_log <- lm(data=all_city, log(WTP_Naturalness) ~  sport_leisure_mean + all_green_mean + Income_mean + water_share_mean +
                  Naturalness_mean)

#### Robustness check linear regression ####
#QQ Plot for appendix 
png("Figures/qqplot.png", width = 7, height = 5, units = "in", res=300)
ols_plot_resid_qq(model_nat)
dev.off()
ols_test_normality(model_nat)
# Breusch Pagan test 
ols_test_breusch_pagan(model_nat)
vif_nat <- as.data.frame(vif(model_nat))
vif_nat <- rownames_to_column(vif_nat)

# VIF Figure for the appendix
ggplot(data=vif_nat) +
  geom_bar(aes(x=rowname, y=vif(model_nat)), stat="identity", width = 0.8) +
  scale_x_discrete(labels=c("UGS Share", "Income", "Naturalness", "Sport & Leisure Share", "Water Bodies Share"),
                   guide = guide_axis(angle = 0)) +
  ylim(c(0,3)) +
  ylab("Variance Inflation Factor") +
  xlab("Variable")

ggsave("Figures/vif.png", width=7, height = 5, dpi="print")


### Income only model socio_demo part, table appendix ####

model_inc <- lm(data=all_city, WTP_Naturalness ~ Income_Dis_Present)
ols_plot_resid_qq(model_inc)
stargazer(model_inc, type ="latex", out ="Tables/regression_inc.tex")

# Remove old values #
rm(forest_area, sport_leisure_area, urban_fabric, urban_fabric_area, 
   water_share, natural_green_share, natural_green_area, pastures_share, 
   all_green_area, sealed_area, observations, Year_pm10_avg)

database <- database_bt 

# Mean centering 

database <- database %>% mutate(all_green_mean = as.numeric(scale(all_green, scale=FALSE)*100),
                                water_share_mean = as.numeric(scale(water_share, scale=FALSE)*100),
                                sport_leisure_mean = as.numeric(scale(sport_leisure, scale=FALSE)*100),
                                Income_mean = as.numeric(scale(Income_city, scale=FALSE)),
                                pollution_mean = as.numeric(scale(Year_pm10_avg, scale=FALSE)),
                                ugs_capita_mean = as.numeric(scale(ugs_capita, scale=FALSE)))


#### Compute BT errors ####

city_names <- c("Berlin", "Bremen", "Cologne", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt",
                "Hamburg", "Hanover", "Leipzig", "Munich", "Nuremberg", "Stuttgart")

transfer_error <- list()
TE_mean_value <- list()
TE_income <- list()

for (city in city_names) {
  all_city_temp <- all_city %>% filter(City != city) %>% mutate(Naturalness_mean = 0)
  city_temp <- all_city %>% filter(City == city)
  model_temp <- lm(data=all_city_temp, WTP_Naturalness ~  sport_leisure_mean + all_green_mean + Income_mean + water_share_mean +
                     Naturalness_mean)
  model_temp_inc <- lm(data=all_city_temp, WTP_Naturalness ~ Income_destatis_mean)
  transfer_error[[city]] <- (predict(model_temp, city_temp) - city_temp$WTP_Naturalness)/city_temp$WTP_Naturalness
  TE_mean_value[[city]] <- (mean(all_city_temp$WTP_Naturalness) - city_temp$WTP_Naturalness)/city_temp$WTP_Naturalness
  TE_income[[city]] <- (predict(model_temp_inc, city_temp) - city_temp$WTP_Naturalness)/city_temp$WTP_Naturalness
}

transfer_error <- as.data.frame(transfer_error)
colnames(transfer_error) <- city_names
transfer_error <- transfer_error %>% 
  mutate(row = row_number()) %>% 
  pivot_longer(-row, names_to = "City", values_to = "TE") %>% select(-row)

transfer_error$value_transfer <- unlist(TE_mean_value)
transfer_error$income_transfer <- unlist(TE_income)

means <- c("Average TE", mean(abs(transfer_error$TE)), mean(abs(transfer_error$value_transfer)), mean(abs(transfer_error$income_transfer)))
transfer_error <- rbind(transfer_error, means)
transfer_error <- transfer_error %>% mutate(across(c("TE", "value_transfer", "income_transfer"), as.numeric))

transfer_error_melt <- melt(transfer_error, id="City")
transfer_error_melt$value <- as.numeric(transfer_error_melt$value)

# Figure compare transfer approaches appendix #
ggplot(data=transfer_error_melt) +
  geom_bar(aes(x=City, y=value*100, fill=variable),  width=.5, stat="identity", position = "dodge") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  ylab("Transfer Error (%)") +
  geom_hline(yintercept=0) +
  scale_fill_manual(values = c("gray10","skyblue", "darkblue"), name = "Approach", 
                    labels = c("Function", "Value", "Income")) +
  theme(axis.text.x = element_text(face = c('bold', rep('plain', 14))),
        legend.position = "bottom") 

ggsave("Figures/transfer_error.png", dpi="print", width=7, height = 5)

ggplot(data=transfer_error) +
  geom_point(aes(x=City, y=as.numeric(TE)*100), shape=15, size=2, col="darkblue") +
  geom_point(aes(x=City, y=as.numeric(value_transfer)*100), shape=17, col="red", size=2) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  ylab("Transfer Error (%)") +
  geom_hline(yintercept=0) 

#### WD model for the paper ####

model_wd <- lm(data=all_city, WTP_Walking_Distance ~ all_green_mean + sport_leisure_mean + 
               water_share_mean + Income_mean + Naturalness_mean )
summary(model_wd)
stargazer(model_wd, type ="latex", out ="Tables/regression_wd.tex")
