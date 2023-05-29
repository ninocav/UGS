#### Compare socio-demographic and value transfer model in terms of prediction ####
bt_socio <- apollo_loadModel("Estimation_results/interactions/MXL_full_int")
bt_value <- apollo_loadModel("Estimation_results/MXL_full")



#### Write a function to predict naturalness values ####

predict_wtp <- function(policy_site, bt_model) {
  # Extract the relevant variables from the data frame
  income <- policy_site$Income_mean
  ugs <- policy_site$ugs_capita_mean
  pollution <- policy_site$Year_pm10_avg
  density <- policy_site$Density
  park <- policy_site$Park
  age <- policy_site$Age_mean
  education <- policy_site$education
  kids <- policy_site$KidsDummy
  gender <- policy_site$gender_female
  flatsz <- policy_site$FlatSize_mean
  water <- policy_site$water_share_mean
  sport_leisure <- policy_site$sport_leisure_mean
  garden <- policy_site$garden
  
  
  # Calculate the predicted value using the mu_nat coefficients
  mu_natural <- bt_model$estimate["mu_natural"]
  mu_nat_income <- ifelse("mu_nat_income" %in% names(bt_model$estimate), bt_model$estimate["mu_nat_income"], 0)
  mu_nat_ugs <- ifelse("mu_nat_ugs" %in% names(bt_model$estimate), bt_model$estimate["mu_nat_ugs"], 0)
  mu_nat_pol <- ifelse("mu_nat_pol" %in% names(bt_model$estimate), bt_model$estimate["mu_nat_pol"], 0)
  mu_nat_density <- ifelse("mu_nat_density" %in% names(bt_model$estimate), bt_model$estimate["mu_nat_density"], 0)
  mu_nat_park <- ifelse("mu_nat_park" %in% names(bt_model$estimate), bt_model$estimate["mu_nat_park"], 0)
  mu_nat_age <- ifelse("mu_nat_age" %in% names(bt_model$estimate), bt_model$estimate["mu_nat_age"], 0)
  mu_nat_education <- ifelse("mu_nat_education" %in% names(bt_model$estimate), bt_model$estimate["mu_nat_education"], 0)
  mu_nat_kids <- ifelse("mu_nat_kids" %in% names(bt_model$estimate), bt_model$estimate["mu_nat_kids"], 0)
  mu_nat_gender <- ifelse("mu_nat_gender" %in% names(bt_model$estimate), bt_model$estimate["mu_nat_gender"], 0)
  mu_nat_flatsz <- ifelse("mu_nat_flatsize" %in% names(bt_model$estimate), bt_model$estimate["mu_nat_flatsize"], 0)
  mu_nat_water<- ifelse("mu_nat_water" %in% names(bt_model$estimate), bt_model$estimate["mu_nat_water"], 0)
  mu_nat_sport <- ifelse("mu_nat_sport" %in% names(bt_model$estimate), bt_model$estimate["mu_nat_sport"], 0)
  mu_nat_garden <- ifelse("mu_nat_garden" %in% names(bt_model$estimate), bt_model$estimate["mu_nat_garden"], 0)
  
  pred <- mu_natural + mu_nat_income * income + mu_nat_park * park +
    mu_nat_ugs * ugs + mu_nat_pol * pollution + mu_nat_density * density + mu_nat_age * age +
    mu_nat_education * education + mu_nat_kids * kids + mu_nat_gender * gender + mu_nat_flatsz * flatsz +
    mu_nat_water * water + mu_nat_sport * sport_leisure + mu_nat_garden * garden 
  
  return(pred)
}

# Test for all_city dataframe 
prediction <- data.frame(City = all_city$City, WTP_Naturalness = all_city$WTP_Naturalness, 
                         Income = all_city$Income_Dis_Present)
prediction$pred_value<- predict_wtp(all_city, bt_value)
prediction$pred_socio <- predict_wtp(all_city, bt_socio)


#### Calculate prediction error ####
prediction <- prediction %>% mutate(pred_err_value = (pred_value - WTP_Naturalness)/WTP_Naturalness,
                                    pred_err_socio = (pred_socio - WTP_Naturalness)/WTP_Naturalness)


errors_sum <- prediction %>% select(starts_with("pred_err")) %>% summarize_all(~ sum(abs(.)))

errors_mean<- prediction %>% select(starts_with("pred_err")) %>%  summarize_all(~mean(abs(.)))


#### Create Table socio-demographic interactions model for paper ####
##### Load results mxl with interactions ######
mxl_full_int <- apollo_loadModel("Estimation_results/interactions/MXL_full_int")
dir.create("Tables/mxl_models")
texreg(mxl_full_int, "Tables/mxl_models/socio_int.tex", digits=3)
                                     