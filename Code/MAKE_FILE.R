### Make file ###

source("Code/data_prep/dataprep.R")

city_names <- c("Bremen", "Hanover", "Cologne", "Berlin", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt",
                "Hamburg", "Leipzig", "Munich", "Nuremberg", "Stuttgart")

n_draws <- 2000 # set draws for mixed logit estimation

#############################################
#source("Code/mxl_cities/mxl_cities.R")
#############################################

source("Code/model_analysis/compare_samples12.R")

source("Code/GIS/city_shapes.R")

source("Code/data_prep/dataprep_thirdwave.R")

##############################################
### Remove "#" to reestimate models, otherwise models are loaded!

#source("Code/mxl_cities/mxl_cities_3.R")

#source("Code/mxl_cities/mxl_3.R")

#source("Code/mxl_cities/mxl_12.R")

#source("Code/mxl_cities/mxl_treatment.R")
#############################################

source("Code/third_wave_only.R")

source("Code/third_vs_rest.R")

#############################################
### Remove "#" to reestimate models, otherwise models are loaded!

#source("Code/mxl_cities/mxl_all_cities.R")

### Estimate conditional logit models 

#source("Code/mxl_cities/mxl_all_cities_clogit.R")

#source("Code/mxl_cities/mxl_all_cities_clogit_prefspace.R")

#source("Code/mxl_cities/mxl_all_cities_clogit_log.R")

### Estimate mxl model in preference space and log model

#source("Code/mxl_cities/mxl_all_cities_prefspace.R")

#source("Code/mxl_cities/mxl_all_cities_log.R")

#############################################

source("Code/model_analysis/compare_all.R")

source("code/model_analysis/info_compare.R")

##### Not relevant for the paper ####

source("Code/model_analysis/compare_clogit.R")

source("Code/model_analysis/compare_clogit_prefspace.R")

source("Code/model_analysis/compare_clogit_log.R")

##########################################

source("Code/model_analysis/compare_mxl_full_prefspace.R")

source("Code/model_analysis/compare_mxl_full_log.R")

source("code/socio_demos.R")

source("Code/GIS/urban_atlas.R")

source("Code/GIS/spatial_points_analysis.R")

source("Code/climate.R")


#########################################
#Estimate socio-demographic interaction model and full model 

### Remove "#" to reestimate models, otherwise models are loaded!

#source("Code/mxl_cities/mxl_full_int.R")

#source("Code/mxl_cities/mxl_full.R")


#########################################


source("Code/data_prep/dataprep_income.R")

source("Code/BT_estimation.R")

source("Code/BT_interaction.R")

source("Code/data_prep/dataprep_bt_cities.R")

source("Code/GIS/urban_atlas_bt.R")


