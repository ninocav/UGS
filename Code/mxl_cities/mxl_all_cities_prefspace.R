### Estimate mixed logit model in preference space for replication part ###
### Do this with a function ###
#store all data frames for the 14 cities in one list

cities <- c("Berlin", "Bremen", "Cologne", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt", 
            "Hamburg", "Hanover", "Leipzig", "Munich", "Nuremberg", "Stuttgart")

database_merged <- database_merged %>% arrange(id)
citylist <- map(set_names(cities), ~ filter(database_merged, City == .x))


rm(City)
city_names <- c("Berlin", "Bremen", "Cologne" , "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt",
                "Hamburg","Hanover", "Leipzig", "Munich", "Nuremberg", "Stuttgart")

z = 1

#### Apollo #####

for (i in citylist) {
  database <- i
  database$id <- as.numeric(database$id)
  
  #initialize model 
  
  apollo_initialise()
  
  
  ### Set core controls
  apollo_control = list(
    modelName  = paste0("MXL_full_ps_", city_names[z]),
    modelDescr = paste0("PS_full_ ", city_names[z]),
    indivID    ="id",
    mixing     = TRUE,
    HB= FALSE,
    nCores     = apollo_cores, # Set to number of cores - 1,
    outputDirectory = "Estimation_results/MXL_ps"
  )
  
  # Define model parameters
  # set values to values from the paper to nicely calibrate the model and speed up computation 
  
  print("test")
  
  apollo_beta=c(mu_asc1 = -1,
                mu_asc2 = -1,
                mu_natural = 1,
                mu_walking = - 0.2,
                mu_rent = -2,
                sig_asc1 = 1,
                sig_asc2 =1,
                sig_natural = 1,
                sig_walking = 0.5,
                sig_rent = 1)
  
  ### specify parameters that should be kept fixed, here = none
  apollo_fixed = c()
  
  ### Set parameters for generating draws, use 2000 sobol draws
  apollo_draws = list(
    interDrawsType = "sobol",
    interNDraws    = n_draws,
    interUnifDraws = c(),
    interNormDraws = c("draws_asc1", "draws_asc2", "draws_natural", "draws_walking", "draws_rent"),
    intraDrawsType = "halton",
    intraNDraws    = 0,
    intraUnifDraws = c(),
    intraNormDraws = c()
  )
  
  ### Create random parameters, define distribution of the parameters
  apollo_randCoeff = function(apollo_beta, apollo_inputs){
    randcoeff = list()
    
    randcoeff[["b_mu_asc1"]] = mu_asc1 + sig_asc1 * draws_asc1
    randcoeff[["b_mu_asc2"]] = mu_asc2 + sig_asc2 * draws_asc2
    randcoeff[["b_mu_natural"]] = mu_natural + sig_natural * draws_natural
    randcoeff[["b_mu_walking"]] = mu_walking + sig_walking * draws_walking
    randcoeff[["b_mu_rent"]] = -exp(mu_rent + sig_rent * draws_rent)
    
    return(randcoeff)
  }
  
  ### validate 
  apollo_inputs = apollo_validateInputs()
  apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
    
    ### Function initialisation: do not change the following three commands
    ### Attach inputs and detach after function exit
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))
    
    ### Create list of probabilities P
    P = list()
    
    ### List of utilities (later integrated in mnl_settings below) as defined in the equation in the paper
    V = list()
    V[['alt1']] = b_mu_asc1 + b_mu_natural * Naturnähe_1 + b_mu_walking * Erreichbarkeit_1 + b_mu_rent * Miete_1
    
    V[['alt2']] = b_mu_asc2 + b_mu_natural * Naturnähe_2 + b_mu_walking * Erreichbarkeit_2 + b_mu_rent * Miete_2
    
    V[['alt3']] = b_mu_natural * Naturnähe_3 + b_mu_walking * Erreichbarkeit_3 + b_mu_rent * Miete_3
    
    
    ### Define settings for MNL model component
    mnl_settings = list(
      alternatives  = c(alt1=1, alt2=2, alt3=3),
      avail         = 1, # all alternatives are available in every choice
      choiceVar     = choice,
      V             = V#,  # tell function to use list vector defined above
      
    )
    
    ### Compute probabilities using MNL model
    P[['model']] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P = apollo_panelProd(P, apollo_inputs, functionality)
    
    ### Average across inter-individual draws - nur bei Mixed Logit!
    P = apollo_avgInterDraws(P, apollo_inputs, functionality)
    
    ### Prepare and return outputs of function
    P = apollo_prepareProb(P, apollo_inputs, functionality)
    return(P)
  }
  
  
  
  # ################################################################# #
  #### MODEL ESTIMATION                                            ##
  # ################################################################# #
  # estimate model with bfgs algorithm
  
  mxl = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, 
                        estimate_settings=list(maxIterations=400,
                                               estimationRoutine="bfgs",
                                               hessianRoutine="analytic"))
  
  
  
  # ################################################################# #
  #### MODEL OUTPUTS                                               ##
  # ################################################################# #
  apollo_saveOutput(mxl)
  z= z+1
}

