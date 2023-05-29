### Load in data from the third survey round ###

third_wave_data <- read_dta("Data/Third_wave/full_sample_choice_cards_reshaped_labeled_clean.dta", encoding = "UTF-8")

third_wave_data <- third_wave_data %>% rename(City = "f0a4") %>% mutate(City= factor(City))


third_wave_data <- third_wave_data %>% select_if(~ sum(!is.na(.)) > 0) # remove NA'

third_wave_data <- third_wave_data %>% rename(Birthyear = "f0b1sq001_sq001", LifeSatisfaction = "f1asq001",
                                              Gender = "f7a", IncomePresent = "f7c", D_Graduation = "f8d",
                                              Graduation = "f8e", Kaltmiete = "f2a1", NK = "f2a2",
                                              FlatSize = "f2b", TypeUGS = "f4x2", WalkingDistance = "f4a",
                                              KidsNo = "f8b", Garden = "f2esq003", Kleingarten = "f2f", SatUGS = "f4x5sq001", SatUGSgeneral = "f4x8sq001",
                                              Street = "f0a1sq001", Number = "f0a1sq002", ResidentSince = "f2dsq001_sq001")
# fix UTF encoding issues 
third_wave_data$Street <- gsub("Ã\u009f", "ß", third_wave_data$Street, fixed = TRUE)
third_wave_data$Street <- gsub("Ã¼", "ü", third_wave_data$Street, fixed = TRUE) 
third_wave_data$Street <- gsub("Ã¤", "ä", third_wave_data$Street, fixed = TRUE)
third_wave_data$Street <- gsub("Ã¶", "ö", third_wave_data$Street, fixed = TRUE) 
third_wave_data$Street <- gsub("Ã\u0096", "Ö", third_wave_data$Street, fixed = TRUE) 
third_wave_data$Street <- gsub("â\u0080\u0093", "-", third_wave_data$Street, fixed = TRUE)

third_wave_data <- third_wave_data %>% mutate(LifeSatisfaction = as.numeric(recode(LifeSatisfaction, A1 = "1", A2 = "2", A3= "3", A4 = "4", 
                                                                         A5 = "5", A6 = "6", A7 = "7", A8 = "8", A9 = "9", A10 = "10", A11 = "11")),
                                              Gender = as.numeric(recode(Gender, A1 = "1", A2 = "2", A3= "3")),
                                              IncomePresent = recode_factor(IncomePresent, A1 = "1", A2 = "2", A3= "3", A4 = "4", 
                                                                            A5 = "5", A6 = "6", A7 = "7", A8 = "8", A9 = "9", A10 = "10", A11 = "NA"),
                                              D_Graduation = as.numeric(recode(D_Graduation, A1 = "1", A2 ="2")),
                                              TypeUGS = as.numeric(recode(TypeUGS, A1 = "1", A2 = "2", A3= "3", A4 = "4", 
                                                                      A5 = "5", A6 = "6", A7 = "7", A8 = "8", A9 = "9", A10 = "10", A11 = "11",
                                                                      "-oth-" = "99")),
                                              Graduation = as.numeric(recode(Graduation, A1 = "1", A2 = "2", A3= "3", A4 = "4", 
                                                                             A5 = "5")),
                                              Garden = as.numeric(recode(Garden, A1 = "1", A2 = "2")), Kleingarten = as.numeric(recode(Kleingarten, A1 = "1", A2 = "2")),
                                              SatUGS = as.numeric(recode(SatUGS, A1 = "1", A2 = "2", A3= "3", A4 = "4", 
                                                                         A5 = "5", A6 = "6", A7 = "7", A8 = "8", A9 = "9", A10 = "10", A11 = "11")),
                                              SatUGSgeneral = as.numeric(recode(SatUGSgeneral, A1 = "1", A2 = "2", A3= "3", A4 = "4", 
                                                                                A5 = "5", A6 = "6", A7 = "7", A8 = "8", A9 = "9", A10 = "10", A11 = "11")))
                                                    

### Drop respondents as in the last waves ###

third_wave_data <- third_wave_data %>% filter(WalkingDistance > 0 & WalkingDistance < 240) %>% 
  filter(Kaltmiete >= 50 & Kaltmiete < 60000 & FlatSize >=12 & FlatSize <= 500 & NK < 10000 &
           NK > 0)

#Additionally drop unrealistic kidsNo -1 and 32

third_wave_data <- third_wave_data %>% filter(KidsNo >= 0 & KidsNo < 32)

### Add some variables like dummy for Gender etc.

third_wave_data <- third_wave_data %>% 
  mutate(gender_female = case_when(Gender==2 ~1, TRUE~0),
         KidsDummy = case_when(KidsNo > 0 ~1, TRUE ~0),
         Age = 2022-Birthyear, MietePerSqm=Kaltmiete/FlatSize,
         Income_Dis_Present = case_when(IncomePresent == 1 ~ 451.3, IncomePresent == 2 ~ 868.3,
                                                                  IncomePresent == 3 ~ 1300.6, IncomePresent == 4 ~ 1768,
                                                                  IncomePresent == 5 ~ 2252.4, IncomePresent == 6 ~ 2744.2,
                                                                  IncomePresent == 7 ~ 3239.6, IncomePresent == 8 ~ 3736.9,
                                                                  IncomePresent == 9 ~ 4441.2, IncomePresent == 10 ~ 6375.4,
                                                                  TRUE ~ NA_real_),
         education = case_when(Graduation == 3 | Graduation == 5 ~ 1, TRUE~0),
         garden = case_when(Garden==1 ~1, TRUE ~0), Park = case_when(TypeUGS == 1 ~ 1, TRUE ~ 0))

### Reshape data for Apollo Use ###

database_third <- pivot_wider(third_wave_data, names_from = alt,
                            values_from = c("alt1", "alt2", "alt3", 
                                            "choosen","status_quo", "Naturnähe", "Erreichbarkeit","Miete"))


# Create choice variable for apollo and dummy for education and gender 
database_third <- database_third %>% mutate(choice = case_when(choosen_1 ==1 ~ 1, choosen_2 ==1 ~ 2, choosen_3 ==1~3),
                                            NaturalnessUGS = Naturnähe_3, Sample = "IMUG")

### Remove respondents that are duplicates ###
database_third$Address <- paste(database_third$Street, database_third$Number, database_third$City, sep = ", ")
database_grouped <- database_third %>% group_by(id) %>% 
  summarize(across(c(Birthyear, LifeSatisfaction, ResidentSince, WalkingDistance, Sample, ResidentSince, submitdate,
                     FlatSize, Kaltmiete, NK, Gender, KidsNo, Address),
                   list(unique = unique), .names = "{col}"))


duplicated_address <- database_grouped[duplicated(database_grouped$Address) | duplicated(database_grouped$Address, fromLast = TRUE), ]
duplicated_address <- duplicated_address %>% arrange(Address)

duplicates <- duplicated_address[duplicated(duplicated_address[, c("Address", "Birthyear", "Gender", "ResidentSince")]) | 
                                   duplicated(duplicated_address[, c("Address", "Birthyear", "Gender", "ResidentSince")], fromLast = TRUE), ]

keep <- distinct(duplicates, Address, .keep_all = TRUE)
keep_id <- keep$id
duplicates <- duplicates %>% dplyr::filter(!id %in% keep_id) # this have to be removed 11 respondents
remove_id <- duplicates$id

database_third <- database_third %>% dplyr::filter(!id %in% remove_id)



##### Create unique ID's to avoid conflicts with former survey rounds #####

database_third <- database_third %>% mutate(id = id*100)

database_third <- database_third %>% filter(City != "-oth-")

##### Remove respondents that live outside the city borders #####
database_third <- database_third %>% dplyr::filter(id != 10111 & 147500 & 245400 & 307100 & 312300 &  327900 & 383600 & 506000 & 
                                                     338000 & 10427 &  14965 &  16109 &  16205 &  11192 & 14892 & 16482 & 
                                                     19187 &  272500 &  589300 & 3947 & 10478 &  14937)

#### Merge databases #####
database_third <- database_third %>% mutate(City = recode(City, "Frankfurt am Main" = "Frankfurt",
                                                          "Köln" = "Cologne", "Hannover" = "Hanover", "München" = "Munich",
                                                          "Nürnberg" = "Nuremberg"))


database_cleaned <- dplyr::select(database_all, -starts_with("f4x"), -IncomePresent, -Graduation) %>% rename(id = "Id") %>% 
  mutate(City = as.factor(City_id))

database_merged <- bind_rows(database_cleaned, database_third)
database_merged <- database_merged %>% filter(City != "-oth-" & City!="NA")

### Remove respondents that are duplicates between IMUG and Respondi ###
database_grouped <- database_merged %>% group_by(id) %>% 
  summarize(across(c(Birthyear, LifeSatisfaction, ResidentSince, WalkingDistance, Sample, ResidentSince, submitdate,
                     FlatSize, Kaltmiete, NK, Gender, KidsNo, Address, Age, City, NaturalnessUGS, Income_Dis_Present,
                     gender_female, education, KidsDummy, SatUGS, Park, Garden),
                   list(unique = unique), .names = "{col}"))

duplicated_address <- c()
duplicated_address <- database_grouped[duplicated(database_grouped$Address) | duplicated(database_grouped$Address, fromLast = TRUE), ]
duplicated_address <- duplicated_address %>% arrange(Address)

duplicates <- duplicated_address[duplicated(duplicated_address[, c("Address", "Birthyear", "Gender", "ResidentSince")]) | 
                                   duplicated(duplicated_address[, c("Address", "Birthyear", "Gender", "ResidentSince")], fromLast = TRUE), ]

keep <- distinct(duplicates, Address, .keep_all = TRUE)
keep_id <- keep$id
duplicates <- duplicates %>% dplyr::filter(!id %in% keep_id) %>% 
  dplyr::filter(Sample=="IMUG")# this have to be removed 90 respondents
remove_id <- duplicates$id

database_merged <- database_merged %>% dplyr::filter(!id %in% remove_id)
database_third <- database_third %>% dplyr::filter(!id %in% remove_id)
database_grouped <- database_grouped %>% dplyr::filter(!id %in% remove_id)

#store all data frames for the 14 cities in one list
cities <- c("Berlin", "Bremen", "Cologne", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt", 
            "Hamburg", "Hanover", "Leipzig", "Munich", "Nuremberg", "Stuttgart")


citylist <- map(set_names(cities), ~ filter(database_third, City == .x))


