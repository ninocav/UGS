### Load packages and data ###

rm(list=ls())

library(GISTools)
library(tidyverse)
library(tidylog)
library(sf)
library(sp)
library(haven)
library(texreg)
library(stargazer)
library(apollo)
library(parallel)
library(xtable)
library(terra)
library(spatstat)
library(exactextractr)
library(ggpubr)
library(reshape2)
library(readxl)
library(RColorBrewer)
library(ggspatial)
library(stringi)
library(rstatix)
library(viridis)



### Specify cores for Apollo

apollo_cores <- detectCores() - 1

### Load Data from first and second survey round ###

data<- read.csv("Data/UGS_W_1_2.csv") %>% arrange(Id) %>% mutate(wide_id = paste(Id, cset, sep =""))

data <- data %>% select_if(~ sum(!is.na(.)) > 0) # remove NA'

### Drop respondents as in Bronmann et al. (2023)

data<- data %>% filter(WalkingDistance > 0 & WalkingDistance < 240) %>% 
  filter(Kaltmiete >= 50 & Kaltmiete < 60000 & FlatSize >=12 & FlatSize <= 500 & NK < 12064 &
           NK > 0)

n_distinct(data$Id)


### Reshape data for Apollo Use ###

database_all <- pivot_wider(data, names_from = alt,
                        values_from = c("alt1", "alt2", "alt3", 
                                        "choosen","status_quo", "Naturnähe", "Erreichbarkeit","Miete"))


# Create choice variable for Apollo and dummy for education and gender, adjust age values
database_all <- database_all %>% mutate(choice = case_when(choosen_1 ==1 ~ 1, choosen_2 ==1 ~ 2, choosen_3 ==1~3)) %>% 
  mutate(education = case_when(Graduation == 3 | Graduation == 5 ~ 1, TRUE~0), Sample = "Respondi",
         gender_female = case_when(Gender==2 ~1, TRUE~0), garden = case_when(Garden==2 ~1, TRUE ~0),
         TypeUGSmod = case_when(TypeUGS < 99 ~ TypeUGS, TRUE ~ NA_integer_),
         Park = case_when(TypeUGSmod == 1 ~ 1, TypeUGSmod > 1 & TypeUGSmod < 99 ~ 0,TRUE ~ NA_real_),
         KidsDummy = case_when(KidsNo > 0 ~1, TRUE ~0)) %>% rename(SatUGSgeneral = "f4x8sq001")

database_all <- separate(database_all, startdate, into = c("date", "time"), sep = " ")
database_all <- separate(database_all, date, into = c("Year", "Month", "Day"), sep ="-")
database_all$Year <- as.numeric(database_all$Year)

# calculate age based on the end of the wave e.g. 2022-1-Birth year 
# (approximately correct since wave ended in January); fixes issue with 17 years old respondents 
database_all <- database_all %>% 
  mutate(Year = ifelse(Year == 2021, Year +1, Year)) %>% 
  mutate(Age = ifelse(Year > 2020, Year -1 - Birthyear, Age))

### Create City variable for each of the 14 cities ###
database_all <- database_all %>%
  mutate(City_id = case_when(
    str_detect(City, "Ber|BER|ber|Berlin|Berlin ") & !str_detect(City, "Nürn|Nuer|Nür|NÜRN|nürn|Stoppen") ~ "Berlin",
    City %in% c("Bremen", "bremen", "Bremen ") | ort == "Bremen" ~ "Bremen",
    City %in% c("Koeln", "Kölb", "kölm", "Koln", "köln", "Köln", "KÖln", "KÖLN", "KÖLN-Zollstock", "Köln" , "Köln Mühlheim", "Köln, Kreisfreie Stadt") | ort == "Köln" ~ "Cologne",
    str_detect(City, "Dortm|dortm|DORTM") | ort =="Dortmund" ~ "Dortmund",
    str_detect(City, "Dres|dres|DRES") | ort =="Dresden" ~ "Dresden",
    str_detect(City, "ldorf") | ort =="Düsseldorf" & !str_detect(City, "Hilden") ~ "Düsseldorf",
    substr(City,1,1) == "E" & !str_detect(City, "Erfu") | substr(City,1,1) =="e"| ort =="Essen" & !str_detect(City, "Erfu") ~ "Essen",
    str_detect(City, "am Main") | substr(City,1,1) == "F" | substr(City,1,1) =="f" | ort=="Frankfurt" ~ "Frankfurt",
    substr(City,1,3) == "Ham" | substr(City,1,3) =="HAM" | ort=="Hamburg" ~ "Hamburg",
    City %in% c("hannover", "Hannover", "HANNOVER", "Hannover ", "Hannover  ") | ort == "Hannover" ~ "Hanover",
    substr(City,1,3) == "Lei" | substr(City,1,3) =="LEI" | ort=="Leipzig" ~ "Leipzig",
    substr(City,1,3) == "Mün" | substr(City,1,3) =="Mue" | ort=="München" ~ "Munich",
    substr(City,1,3) == "Nür" | substr(City,1,3) =="Nue" | substr(City, 1,3) =="nue" | ort=="Nürnberg" ~ "Nuremberg",
    substr(City,1,3) == "Stu" | substr(City,1,3) =="STU" | substr(City, 1,3) =="Stt" | ort=="Stuttgart" ~ "Stuttgart",
    TRUE ~ NA_character_
  ))

# Remove respondents that live not in the 14 largest German cities
check <- database_all %>% filter(is.na(City_id)) 
table(check$City)
rm(check)

database_all <- database_all %>% filter(!is.na(City_id))


### Remove respondents that are duplicates ###
database_all$Address <- paste(database_all$Street, database_all$Number, database_all$City, sep = ", ")
database_grouped <- database_all %>% group_by(Id) %>% 
  summarize(across(c(Birthyear, LifeSatisfaction, ResidentSince, WalkingDistance, Sample, ResidentSince, submitdate,
                     FlatSize, Kaltmiete, NK, Gender, KidsNo, HousholdSize, GraduationOther, Address),
                   list(unique = unique), .names = "{col}"))


duplicated_address <- database_grouped[duplicated(database_grouped$Address) | duplicated(database_grouped$Address, fromLast = TRUE), ]
duplicated_address <- duplicated_address %>% arrange(Address)

duplicates <- duplicated_address[duplicated(duplicated_address[, c("Address", "Birthyear", "Gender", "ResidentSince")]) | 
                                   duplicated(duplicated_address[, c("Address", "Birthyear", "Gender", "ResidentSince")], fromLast = TRUE), ]

keep <- distinct(duplicates, Address, .keep_all = TRUE)
keep_id <- keep$Id
duplicates <- duplicates %>% dplyr::filter(!Id %in% keep_id) # this have to be removed 131 respondents
remove_id <- duplicates$Id

database_all <- database_all %>% dplyr::filter(!Id %in% remove_id)

##### Remove respondents that live outside the city borders ##### (identified in script "spatial_points_analysis")
database_all <- database_all %>% dplyr::filter(Id != 10111 & 147500 & 245400 & 307100 & 312300 &  327900 & 383600 & 506000 & 
                                                 338000 & 10427 &  14965 &  16109 &  16205 &  11192 & 14892 & 16482 & 
                                                 19187 &  272500 &  589300 & 3947 & 10478 &  14937)

#store all data frames for the 14 cities in one list
cities <- c("Berlin", "Bremen", "Cologne", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt", 
            "Hamburg", "Hanover", "Leipzig", "Munich", "Nuremberg", "Stuttgart")


citylist_12 <- map(set_names(cities), ~ filter(database_all, City_id == .x))


#### Read in population data from Destatis #####

pop_data <- read_excel("Data/City data destatis 2021.xlsx", sheet=2) %>% dplyr::select(-1:-6) %>% 
  slice(-1:-6) %>% rename(City = 1, PLZ = 2, Area = 3, Total =4, Male = 5, Female = 6, Density = 7) %>% 
  mutate_at(vars(3:7), as.numeric) %>% 
  mutate(Share_Female = round(Female/Total, 3)) %>% filter(City!="Städte insgesamt")

pop_data_500 <- pop_data %>% filter(Total >= 500000) %>% mutate(City = str_extract(City, "^[^,]+")) %>% 
  mutate(City = ifelse(City == "Frankfurt am Main", "Frankfurt", City), City= ifelse(City=="Köln", "Cologne", City), 
         City =ifelse(City=="München", "Munich", City), City= ifelse(City=="Nürnberg", "Nuremberg", City),
         City=ifelse(City=="Hannover", "Hanover", City))
