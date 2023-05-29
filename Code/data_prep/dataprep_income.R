#### Load income data from Destatis #####

income_cities <- read.csv2("Data/income_data_destatis.csv", encoding = "latin1") %>% filter(X != "")
colnames(income_cities) <- c("Year", "Code", "City_type", "Dis_Income", "Dis_Income_ca")
income_cities <- income_cities %>% select("City_type", "Dis_Income_ca") %>% 
  mutate(City = sub(",.*", "", City_type), Type = sub("^.*, ", "", City_type)) %>% select(City, Type, Dis_Income_ca)
income_cities$City <- trimws(income_cities$City)


# Income data for 14 cities

city_names <- c("Bremen", "Hannover", "Köln", "Berlin", "Dortmund", "Dresden", "Düsseldorf", "Essen", "Frankfurt am Main",
                "Hamburg", "Leipzig", "München", "Nürnberg", "Stuttgart")

income_cities500 <- income_cities %>% filter(City %in% city_names) %>% mutate(Dis_Income_ca = as.numeric(Dis_Income_ca)) %>% 
  filter(!is.na(Dis_Income_ca)) %>% 
  filter(Type != "Landkreis" & Type != "Stat. Region" & Type != "Regierungsbezirk" & Type != "  Bremen") %>% 
  mutate(Income = Dis_Income_ca/12) %>% select(City, Income) %>% 
  mutate(City = case_when(City == "Frankfurt am Main"~ "Frankfurt", City == "Hannover" ~ "Hanover",
                          City == "München" ~ "Munich", City == "Nürnberg" ~ "Nuremberg", 
                          City == "Köln" ~ "Cologne", TRUE ~ City))

all_city <- left_join(all_city, income_cities500, by ="City")