##### Air pollution ####
# load pollution data for last five years from:
# https://www.umweltbundesamt.de/daten/luft/luftdaten/jahresbilanzen/eJxrWpScv9BwUWXqEiMDQwsAMLMFuA==
years <- list("2018", "2019", "2020", "2021", "2022")
pollution <- list()
for (i in years) {
pollution[[i]] <- read.csv2(paste0("Data/Climate/Jahresbilanzen_Feinstaub_", i, ".csv"))  
}

# Filter for relevant cities
station_name <- c("Berlin", "Bremen", "Dortmund", "Dresden", "Düsseldorf", "Köln", "Essen", "Frankfurt", "Hamburg",
                  "Hannover", "Leipzig", "München", "Nürnberg", "Stuttgart")


pollution <- map(pollution, ~ filter(.x, grepl(paste(station_name, collapse = "|"), str_extract(Stationsname, "^[^-]+"))))
pollution <- map(pollution, ~mutate(.x, City = case_when(
  grepl("Berlin", Stationsname, ignore.case = TRUE) ~ "Berlin",
  grepl("Bremen", Stationsname, ignore.case = TRUE) ~ "Bremen",
  grepl("Dortmund", Stationsname, ignore.case = TRUE) ~ "Dortmund",
  grepl("Dresden", Stationsname, ignore.case = TRUE) ~ "Dresden",
  grepl("Düsseldorf", Stationsname, ignore.case = TRUE) ~ "Düsseldorf",
  grepl("Köln", Stationsname, ignore.case = TRUE) ~ "Cologne",
  grepl("Essen", Stationsname, ignore.case = TRUE) ~ "Essen",
  grepl("Frankfurt", Stationsname, ignore.case = TRUE) ~ "Frankfurt",
  grepl("Hamburg", Stationsname, ignore.case = TRUE) ~ "Hamburg",
  grepl("Hannover", Stationsname, ignore.case = TRUE) ~ "Hanover",
  grepl("Leipzig", Stationsname, ignore.case = TRUE) ~ "Leipzig",
  grepl("München", Stationsname, ignore.case = TRUE) ~ "Munich",
  grepl("Nürnberg", Stationsname, ignore.case = TRUE) ~ "Nuremberg",
  grepl("Stuttgart", Stationsname, ignore.case = TRUE) ~ "Stuttgart",
  TRUE ~ "unknown"
))) 

# Remove station in Mönchengladbach and order by city
pollution <- map(pollution, ~filter(.x, Stationscode!=	"DENW100")) %>% 
  map(~arrange(.x, City))

# Calculate average over city stations 
pollution <- map(pollution, ~ group_by(.x, City) %>% 
                   mutate(Average = mean(Jahresmittelwert.in.µg.m.),
                          Days_av = mean(Zahl.der.Tageswerte.über.50.µg.m3)))

# Merge data frame for all years
all_pollution <- bind_rows(pollution)

# Group by city and calculate average of the variable over 5 years
all_pollution_sum <- all_pollution %>% 
  group_by(City) %>% 
  summarize(Year_pm10_avg = mean(Jahresmittelwert.in.µg.m.),
            Day_pm10_over = mean(Zahl.der.Tageswerte.über.50.µg.m3))

# Plot average air pollution ppm10 per city
ggplot(all_pollution_sum) +
  geom_bar(aes(x=City, y=Year_pm10_avg ), stat="identity", fill="slategrey") +
  scale_x_discrete(guide = guide_axis(angle = 45))

#Filter only for traffic stations
all_pollution_traf <- all_pollution %>% 
  filter(Art.der.Station == "Verkehr") %>%  group_by(City) %>% 
  summarize(Year_pm10_avg  = mean(Jahresmittelwert.in.µg.m.),
            Day_pm10_over = mean(Zahl.der.Tageswerte.über.50.µg.m3))

all_city <- left_join(all_city, all_pollution_traf, by="City")

# Do pollution WTP plot for the paper 
ggplot(all_city, aes(x = WTP_Naturalness, y = Year_pm10_avg)) + 
  geom_point()+
  geom_text(label=City, vjust=-0.5, hjust=0.5, size=3.0) +
  xlab("WTP Naturalness (€/month)") +
  ylab(expression(paste("PM"[10], " (", mu, "g/m"^3, ")")))

ggsave("Figures/pm10.png", dpi="print", width=7, height=5)

##### Hot days #####
#data from: https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/monthly/climate_indices/kl/historical/
library(lubridate)

# Use functions to load in data for the cities 
calculate_hot_days <- function(file_path, start_year) {
  # read data
  data <- read.csv2(file_path)
  station_id <- unique(data$STATIONS_ID)
  subfolder <- basename(dirname(file_path))
  
  # convert date column
  data$date <- as.Date(as.character(data$MESS_DATUM_BEGINN), format = "%Y%m%d")
  
  # filter based on year
  data <- data %>% filter(year(date) >= start_year) %>% filter(MO_HEISSE_TAGE >= 0)
  
  # calculate hot days and hot average
  hot_days <- sum(data$MO_HEISSE_TAGE)
  hot_av <- hot_days / 10
  
  return(list(hot_days = hot_days, hot_av = hot_av, station_id = station_id, City = subfolder))
}

# set the directory path
dir_path <- "Data/Climate/Hot_days"
subfolders <- city_names

# retrieve a list of files that match the pattern
files <- list.files(dir_path, recursive = TRUE, full.names = TRUE, pattern <- "produkt_klindex_monat.*\\.txt")

# filter the list to only include files in the desired subfolders
files <- files[grep(paste0("^", dir_path, "/", paste(subfolders, collapse = "|")), files)]

start_year <- 2013
rm(hot_days_df)
results <- lapply(files, function(file) {
  calculate_hot_days(file, start_year)
})

hot_days_df <- do.call(rbind, results)

# reorder columns
hot_days_df <- hot_days_df[, c("City", "station_id", "hot_days", "hot_av")]
hot_days_df <- as.data.frame(hot_days_df)

hot_days_df$City <- unlist(hot_days_df$City)
hot_days_df$station_id <- unlist(hot_days_df$station_id)
hot_days_df$hot_days <- unlist(hot_days_df$hot_days)
hot_days_df$hot_av <- unlist(hot_days_df$hot_av)

hot_days_df$City <- as.factor(hot_days_df$City)
hot_days_sum <- hot_days_df %>% group_by(City) %>% summarize(hot_days_city = mean(hot_av))

ggplot(data=hot_days_sum) +
  geom_bar(aes(x=City, y=hot_days_city), stat="identity") +
  scale_x_discrete(guide = guide_axis(angle = 45)) + NULL 

all_city <- left_join(all_city, hot_days_sum, by="City")

# Do figure hot days WTP for the paper 
ggplot(all_city, aes(x = WTP_Naturalness, y = hot_days_city)) + 
  geom_point()+
  geom_text(label=City, vjust=-0.5, hjust=0.5, size=3.0) +
  xlab("WTP Naturalness (€/month)") +
  ylab("Number of Hot Days")

ggsave("Figures/hot_days.png", dpi="print", width=7, height=5)

#### Illustrate time series data for Bremen ####

hot_bremen <- read.csv2("Data/Climate/Hot_days/Bremen/produkt_klindex_monat_18900101_20221231_00691.txt")
hot_bremen$date <- as.Date(as.character(hot_bremen$MESS_DATUM_BEGINN), format = "%Y%m%d")
hot_bremen <- hot_bremen %>% filter(MO_HEISSE_TAGE >= 0) %>% mutate(year = year(date))

hot_bremen_sum <- hot_bremen %>% 
  group_by(year) %>% summarize(sum_year = sum(MO_HEISSE_TAGE))

ggplot(hot_bremen_sum, aes(x=year, y=sum_year)) +
  geom_bar(stat = "identity") + 
  xlab("Time") +
  ylab("Number of Hot Days") +
  geom_smooth(data=hot_bremen_sum, method=lm, col="indianred", formula = y ~ x)

ggsave("Figures/example_hot_bremen.png", dpi="print", width = 7, height = 5)
