library(dplyr)
library(tidyr)

files <- list.files("data/CAA_punctuality", full.names = TRUE)
data_raw <- list()

for(i in 1:length(files)){
  flights <- read.csv(files[i],
                      stringsAsFactors = FALSE)
  if(i %in% c(1,2)){
    flights_ly <- flights[,c("reporting_airport","origin_destination_country",
                             "origin_destination","airline_name",
                             "arrival_departure","scheduled_charter",
                             "previous_year_month_flights_matched")]
    
    names(flights_ly) <- c("reporting_airport","origin_destination_country",
                           "origin_destination","airline_name",
                           "arrival_departure","scheduled_charter",
                           "total_flights")
    flights_ly <- flights_ly[!(flights_ly$origin_destination_country == "UNITED KINGDOM" & 
                                 flights_ly$arrival_departure == "D"),]
  }
  flights$total_flights <- flights$number_flights_matched + flights$actual_flights_unmatched
  flights <- flights[,c("reporting_airport","origin_destination_country",
                        "origin_destination","airline_name",
                        "arrival_departure","scheduled_charter",
                        "total_flights")]
  # Exclude flights leaving for the UK
  # Otherwise will be double counted
  flights <- flights[!(flights$origin_destination_country == "UNITED KINGDOM" & flights$arrival_departure == "D"),]
  
  
  year <- strsplit(files[i], "/")
  year <- year[[1]][3]
  year <- as.integer(substr(year,1,4))
  flights$year <- year
  if(i %in% c(1,2)){
    flights_ly$year <- year - 1
    flights <- rbind(flights, flights_ly)
  }
  data_raw[[i]] <- flights 
}

data_raw <- dplyr::bind_rows(data_raw)


data_raw$airport2[data_raw$origin_destination == "ISTANBUL" & data_raw$year < 2019] = "ISTANBUL ATATURK"


flow <- data_raw %>%
  group_by(reporting_airport, origin_destination, origin_destination_country, year) %>%
  summarise(total_flights = sum(total_flights, na.rm = TRUE)) %>%
  spread(year, total_flights)


# Put into Standard format
names(flow) <- c("airport1","airport2","airport2_country",paste0("flt_",1990:2021))
flow$airport1_country <- "United Kingdom"
flow$airport1 <- tools::toTitleCase(tolower(flow$airport1))
#gsub("\xd3","O",flow$airport2[5159], fixed = TRUE)

flow$airport2 <- stringi::stri_trans_general(flow$airport2, "latin-ascii")
flow$airport2 <- tools::toTitleCase(tolower(flow$airport2))
flow$airport2_country <- tools::toTitleCase(tolower(flow$airport2_country))
flow <- flow[,c("airport1","airport1_country","airport2_country","airport2",paste0("flt_",1990:2021))]

saveRDS(flow,"data/clean/flights_od_2021.Rds")
