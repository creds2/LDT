# batch import
library(dplyr)
library(tidyr)

files <- list.files("data/CAA_punctuality/", full.names = TRUE)
data_raw <- list()

for(i in 1:length(files)){
  flights <- read.csv(files[i],
                      stringsAsFactors = FALSE)
  flights$total_flights <- flights$number_flights_matched + flights$actual_flights_unmatched
  flights <- flights[,c("reporting_airport","origin_destination_country",
                          "origin_destination","airline_name",
                          "arrival_departure","scheduled_charter",
                          "total_flights")]
  
  
  year <- strsplit(files[i], "/")
  year <- year[[1]][3]
  year <- as.integer(substr(year,1,4))
  flights$year <- year
  data_raw[[i]] <- flights 
}

data_raw <- dplyr::bind_rows(data_raw)

flow <- data_raw %>%
  group_by(reporting_airport, origin_destination, origin_destination_country, year) %>%
  summarise(total_flights = sum(total_flights)) %>%
  spread(year, total_flights)


# Load Airpoty Locations

airports <- readRDS("airports_sf.Rds")
flow$origin_destination_country <- tolower(flow$origin_destination_country)
flow$origin_destination_country[flow$origin_destination_country == "usa"] <- "United States of America"
# Check matching values

foo <- left_join(flow, airports, by = c("origin_destination" = "origin_destination_orig", "origin_destination_country" = "origin_destination_country"))
foo <- foo[is.na(foo$lon),]
