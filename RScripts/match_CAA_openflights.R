# Match the CAA routes to the Open Flights Routes
openflights <- readRDS("data/OpenFlights/UKflights.Rds")

# Flights OD Data

flights <- read.csv("data/CAA_punctuality/2018_Annual_Punctuality_Statistics_Full_Analysis_Arrival_Departure.csv",
                    stringsAsFactors = FALSE)
flights <- flights[,1:11]
names(flights) <- c("date","reporting_period","reporting_airport",
                    "origin_destination_country","origin_destination","airline_name",
                    "arrival_departure","scheduled_charter","number_flights_matched",
                    "actual_flights_unmatched","number_flights_cancelled")
flights$date <- NULL

# group togther into OD dataset
flow <- flights %>%
  group_by(reporting_airport, origin_destination, origin_destination_country) %>%
  summarise(number_flights_matched = sum(number_flights_matched),
            actual_flights_unmatched = sum(actual_flights_unmatched),
            number_flights_cancelled = sum(number_flights_cancelled))