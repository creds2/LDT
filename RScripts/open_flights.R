# Open Flights
library(dplyr)

routes <- read.delim("data/OpenFlights/routes.dat", header = FALSE, 
                     sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
names(routes) <- c("Airline","AirlineID","Origin","OriginID","Dest","DestID",
                   "Codeshare","Stops","Equipment")

airlines <- read.delim("data/OpenFlights/airlines.dat", header = FALSE, 
                       sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
names(airlines) <- c("AirlineID","Name","Alias","IATA","ICAO","Callsign",
                   "Country","Active")

airports <- read.delim("data/OpenFlights/airports.dat", header = FALSE, 
                       sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
names(airports) <- c("AirportID","Name","City","Country","IATA","ICAO","Latitude",
                     "Longitude","Altitude","Timezone","DST","TzDB","Type","Source")


planes <- read.delim("data/OpenFlights/planes.dat", header = FALSE,
                     sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
names(planes) <- c("Name","IATA","ICAO")

# Fix missing ID
routes$OriginID[routes$Origin == "HYD"] <- "12087"
routes$DestID[routes$Dest == "HYD"] <- "12087"

airports_uk <- airports[airports$Country == "United Kingdom",]
routes_uk <- routes[routes$OriginID %in% airports_uk$AirportID | 
                      routes$DestID %in% airports_uk$AirportID,]

routes_uk$OriginID <- as.integer(routes_uk$OriginID)
routes_uk$DestID <- as.integer(routes_uk$DestID)
routes_uk$AirlineID <- as.integer(routes_uk$AirlineID)

airlines_uk <- airlines[airlines$AirlineID %in% routes_uk$AirlineID, ]
airlines_uk <- airlines_uk[,c("AirlineID","Name")]
names(airlines_uk) <- c("AirlineID","AirlineName")


routes_uk <-  left_join(routes_uk, airlines_uk, by = "AirlineID")

airport_match <- airports[,c("AirportID","Name","Latitude","Longitude")]
names(airport_match) <- c("AirportID","Airport_o","Latitude_o","Longitude_o")
routes_uk <-  left_join(routes_uk, airport_match, by = c("OriginID" = "AirportID"))
names(airport_match) <- c("AirportID","Airport_d","Latitude_d","Longitude_d")
routes_uk <-  left_join(routes_uk, airport_match, by = c("DestID" = "AirportID"))
routes_uk$Equipment <- substr(routes_uk$Equipment, 1, 3) # Take the first of mulitple types

planes <- planes[!duplicated(planes$IATA), ] # Some types share ID codes remove
planes <- planes[,c("Name","IATA")]
names(planes) <- c("Aircraft","IATA")

#Add some missing Data
planes_missing <- data.frame(Aircraft = c("32B", "73N", "32S", "32A", "ARJ", "L4T", "73W", "CRJ", "EMJ", "73C", "76W", "75W", "73H"),
                             IATA = c("Airbus A321", "Boeing 737-300", "Airbus A318", "Airbus A320", "Avro RJ70", "LET 410", "Boeing 737-700", "Canadair Regional Jet", "Embraer Jet 190", "Boeing 737-300", "Boeing 767-300ER", "Boeing 757-200", "Boeing 737-800"),
                             stringsAsFactors = FALSE)
planes <- rbind(planes, planes_missing)


routes_uk <-  left_join(routes_uk, planes, by = c("Equipment" = "IATA"))

saveRDS(routes_uk,"data/OpenFlights/UKflights.Rds")

fuel <- readRDS("data/Fuel Consumption - Wikipedia.Rds")
fuel <- fuel[!duplicated(fuel$Model),]

foo <- as.data.frame(table(routes_uk$Aircraft))
foo <- left_join(foo, fuel, by = c("Var1" = "Model"))
