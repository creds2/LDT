# Open Flights

routes <- read.delim("data/OpenFlights/routes.dat", header = FALSE, sep = ",")
names(routes) <- c("Airline","AirlineID","Origin","OriginID","Dest","DestID",
                   "Codeshare","Stops","Equipment")

airlines <- read.delim("data/OpenFlights/airlines.dat", header = FALSE, sep = ",")
names(airlines) <- c("AirlineID","Name","Alias","IATA","ICAO","Callsign",
                   "Country","Active")

airports <- read.delim("data/OpenFlights/airports.dat", header = FALSE, sep = ",")
names(airports) <- c("AirportID","Name","City","Country","IATA","ICAO","Latitude",
                     "Longitude","Altitude","Timezone","DST","TzDB","Type","Source")
