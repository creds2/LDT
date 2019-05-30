# batch import
library(dplyr)
library(tidyr)
library(stringdist)
library(ggmap)
library(RANN)

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
  # Exclude flights leaving for the UK
  # Otherwise will be double counted
  flights <- flights[!(flights$origin_destination_country == "UNITED KINGDOM" & flights$arrival_departure == "D"),]
  
  
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


# Load Airport Locations
airports <- read.delim("data/OpenFlights/airports.dat", header = FALSE, 
                       sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
names(airports) <- c("AirportID","Name","City","Country","IATA","ICAO","Latitude",
                     "Longitude","Altitude","Timezone","DST","TzDB","Type","Source")
airports <- airports[,c("AirportID","Name","City","Country","IATA","ICAO","Latitude","Longitude")]

# Match Spellings
flow$origin_destination_country[flow$origin_destination_country == "USA"] <- "UNITED STATES"

# Match CAA to OpenFlights Names
flow$fromAirport <- paste0(tools::toTitleCase(tolower(flow$reporting_airport))," Airport, United Kingdom")
flow$toAirport <- paste0(tools::toTitleCase(tolower(flow$origin_destination))," Airport, ",tools::toTitleCase(tolower(flow$origin_destination_country)))

airports$fullname <- paste0(airports$Name,", ",airports$Country)

# geocode the CAA airports
# Don't do if aviaible as costs money
if(file.exists("airports_CAA.Rds")){
  airports_CAA <- readRDS("airports_CAA.Rds")
}else{
  airports_CAA <- unique(c(flow$toAirport, flow$fromAirport))
  register_google(key = Sys.getenv("GOOGLE")) # Get Google Key
  coords_google <- ggmap::geocode(airports_CAA)
  airports_CAA <- cbind(airports_CAA, coords_google)
  names(airports_CAA) = c("name","lon","lat")
  # fix missing
  airports_CAA$lon[airports_CAA$name == "Nimes Airport, France"] <- 4.416389
  airports_CAA$lat[airports_CAA$name == "Nimes Airport, France"] <- 43.7575
  
  airports_CAA$lon[airports_CAA$name == "Al-Udeid Usafb Airport, Qatar"] <- 51.318611
  airports_CAA$lat[airports_CAA$name == "Al-Udeid Usafb Airport, Qatar"] <- 25.118611
  
  airports_CAA$lon[airports_CAA$name == "Moret Airport, France"] <- 2.7993987
  airports_CAA$lat[airports_CAA$name == "Moret Airport, France"] <- 48.3419411
  
  airports_CAA$lon[airports_CAA$name == "Rochester Municipal Airport, United States"] <- -92.5
  airports_CAA$lat[airports_CAA$name == "Rochester Municipal Airport, United States"] <- 43.908333
  
  airports_CAA$lon[airports_CAA$name == "Rochester Municipal Airport Airport, United States"] <- -92.5
  airports_CAA$lat[airports_CAA$name == "Rochester Municipal Airport Airport, United States"] <- 43.908333
  
  # Can't Find except in CAA reference https://www.caa.co.uk/uploadedFiles/CAA/Content/Standard_Content/Data_and_analysis/Tools_and_help/CAA%20data%20and%20analysis%20codelist.pdf 
  airports_CAA$lon[airports_CAA$name == "Londtrop Airport, Denmark"] <- NA 
  airports_CAA$lat[airports_CAA$name == "Londtrop Airport, Denmark"] <- NA
  
  # No fixed location
  airports_CAA$lon[airports_CAA$name == "Private Strips/Helipads Airport, United Kingdom"] <- NA
  airports_CAA$lat[airports_CAA$name == "Private Strips/Helipads Airport, United Kingdom"] <- NA
  
  airports_CAA$lon[airports_CAA$name == "Unknown Airport, Unknown"] <- NA
  airports_CAA$lat[airports_CAA$name == "Unknown Airport, Unknown"] <- NA
  
  airports_CAA <- sf::st_as_sf(airports_CAA[!is.na(airports_CAA$lon),], coords = c("lon","lat"), crs = 4326)
  saveRDS(airports_CAA, "airports_CAA.Rds")
}


# Match to nearest openflights airport
ap_CAA <- as.data.frame(st_coordinates(airports_CAA))
ap_CAA$nameCAA <- airports_CAA$name
ap_of <- airports[,c("fullname","Longitude","Latitude")]
nn_res <- nn2(ap_of[,c("Longitude","Latitude")], ap_CAA[,c("X","Y")] , k = 1)
ap_CAA$nameOF <- ap_of$fullname[as.numeric(nn_res$nn.idx)]
ap_CAA$dist <- as.numeric(nn_res$nn.dists)

# flow$fromMatch <- sapply(flow$fromAirport, function(y){
#   agrep(pattern = y, x = airports$fullname, value = TRUE)
# }) 
# 
# foo2 <- flow[1:10,]
# foo2$fromMatch <- sapply(foo2$fromAirport, function(y){
#   agrep(pattern = y, x = airports$fullname, value = TRUE)
# }) 
# flow <- flow[!duplicated(flow$fromAirport),]
# 
# 
# flow$fromMatch <- airports$fullname[amatch(flow$fromAirport, table = airports$fullname, method = "osa", maxDist = Inf)]
# bar2 <- unique(flow[,c("fromAirport","fromMatch")])
