# batch import
library(dplyr)
library(tidyr)
library(stringdist)
library(ggmap)
library(RANN)
library(sf)
library(tmap)
tmap_mode("view")

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

# Correct Wrong Locations
airports_CAA$geometry[airports_CAA$name == "Sydney Canada Airport, Canada"] <-
  st_point(c(airports$Longitude[airports$fullname == "Sydney / J.A. Douglas McCurdy Airport, Canada"],
             airports$Latitude[airports$fullname == "Sydney / J.A. Douglas McCurdy Airport, Canada"] ))

airports_CAA$geometry[airports_CAA$name == "Houston Airport, United States"] <-
  st_point(c(airports$Longitude[airports$fullname == "George Bush Intercontinental Houston Airport, United States"],
             airports$Latitude[airports$fullname == "George Bush Intercontinental Houston Airport, United States"] ))

airports_CAA$geometry[airports_CAA$name == "Wichita Airport, United States"] <-
  st_point(c(airports$Longitude[airports$fullname == "Wichita Eisenhower National Airport, United States"],
             airports$Latitude[airports$fullname == "Wichita Eisenhower National Airport, United States"] ))

airports_CAA$geometry[airports_CAA$name == "Denver Airport, United States"] <-
  st_point(c(airports$Longitude[airports$fullname == "Denver International Airport, United States"],
             airports$Latitude[airports$fullname == "Denver International Airport, United States"] ))

airports_CAA$geometry[airports_CAA$name == "Phoenix Airport, United States"] <-
  st_point(c(airports$Longitude[airports$fullname == "Phoenix Sky Harbor International Airport, United States"],
             airports$Latitude[airports$fullname == "Phoenix Sky Harbor International Airport, United States"] ))

airports_CAA$geometry[airports_CAA$name == "Grenada Airport, Grenada"] <-
  st_point(c(airports$Longitude[airports$fullname == "Point Salines International Airport, Grenada"],
             airports$Latitude[airports$fullname == "Point Salines International Airport, Grenada"] ))

airports_CAA$geometry[airports_CAA$name == "Las Palmas Airport, Spain(Canary Islands)"] <-
  st_point(c(airports$Longitude[airports$fullname == "Gran Canaria Airport, Spain"],
             airports$Latitude[airports$fullname == "Gran Canaria Airport, Spain"] ))

airports_CAA$geometry[airports_CAA$name == "Chateauroux Airport, France"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Châteauroux-Déols "Marcel Dassault" Airport, France'],
             airports$Latitude[airports$fullname == 'Châteauroux-Déols "Marcel Dassault" Airport, France'] ))

airports_CAA$geometry[airports_CAA$name == "Schwerin/Parchim Airport, Germany"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Schwerin Parchim Airport, Germany'],
             airports$Latitude[airports$fullname == 'Schwerin Parchim Airport, Germany'] ))

airports$Longitude[airports$fullname == 'Oslo Lufthavn, Norway'] <- 
  airports_CAA$geometry[airports_CAA$name == "Oslo (Fornebu) Airport, Norway"][[1]][1]
airports$Latitude[airports$fullname == 'Oslo Lufthavn, Norway'] <- 
  airports_CAA$geometry[airports_CAA$name == "Oslo (Fornebu) Airport, Norway"][[1]][2]

airports_CAA$geometry[airports_CAA$name == "Geilo (Dagali) Airport, Norway"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Geilo Airport Dagali, Norway'],
             airports$Latitude[airports$fullname == 'Geilo Airport Dagali, Norway'] ))

airports_CAA$geometry[airports_CAA$name == "Nis Airport, Fed Rep Yugo Serbia M'enegro"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Nis Airport, Serbia'],
             airports$Latitude[airports$fullname == 'Nis Airport, Serbia'] ))

airports_CAA$geometry[airports_CAA$name == "Amman (King Hussein) Airport, Jordan"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Amman-Marka International Airport, Jordan'],
             airports$Latitude[airports$fullname == 'Amman-Marka International Airport, Jordan'] ))

airports_CAA$geometry[airports_CAA$name == "Kassala Airport, Sudan"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Kassala Airport, Sudan'],
             airports$Latitude[airports$fullname == 'Kassala Airport, Sudan'] ))

airports_CAA$geometry[airports_CAA$name == "Dubai (World Central) Airport, United Arab Emirates"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Al Maktoum International Airport, United Arab Emirates'],
             airports$Latitude[airports$fullname == 'Al Maktoum International Airport, United Arab Emirates'] ))

airports_CAA$geometry[airports_CAA$name == "Taipei Airport, Nationalist China (Taiwan)"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Taipei Songshan Airport, Taiwan'],
             airports$Latitude[airports$fullname == 'Taipei Songshan Airport, Taiwan'] ))

airports_CAA$geometry[airports_CAA$name == "Krasnodar Airport, Russia"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Krasnodar Pashkovsky International Airport, Russia'],
             airports$Latitude[airports$fullname == 'Krasnodar Pashkovsky International Airport, Russia'] ))

airports_CAA$geometry[airports_CAA$name == "Simferopol Airport, Ukraine"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Simferopol International Airport, Ukraine'],
             airports$Latitude[airports$fullname == 'Simferopol International Airport, Ukraine'] ))

airports_CAA$geometry[airports_CAA$name == "Kharnia Airport, Greece"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Chania International Airport, Greece'],
             airports$Latitude[airports$fullname == 'Chania International Airport, Greece'] ))






# Match to nearest openflights airport
ap_CAA <- as.data.frame(st_coordinates(airports_CAA))
ap_CAA$nameCAA <- airports_CAA$name
ap_of <- airports[,c("fullname","Longitude","Latitude")]
nn_res <- nn2(ap_of[,c("Longitude","Latitude")], ap_CAA[,c("X","Y")] , k = 1 )
ap_CAA$nameOF <- ap_of$fullname[as.numeric(nn_res$nn.idx)]
ap_CAA$dist <- as.numeric(nn_res$nn.dists)

# Plot to compare
ap_of <- st_as_sf(ap_of, coords = c("Longitude","Latitude"), crs = 4326)
ap_CAA <- st_as_sf(ap_CAA, coords = c("X","Y"), crs = 4326)
tm_shape(ap_of) +
  tm_dots(col = "black") +
  tm_shape(ap_CAA[ap_CAA$dist > 0.031,]) +
  tm_dots(col = "red")

# Fix Duplicated Airports
airports_CAA$geometry[airports_CAA$name == "Chicago (Du Page) Airport, United States"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Dupage Airport, United States'],
             airports$Latitude[airports$fullname == 'Dupage Airport, United States'] ))

airports_CAA$geometry[airports_CAA$name == "Pontiac Airport, United States"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Oakland County International Airport, United States'],
             airports$Latitude[airports$fullname == 'Oakland County International Airport, United States'] ))

airports_CAA$geometry[airports_CAA$name == "Ciego De Avila Airport, Cuba"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Maximo Gomez Airport, Cuba'],
             airports$Latitude[airports$fullname == 'Maximo Gomez Airport, Cuba'] ))

airports_CAA$geometry[airports_CAA$name == "Aruba Airport, Isle of Curacao Neth.antilles"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Queen Beatrix International Airport, Aruba'],
             airports$Latitude[airports$fullname == 'Queen Beatrix International Airport, Aruba'] ))

airports_CAA$geometry[airports_CAA$name == "San Pedro (Cape Verde) Airport, Cape Verde Islands"] <-
  st_point(c(airports$Longitude[airports$fullname == 'São Pedro Airport, Cape Verde'],
             airports$Latitude[airports$fullname == 'São Pedro Airport, Cape Verde'] ))

airports_CAA$geometry[airports_CAA$name == "Reykjavik Airport, Iceland"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Reykjavik Airport, Iceland'],
             airports$Latitude[airports$fullname == 'Reykjavik Airport, Iceland'] ))

airports_CAA$geometry[airports_CAA$name == "Oporto (Portugal) Airport, Portugal(Excluding Madeira)"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Francisco de Sá Carneiro Airport, Portugal'],
             airports$Latitude[airports$fullname == 'Francisco de Sá Carneiro Airport, Portugal'] ))

airports_CAA$geometry[airports_CAA$name == "Monte Real Airport, Portugal(Excluding Madeira)"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Monte Real Air Base, Portugal'],
             airports$Latitude[airports$fullname == 'Monte Real Air Base, Portugal'] ))

airports_CAA$geometry[airports_CAA$name == "Lisbon Airport, Portugal(Excluding Madeira)"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Humberto Delgado Airport (Lisbon Portela Airport), Portugal'],
             airports$Latitude[airports$fullname == 'Humberto Delgado Airport (Lisbon Portela Airport), Portugal'] ))

airports_CAA$geometry[airports_CAA$name == "Cascais Airport, Portugal(Excluding Madeira)"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Cascais Airport, Portugal'],
             airports$Latitude[airports$fullname == 'Cascais Airport, Portugal'] ))

airports_CAA$geometry[airports_CAA$name == "Azores Santa Maria Airport, Portugal(Excluding Madeira)"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Santa Maria Airport, Portugal'],
             airports$Latitude[airports$fullname == 'Santa Maria Airport, Portugal'] ))

airports_CAA$geometry[airports_CAA$name == "Azores Horta Airport, Portugal(Excluding Madeira)"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Horta Airport, Portugal'],
             airports$Latitude[airports$fullname == 'Horta Airport, Portugal'] ))

airports_CAA$geometry[airports_CAA$name == "Casablanca Anfa Airport, Morocco"] <-
  st_point(c(-7.660556,33.556944))

airports_CAA$geometry[airports_CAA$name == "Kinloss Airport, United Kingdom"] <-
  st_point(c(airports$Longitude[airports$fullname == 'RAF Kinloss, United Kingdom'],
             airports$Latitude[airports$fullname == 'RAF Kinloss, United Kingdom'] ))

airports_CAA$geometry[airports_CAA$name == "Forres Airport, United Kingdom"] <-
  st_point(c(airports$Longitude[airports$fullname == 'RAF Kinloss, United Kingdom'],
             airports$Latitude[airports$fullname == 'RAF Kinloss, United Kingdom'] ))

airports_CAA$geometry[airports_CAA$name == "Leuchars Airport, United Kingdom"] <-
  st_point(c(airports$Longitude[airports$fullname == 'RAF Leuchars, United Kingdom'],
             airports$Latitude[airports$fullname == 'RAF Leuchars, United Kingdom'] ))

airports_CAA$geometry[airports_CAA$name == "Unst Airport, United Kingdom"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Unst Airport, United Kingdom'],
             airports$Latitude[airports$fullname == 'Unst Airport, United Kingdom'] ))

airports_CAA$geometry[airports_CAA$name == "Filton Airport, United Kingdom"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Bristol Filton Airport, United Kingdom'],
             airports$Latitude[airports$fullname == 'Bristol Filton Airport, United Kingdom'] ))

airports_CAA$geometry[airports_CAA$name == "Haydock Park Airport, United Kingdom"] <-
  st_point(c(-2.6243584,53.4797728))

airports_CAA$geometry[airports_CAA$name == "Coal Aston Airport, United Kingdom"] <-
  st_point(c(-1.430556,53.304722))

airports_CAA$geometry[airports_CAA$name == "St Nazaire Airport, France"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Saint-Nazaire-Montoir Airport, France'],
             airports$Latitude[airports$fullname == 'Saint-Nazaire-Montoir Airport, France'] ))

airports_CAA$geometry[airports_CAA$name == "Coal Aston Airport, United Kingdom"] <-
  st_point(c(-0.250833,51.765833))

airports_CAA$geometry[airports_CAA$name == "Villacoublay Airport, France"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Villacoublay-Vélizy (BA 107) Air Base, France'],
             airports$Latitude[airports$fullname == 'Villacoublay-Vélizy (BA 107) Air Base, France'] ))

airports_CAA$geometry[airports_CAA$name == "Reims Airport, France"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Reims-Prunay Airport, France'],
             airports$Latitude[airports$fullname == 'Reims-Prunay Airport, France'] ))

airports_CAA$geometry[airports_CAA$name == "Pontoise Airport, France"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Pontoise - Cormeilles-en-Vexin Airport, France'],
             airports$Latitude[airports$fullname == 'Pontoise - Cormeilles-en-Vexin Airport, France'] ))

airports_CAA$geometry[airports_CAA$name == "Guyancourt Airport, France"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Toussus-le-Noble Airport, France'],
             airports$Latitude[airports$fullname == 'Toussus-le-Noble Airport, France'] ))

airports_CAA$geometry[airports_CAA$name == "Amiens Airport, France"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Amiens-Glisy Airport, France'],
             airports$Latitude[airports$fullname == 'Amiens-Glisy Airport, France'] ))

airports_CAA$geometry[airports_CAA$name == "Ouargla Airport, Algeria"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Ain el Beida Airport, Algeria'],
             airports$Latitude[airports$fullname == 'Ain el Beida Airport, Algeria'] ))

airports_CAA$geometry[airports_CAA$name == "In Salah Airport, Algeria"] <-
  st_point(c(airports$Longitude[airports$fullname == 'In Salah Airport, Algeria'],
             airports$Latitude[airports$fullname == 'In Salah Airport, Algeria'] ))

airports_CAA$geometry[airports_CAA$name == "Volkel Airport, Netherlands"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Volkel Air Base, Netherlands'],
             airports$Latitude[airports$fullname == 'Volkel Air Base, Netherlands'] ))

airports_CAA$geometry[airports_CAA$name == "Wildenrath Airport, Germany"] <-
  st_point(c(6.221667,51.114444))

airports_CAA$geometry[airports_CAA$name == "Bruggen Airport, Germany"] <-
  st_point(c(6.129444,51.2))

airports_CAA$geometry[airports_CAA$name == "Moenchengladbach Airport, Germany"] <-
  st_point(c(airports$Longitude[airports$fullname == 'Mönchengladbach Airport, Germany'],
             airports$Latitude[airports$fullname == 'Mönchengladbach Airport, Germany'] ))

airports_CAA$geometry[airports_CAA$name == "Frejus Airport, France"] <-
  st_point(c(6.735556,43.417222))



# Exclude any poor matches
ap_CAA$nameOF[ap_CAA$dist > 0.031] <- NA
ap_CAA$dist[ap_CAA$dist > 0.031] <- NA

#Check for duplicated locations
summary(duplicated(ap_CAA$geometry))
dup <- ap_CAA[ap_CAA$geometry %in% ap_CAA$geometry[duplicated(ap_CAA$geometry)],]

# Fix Matches to Duplicate Airports
tm_shape(ap_of) +
  tm_dots(col = "black") +
  tm_shape(dup) +
  tm_dots(col = "red")



# Add from and to locations to flow
ap_from <- ap_CAA
names(ap_from) <- c("fromAirport","fromAirportOF","dist","geometry_from")
ap_from$dist <- NULL
flow <- left_join(flow, ap_from)
