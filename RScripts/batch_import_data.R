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
  summarise(total_flights = sum(total_flights, na.rm = TRUE)) %>%
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
airports_CAA$name <- as.character(airports_CAA$name)

# Correct Wrong Locations
tidy1 <- function(x,y){
  airports_CAA$geometry[airports_CAA$name == x] <-
    st_point(c(airports$Longitude[airports$fullname == y],
               airports$Latitude[airports$fullname == y] ))
  assign('airports_CAA',airports_CAA,envir=.GlobalEnv)
  
}
tidy1("Sydney Canada Airport, Canada","Sydney / J.A. Douglas McCurdy Airport, Canada")
tidy1("Houston Airport, United States","George Bush Intercontinental Houston Airport, United States")
tidy1("Wichita Airport, United States","Wichita Eisenhower National Airport, United States")
tidy1("Denver Airport, United States","Denver International Airport, United States")
tidy1("Phoenix Airport, United States","Phoenix Sky Harbor International Airport, United States")
tidy1("Grenada Airport, Grenada","Point Salines International Airport, Grenada")
tidy1("Las Palmas Airport, Spain(Canary Islands)","Gran Canaria Airport, Spain")
tidy1("Chateauroux Airport, France",'Châteauroux-Déols "Marcel Dassault" Airport, France')
tidy1("Schwerin/Parchim Airport, Germany",'Schwerin Parchim Airport, Germany')

airports$Longitude[airports$fullname == 'Oslo Lufthavn, Norway'] <- 
  airports_CAA$geometry[airports_CAA$name == "Oslo (Fornebu) Airport, Norway"][[1]][1]
airports$Latitude[airports$fullname == 'Oslo Lufthavn, Norway'] <- 
  airports_CAA$geometry[airports_CAA$name == "Oslo (Fornebu) Airport, Norway"][[1]][2]

tidy1("Geilo (Dagali) Airport, Norway",'Geilo Airport Dagali, Norway')
tidy1("Nis Airport, Fed Rep Yugo Serbia M'enegro",'Nis Airport, Serbia')
tidy1("Amman (King Hussein) Airport, Jordan",'Amman-Marka International Airport, Jordan')
tidy1("Kassala Airport, Sudan",'Kassala Airport, Sudan')
tidy1("Dubai (World Central) Airport, United Arab Emirates",'Al Maktoum International Airport, United Arab Emirates')
tidy1("Taipei Airport, Nationalist China (Taiwan)",'Taipei Songshan Airport, Taiwan')
tidy1("Krasnodar Airport, Russia",'Krasnodar Pashkovsky International Airport, Russia')
tidy1("Simferopol Airport, Ukraine",'Simferopol International Airport, Ukraine')
tidy1("Kharnia Airport, Greece",'Chania International Airport, Greece')

# Fix Duplicated Airports
tidy1("Chicago (Du Page) Airport, United States",'Dupage Airport, United States')
tidy1("Pontiac Airport, United States",'Oakland County International Airport, United States')
tidy1("Ciego De Avila Airport, Cuba",'Maximo Gomez Airport, Cuba')
tidy1("Aruba Airport, Isle of Curacao Neth.antilles",'Queen Beatrix International Airport, Aruba')
tidy1("San Pedro (Cape Verde) Airport, Cape Verde Islands",'São Pedro Airport, Cape Verde')
tidy1("Reykjavik Airport, Iceland",'Reykjavik Airport, Iceland')
tidy1("Oporto (Portugal) Airport, Portugal(Excluding Madeira)",'Francisco de Sá Carneiro Airport, Portugal')
tidy1("Monte Real Airport, Portugal(Excluding Madeira)",'Monte Real Air Base, Portugal')
tidy1("Lisbon Airport, Portugal(Excluding Madeira)",'Humberto Delgado Airport (Lisbon Portela Airport), Portugal')
tidy1("Cascais Airport, Portugal(Excluding Madeira)",'Cascais Airport, Portugal')
tidy1("Azores Santa Maria Airport, Portugal(Excluding Madeira)",'Santa Maria Airport, Portugal')
tidy1("Azores Horta Airport, Portugal(Excluding Madeira)",'Horta Airport, Portugal')

airports_CAA$geometry[airports_CAA$name == "Casablanca Anfa Airport, Morocco"] <-
  st_point(c(-7.660556,33.556944))

tidy1("Kinloss Airport, United Kingdom",'RAF Kinloss, United Kingdom')
tidy1("Forres Airport, United Kingdom",'RAF Kinloss, United Kingdom')
tidy1("Leuchars Airport, United Kingdom",'RAF Leuchars, United Kingdom')
tidy1("Unst Airport, United Kingdom",'Unst Airport, United Kingdom')
tidy1("Filton Airport, United Kingdom",'Bristol Filton Airport, United Kingdom')

airports_CAA$geometry[airports_CAA$name == "Haydock Park Airport, United Kingdom"] <-
  st_point(c(-2.6243584,53.4797728))

airports_CAA$geometry[airports_CAA$name == "Coal Aston Airport, United Kingdom"] <-
  st_point(c(-1.430556,53.304722))

tidy1("St Nazaire Airport, France",'Saint-Nazaire-Montoir Airport, France')

airports_CAA$geometry[airports_CAA$name == "Coal Aston Airport, United Kingdom"] <-
  st_point(c(-0.250833,51.765833))

tidy1("Villacoublay Airport, France",'Villacoublay-Vélizy (BA 107) Air Base, France')
tidy1("Reims Airport, France",'Reims-Prunay Airport, France')
tidy1("Pontoise Airport, France",'Pontoise - Cormeilles-en-Vexin Airport, France')
tidy1("Guyancourt Airport, France",'Toussus-le-Noble Airport, France')
tidy1("Amiens Airport, France",'Amiens-Glisy Airport, France')
tidy1("Ouargla Airport, Algeria",'Ain el Beida Airport, Algeria')
tidy1("In Salah Airport, Algeria",'In Salah Airport, Algeria')
tidy1("Volkel Airport, Netherlands",'Volkel Air Base, Netherlands')

airports_CAA$geometry[airports_CAA$name == "Wildenrath Airport, Germany"] <-
  st_point(c(6.221667,51.114444))

airports_CAA$geometry[airports_CAA$name == "Bruggen Airport, Germany"] <-
  st_point(c(6.129444,51.2))

tidy1("Moenchengladbach Airport, Germany",'Mönchengladbach Airport, Germany')

airports_CAA$geometry[airports_CAA$name == "Frejus Airport, France"] <-
  st_point(c(6.735556,43.417222))

tidy1("Koblenz Airport, Germany",'Koblenz-Winningen Airfield, Germany')
airports_CAA$geometry[airports_CAA$name == "St Moritz Airport, Switzerland"] <-
  st_point(c(9.883889,46.533889))
tidy1("Port Gentil Airport, Gabon",'Port Gentil Airport, Gabon')
tidy1("Oslo (Fornebu) Airport, Norway",'Oslo, Fornebu Airport, Norway')

airports_CAA$geometry[airports_CAA$name == "Ingolstadt Airport, Germany"] <-
  st_point(c(11.533889,48.715556))
airports_CAA$geometry[airports_CAA$name == "Satenas Airport, Sweden"] <-
  st_point(c(12.7144003,58.4263992))

tidy1("Sirte/Surt Airport, Libya",'Gardabya Airport, Libya')
tidy1("Sebha Airport, Libya",'Sabha Airport, Libya')
tidy1("Asmara Airport, Ethiopia",'Asmara International Airport, Eritrea')
tidy1("Elat Airport, Israel",'Eilat Airport, Israel')

airports_CAA$geometry[airports_CAA$name == "Gatow Airport, Germany"] <-
  st_point(c(13.138056,52.474444))

tidy1("Mora Airport, Sweden",'Mora Airport, Sweden')
tidy1("Akrotiri Airport, Cyprus",'RAF Akrotiri, Cyprus')
airports_CAA$geometry[airports_CAA$name == "Hatfield Airport, United Kingdom"] <-
  st_point(c(-0.250833,51.765833))
tidy1("Cannes Airport, France",'Cannes-Mandelieu Airport, France')
tidy1("Pretoria Airport, Republic of South Africa",'Wonderboom Airport, South Africa')
tidy1("Osaka (Kansai) Airport, Japan",'Kansai International Airport, Japan')



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
# tm_shape(ap_of) +
#   tm_dots(col = "black") +
#   tm_shape(ap_CAA[ap_CAA$dist > 0.031,]) +
#   tm_dots(col = "red")

# Exclude any poor matches
ap_CAA$nameOF[ap_CAA$dist > 0.031] <- NA
ap_CAA$dist[ap_CAA$dist > 0.031] <- NA

#Check for duplicated locations
summary(duplicated(ap_CAA$geometry))
dup <- ap_CAA[ap_CAA$geometry %in% ap_CAA$geometry[duplicated(ap_CAA$geometry)],]

# Fix Matches to Duplicate Airports
# tm_shape(ap_of) +
#   tm_dots(col = "black") +
#   tm_shape(dup) +
#   tm_dots(col = "red")

# Now Fixed som make master list of airports and flows
ap_join <- ap_CAA
ap_join <- st_drop_geometry(ap_join)
ap_join <- ap_join[,c("nameCAA","nameOF")]
ap_join$nameOF <- ifelse(is.na(ap_join$nameOF), ap_join$nameCAA, ap_join$nameOF)

names(ap_join) <- c("fromAirport","fromAirportOF")
flow <- left_join(flow, ap_join)
names(ap_join) <- c("toAirport","toAirportOF")
flow <- left_join(flow, ap_join)

# Remove the small number of unknown flights
flow <- flow[!is.na(flow$toAirportOF),]

#Combine flows for same airport but differnt names
flow <- flow[,c("fromAirportOF","toAirportOF","1991",as.character(1993:2018))]
flow <- flow %>%
  group_by(fromAirportOF, toAirportOF) %>%
  summarise_all(sum, na.rm = TRUE)

# Master Locations
airports_final <- ap_CAA
airports_final$nameOF <- ifelse(is.na(airports_final$nameOF), airports_final$nameCAA, airports_final$nameOF)
airports_final <- airports_final[,c("nameOF")]
airports_final <- airports_final[!duplicated(airports_final$nameOF),]
saveRDS(airports_final, "airports_final.Rds")

# Make Great Circle
# Add from an to Geometry
names(airports_final) <- c("fromAirportOF","geometry_from")
flow <- left_join(flow, airports_final)
names(airports_final) <- c("toAirportOF","geometry_to")
flow <- left_join(flow, airports_final)

line <- lapply(1:nrow(flow), function(i){
  st_cast(st_combine(x = c(flow$geometry_from[i], flow$geometry_to[i])), "LINESTRING")
})
line <- do.call(c, line)
line <- st_segmentize(line, units::set_units(5, km))
flow$geometry <- line
flow$geometry_from <- NULL
flow$geometry_to <- NULL

flow <- st_as_sf(flow, crs = 4326)

flow$length_km <- round(as.numeric(st_length(flow)) / 1000, 1)
saveRDS(flow, "flow_final.Rds")



