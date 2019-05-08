library(dplyr)
library(stplanr)

# Flights OD Data

flights <- read.csv("D:/Users/earmmor/OneDrive - University of Leeds/CREDS Data/CAA/201801_Punctuality_Statistics_Full_Analysis_Arrival_Departure.csv",
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


# construct airport lookup list
airports_int <- flights[,c("origin_destination","origin_destination_country")]
airports_int <- unique(airports_int)
airports_int$origin_destination_orig <- airports_int$origin_destination
airports_int$origin_destination <- tolower(airports_int$origin_destination)
airports_int$origin_destination_country <- tolower(airports_int$origin_destination_country)

# fix typos
airports_int$origin_destination[airports_int$origin_destination == "wick john o groats"] <- "Wick"
airports_int$origin_destination[airports_int$origin_destination == "tenerife (surreina sofia)"] <- "Tenerife South"
airports_int$origin_destination[airports_int$origin_destination == "st pete (clearwater)"] <- "St. Petersburg–Clearwater International"
airports_int$origin_destination[airports_int$origin_destination == "new york (newark)"] <- "Newark Liberty International"
airports_int$origin_destination[airports_int$origin_destination == "newburgh/usa"] <- "New York Stewart International"
airports_int$origin_destination[airports_int$origin_destination == "ilha do sal c.verde"] <- "Amílcar Cabral International"           
airports_int$origin_destination[airports_int$origin_destination == "new york (jf kennedy)"] <- "John F. Kennedy International"
airports_int$origin_destination[airports_int$origin_destination == "oporto (portugal)"] <- "Francisco Sá Carneiro"
airports_int$origin_destination[airports_int$origin_destination == "lublin (port lotniczy)"  ] <- "Lublin"      
airports_int$origin_destination[airports_int$origin_destination == "city of derry (eglinton)"] <- "City of Derry"
airports_int$origin_destination[airports_int$origin_destination == "szczecin (golenow)"] <- "Port Lotniczy Szczecin-Goleniow"
airports_int$origin_destination[airports_int$origin_destination == "st lucia (hewanorra)"     ] <- "Hewanorra International"     
airports_int$origin_destination[airports_int$origin_destination == "port of spain"] <- "Piarco International"
airports_int$origin_destination[airports_int$origin_destination == "san jose cost rica"] <- "Juan Santamaria International"
airports_int$origin_destination[airports_int$origin_destination == "baku (heyder aliyev int'l)"] <- "Heydar Aliyev International"    
airports_int$origin_destination[airports_int$origin_destination == "baghdad (geca)"] <- "Baghdad International"
airports_int$origin_destination[airports_int$origin_destination == "sulaymaniyah int"] <- "Sulaymaniyah International"
airports_int$origin_destination[airports_int$origin_destination == "casablanca mohamed v"   ] <- "Casablanca Mohammed V International"       
airports_int$origin_destination[airports_int$origin_destination == "tangiers (ibn batuta)"] <- "Aeroport Tanger Ibn Battouta"
airports_int$origin_destination[airports_int$origin_destination == "minneapolis-st paul"] <- "Minneapolis−Saint Paul International"
airports_int$origin_destination[airports_int$origin_destination == "santiago de compostela (spain)"] <- "Santiago de Compostela"
airports_int$origin_destination[airports_int$origin_destination == "santa cruz de la palma"] <- "La Palma"
airports_int$origin_destination[airports_int$origin_destination == "grand cayman"] <- "Owen Roberts International"
airports_int$origin_destination[airports_int$origin_destination == "jakarta (soekarno-hatta intnl)"] <- "Soekarno-Hatta International"
airports_int$origin_destination[airports_int$origin_destination == "kuala lumpur (sepang)"] <- "Kuala Lumpur International"
airports_int$origin_destination[airports_int$origin_destination == "hyderabad ( rajiv ghandi )"] <- "Rajiv Gandhi International"
airports_int$origin_destination[airports_int$origin_destination == "kharkov osnova intl"] <- "International Airport Kharkiv"
airports_int$origin_destination[airports_int$origin_destination == "tarbes-lourdes international"] <- "Tarbes–Lourdes–Pyrénées"
airports_int$origin_destination[airports_int$origin_destination == "lametia-terme"] <- "Lamezia Terme International"
airports_int$origin_destination[airports_int$origin_destination == "trieste (ronchi dei legionari)"] <- "Trieste"
airports_int$origin_destination[airports_int$origin_destination == "castellon de la plana airport"] <- "Castellón–Costa Azahar"
airports_int$origin_destination[airports_int$origin_destination == "cunagua (cayo coco)"] <- "Jardines del Rey"
airports_int$origin_destination[airports_int$origin_destination == "aberdeen"] <- "Aberdeen International"
airports_int$origin_destination[airports_int$origin_destination == "cardiff wales"] <- "Cardiff"
airports_int$origin_destination[airports_int$origin_destination == "alicante"] <- "Aeropuerto de Alicante-Elche"
airports_int$origin_destination[airports_int$origin_destination == "exeter"] <- "Exeter International"
airports_int$origin_destination[airports_int$origin_destination == "malta"] <- "Malta International"
airports_int$origin_destination[airports_int$origin_destination == "arrecife"] <- "Lanzarote"
airports_int$origin_destination[airports_int$origin_destination == "amsterdam"] <- "Luchthaven Schiphol"
airports_int$origin_destination[airports_int$origin_destination == "dhakha"] <- "dhakha"
airports_int$origin_destination[airports_int$origin_destination == "dhakha"] <- "male international"
airports_int$origin_destination[airports_int$origin_destination == "san jose"] <- "san jose"
airports_int$origin_destination[airports_int$origin_destination == "las vegas"] <- "McCarran International"
airports_int$origin_destination[airports_int$origin_destination == "biggin hill"] <- "biggin hill"
airports_int$origin_destination[airports_int$origin_destination == "phoenix"] <- "phoenix sky harbor international"
airports_int$origin_destination[airports_int$origin_destination == "lyon"] <- "lyon"

airports_int$origin_destination_country[airports_int$origin_destination_country == "usa"] <- "United States of America"


airports_int$full <- paste0(airports_int$origin_destination," airport, ",airports_int$origin_destination_country)


coords_google <- ggmap::geocode(airports_int$full)

# coords <- sapply(airports_int$full, geo_code)
# names(coords) <- NULL
# for(i in 1:length(coords)){
#   if(length(coords[[i]]) == 0){
#     coords[[i]] <- c(NA, NA)
#   }
# }
# 
# coords <- matrix(unlist(coords), ncol = 2, byrow = T)
#airports_int <- cbind(airports_int, coords)
airports_int <- cbind(airports_int, coords_google)
saveRDS(airports_int, "airports.Rds")
missing = airports_int[is.na(airports_int$lon),]
airports_int$lon[airports_int$origin_destination == "nimes"] <- 4.416389
airports_int$lat[airports_int$origin_destination == "nimes"] <- 43.7575
rm(airports_sf)
airports_sf <- sf::st_as_sf(airports_int, coords = c("lon","lat"), crs = 4326)
airports_sf$origin_destination <- toupper(airports_sf$origin_destination)
saveRDS(airports_int, "airports_sf.Rds")
flow_sf  <- od2line(flow, airports_sf[,"origin_destination"])
qtm(flow_sf)

