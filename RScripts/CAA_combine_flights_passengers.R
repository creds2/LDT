library(dplyr)
library(sf)
library(ggmap)
library(tmap)
tmap_mode("view")
# Read in

#pass_transit <- readRDS("data/CAA_transit.Rds")
pass_int_od <- readRDS("data/CAA_int_od.Rds")
pass_dom_od <- readRDS("data/CAA_dom_od.Rds")

#airports <- readRDS("airports_final.Rds")
airports2 <- readRDS("airports.Rds")
#flights_flow <- readRDS("flow_final.Rds")

# Remove totals
pass_int_od <- pass_int_od[!grepl("Total",pass_int_od$uk_airport),]

# get the locations of the passenger airports
pass_int_od$foreign_airport <- tolower(pass_int_od$foreign_airport)
pass_int_od$foreign_country <- tolower(pass_int_od$foreign_country)

pass_int_od$foreign_airport <- gsub(" airport","",pass_int_od$foreign_airport)

# fix typo differences
pass_int_od$foreign_country[pass_int_od$foreign_airport == "aarhus"] <- "denmark"
pass_int_od$foreign_country[pass_int_od$foreign_country == "rumania"] <- "romania"
pass_int_od$foreign_country[pass_int_od$foreign_country == "fed rep yugo serbia m'enegro"] <- "republic of serbia"

pass_int_od$foreign_country[pass_int_od$foreign_airport == "podgorica"] <- "republic of montenegro"
pass_int_od$foreign_country[pass_int_od$foreign_airport == "pristina"] <- "kosovo"
pass_int_od$foreign_country[pass_int_od$foreign_airport == "podgorica"] <- "republic of montenegro"
pass_int_od$foreign_country[pass_int_od$foreign_airport == "tivat"] <- "republic of montenegro"
pass_int_od$foreign_country[pass_int_od$foreign_airport == "kosovo"] <- "kosovo"
pass_int_od$foreign_country[pass_int_od$foreign_airport == "republic of montenegro"] <- "republic of montenegro"
pass_int_od$foreign_country[pass_int_od$foreign_airport == "taipei"] <- "taiwan"

pass_int_od$foreign_airport[is.na(pass_int_od$foreign_airport)] <- "="

pass_int_od$foreign_airport[pass_int_od$foreign_airport == "ireland west knock"] <- "ireland west(knock)"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "cologne (bonn)"] <- "cologne bonn"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "kavalla"] <- "kavala"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "gerona"] <- "girona"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "la coruna"] <- "a coruna"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "basle mulhouse"] <- "bale mulhouse" #### should be basle mulhouse
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "izmir (adnam menderes)"] <- "izmir (adnan menderes)"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "izmir (adnam menderes)"] <- "International Airport Kharkiv"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "hong kong (chep lap kok)"] <- "hong kong (chek lap kok)"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "szczecin (golenow)"] <- "Port Lotniczy Szczecin-Goleniow"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "kerry coun2001"] <- "kerry county"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "santiago de compostela"] <- "Santiago de Compostela"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "baden baden"] <- "karlsruhe/baden baden"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "villafranca"] <- "verona villafranca"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "st moritz"] <- "samedan/st moritz"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "kishinev"] <- "chisinau (kishinev)"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "imam khomieni"] <- "tehran imam khomeini"

#fix missing countires
pass_int_od$foreign_country[is.na(pass_int_od$foreign_country) & pass_int_od$foreign_airport %in% c("innsbruck","salzburg","vienna")] <- "austria"
pass_int_od$foreign_country[is.na(pass_int_od$foreign_country) & pass_int_od$foreign_airport %in% c("antwerp","brussels")] <- "belgium"
pass_int_od$foreign_country[is.na(pass_int_od$foreign_country) & pass_int_od$foreign_airport %in% c("copenhagen","billund","esbjerg","aarhus (tirstrup)")] <- "denmark"

pass_int_od$foreign_country[pass_int_od$foreign_country == "usa"] <- "United States of America"
pass_airport_int <- unique(pass_int_od[,c("foreign_airport","foreign_country")])

foo <- pass_airport_int$foreign_airport[duplicated(pass_airport_int$foreign_airport)]
foo <- foo[foo != "="]
if(length(foo) > 0){
  stop("Duplicated airport names")
  foo  <- pass_airport_int[pass_airport_int$foreign_airport %in% foo,]
}

pass_airport_int$full <- paste0(pass_airport_int$foreign_airport," airport, ",pass_airport_int$foreign_country)

match1 <- left_join(pass_airport_int, airports2, by = "full")
match_succ <- match1[!is.na(match1$origin_destination), ]
match_fail <- match1[is.na(match1$origin_destination), ]

# remove the unknown airports "="
match_fail <- match_fail[match_fail$foreign_airport != "=",]

# try a fuzzy text match
match_fail$approx_match <- sapply(match_fail$full, function(pattern){
  agrep(pattern, airports2$full, ignore.case = TRUE, value = TRUE,
        max.distance = 0.1)
  
})

match_fuzzy <- match_fail[lengths(match_fail$approx_match) > 0,]
match_fuzzy <- match_fuzzy[,c("foreign_airport","foreign_country", "approx_match")]
match_fuzzy$approx_match <- sapply(match_fuzzy$approx_match, function(x){
  gsub("airport","",x)
})

# try google geocode
if(FALSE){ # don't run by accident it costs money
  register_google(key = Sys.getenv("GOOGLE")) # Get Google Key
  airports_missing <- match_fail$full
  coords_google <- ggmap::geocode(airports_missing)
  airports_missing <- cbind(airports_missing, coords_google)
  saveRDS(airports_missing, "data/CAA_passengers_airports_google.Rds")
}else{
  airports_missing <- readRDS("data/CAA_passengers_airports_google.Rds")
  airports_missing$airports_missing <- as.character(airports_missing$airports_missing)
}

airports_missing$airports_missing[airports_missing$airports_missing == "aarhus airport, NA"] = "aarhus airport, denmark"
# check it is good
ap <- st_as_sf(airports_missing[!is.na(airports_missing$lon),], coords = c("lon","lat"), crs = 4326)
qtm(ap)

match_fail <- match_fail[,c("foreign_airport","foreign_country","full")]
match_fail <- left_join(match_fail, airports_missing, by = c("full" = "airports_missing"))

match_final <- bind_rows(match_succ, match_fail)
match_final$lon[match_final$foreign_airport == "nimes"] <- match_final$`1`[match_final$foreign_airport == "nimes"]
match_final$lat[match_final$foreign_airport == "nimes"] <- match_final$`2`[match_final$foreign_airport == "nimes"]

match_final <- match_final[,c("foreign_airport","foreign_country","full","lon","lat")]

match_final$lon[match_final$foreign_airport == "al-udeid usafb"] <- 51.314415
match_final$lat[match_final$foreign_airport == "al-udeid usafb"] <- 25.117309

match_final$lon[match_final$foreign_airport == "rochester municipal"] <- -92.495464
match_final$lat[match_final$foreign_airport == "rochester municipal"] <- 43.909981



match_final_sf <- st_as_sf(match_final[!is.na(match_final$lon),], coords = c("lon","lat"), crs = 4326)
qtm(match_final_sf)

write_sf(match_final_sf,"data/airports_pass.gpkg")

# TODO: join on UK airports
airports_dom <- unique(c(pass_dom_od$airport1, pass_dom_od$airport2))
