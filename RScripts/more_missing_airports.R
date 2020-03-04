head(airports_missing2) # produced from clean_passenger_data.R

airports_missing2$full <- paste0(airports_missing2$foreign_airport," airport, ", airports_missing2$foreign_country)

coords_google <- ggmap::geocode(airports_missing2$full)

airports_missing2 <- cbind(airports_missing2, coords_google)
saveRDS(airports_missing2, "data/airports_missing.Rds")
airports_misssf <- st_as_sf(airports_missing2[!is.na(airports_missing2$lon),], coords = c("lon","lat"), crs = 4326)
qtm(airports_misssf)
