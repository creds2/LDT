library(sf)
library(tmap)
library(dplyr)
library(nngeo)

pass_od <- read_sf("data/clean/od_flights_pass.gpkg")
airports <- read_sf("data/clean/airports_clean_second_pass.gpkg")
pass_od <- st_drop_geometry(pass_od)


foo <- airports[duplicated(airports$airport),]
foo <- airports[airports$airport %in% foo$airport,]
foo$airport_valid <- foo$airport %in% od$airport2

nn <- nngeo::st_nn(airports, airports, k = 2, maxdist = 50000, returnDist = TRUE)

res <- sapply(nn$dist, function(x){
  if(length(x) == 2){
    return(x[2])
  } else {
    return(NA)
  }
})


airports$dist <- res

bar <- airports[!is.na(airports$dist),]
bar <- bar[bar$dist < 10000,]
qtm(airports) +
  qtm(bar, dots.col = "red")

bar <- pass_od[pass_od$airport2 == "Oslo (Gardemoen)" | pass_od$airport2 == "Oslo (Gardermoen)",]

# Find airports that have no OD

foo_none <- left_join(airports, unique(st_drop_geometry(pass_od[,2:5])), by = c("airport" = "airport2", "country" = "airport2_country"))
