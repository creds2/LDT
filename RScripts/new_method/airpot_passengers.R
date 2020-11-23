# find rail airports
library(sf)
library(dplyr)

pass_od <- readRDS("data/clean/passenger_od_second_clean.Rds")
airports <- read_sf("data/clean/airports_clean_second_pass.gpkg")

pass_od <- pass_od[,c("airport1","airport1_country", "airport2","airport2_country","2018")]
pass <- group_by(pass_od, airport2) %>%
  summarise(pass = sum(`2018`))
pass <- pass[pass$pass > 0,]

airports <- left_join(airports, pass, by = c("airport" = "airport2"))
airports <- airports[airports$pass > 0,]
write_sf(airports, "data/clean/airports_pass_2018.gpkg")
