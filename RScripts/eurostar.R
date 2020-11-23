# EuroStar Comparison
library(sf)
library(tmap)

pass_od <- read_sf("data/clean/od_flights_pass.gpkg")
airports <- read_sf("data/clean/airports_clean_second_pass.gpkg")

airports <- airports[airports$country %in% c("United Kingdom","France","Belgium"),]

airports_sub <- airports[airports$airport %in% c("Paris (Charles De Gaulle)", "Brussels","Heathrow"),]
airports_sub <- st_buffer(airports_sub, 0.8)
airports_sub <- st_union(airports_sub)


airports <- airports[airports_sub,]
pass_eurostar <- pass_od[pass_od$airport1 %in% airports$airport & pass_od$airport2 %in% airports$airport, ]
pass_eurostar <- pass_eurostar[pass_eurostar$airport1_country != pass_eurostar$airport2_country, ]
qtm(pass_eurostar, lines.col = "X2018", scale = 3)

write.csv(st_drop_geometry(pass_eurostar), "data/flights_london_paris_brussles.csv", row.names = FALSE)

eurostar <- data.frame(Airport = rep("Eurostar", 24),
                       Year = 1995:2018,
                       Passengers = 1e6 *c(3,4.9,6,6.3,6.6,7.1,7,6.6,6.3,
                                           7.3,7.5,7.9,8.3,9.1,9.2,9.5,9.7,9.9,
                                           10,10.2,10.2,10, 10.1,11)  )


write.csv(eurostar, "data/eurostar_pass.csv", row.names = FALSE)

paris_sum <- rbind(paris_sum, eurostar)
ggplot(paris_sum, aes(Year, Passengers, colour = Airport)) +
  geom_line(lwd = 1) +
  scale_x_continuous(breaks = seq(1990,2018,2))

qtm(od_top, lines.col = "pass_km_2018")
write_sf(od_good, "data/clean/od_flights_pass.gpkg")