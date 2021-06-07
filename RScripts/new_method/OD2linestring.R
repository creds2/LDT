library(sf)
library(tmap)
library(dplyr)
tmap_mode("view")

od <- readRDS("data/clean/pass_flighs_od_v2.Rds")
airports <- read_sf("data/clean/airports_clean_second_pass_v2.gpkg")


qtm(airports)

names(airports) <- c("airport","country","geom_from")
od <- left_join(od, airports, by = c("airport1" = "airport","airport1_country" = "country"))
names(airports) <- c("airport","country","geom_to")
od <- left_join(od, airports, by = c("airport2" = "airport","airport2_country" = "country"))

summary(st_is_empty(od$geom_from))
summary(st_is_empty(od$geom_to))

od_good <- od[!st_is_empty(od$geom_from) & !st_is_empty(od$geom_to),]
od_bad <- od[st_is_empty(od$geom_from) | st_is_empty(od$geom_to),]
message(paste(round(nrow(od_bad) / nrow(od) * 100,2),"% bad geoms"))

od_bad_from <- od_bad[st_is_empty(od_bad$geom_from),]
od_bad_to <- od_bad[st_is_empty(od_bad$geom_to),]
od_bad_to <- od_bad_to[od_bad_to$airport2 != "Unknown",]

od_bad_to <- od_bad_to %>% group_by(airport2, airport2_country) %>%
  summarise(pass = sum(c(`2018`,`2017`,`2016`,`2015`,`2014`,`2013`,`2012`,`2011`,`2010`,
                         `2009`,`2008`,`2007`,`2006`,`2005`,`2004`,`2003`,`2002`,`2001`,`2000`,
                         `1999`,`1998`,`1997`,`1996`,`1995`,`1994`,`1993`,`1992`,`1991`,`1990`) , na.rm = TRUE),
            flt = sum(c(`flt_2018`,`flt_2017`,`flt_2016`,`flt_2015`,`flt_2014`,`flt_2013`,`flt_2012`,`flt_2011`,`flt_2010`,
                        `flt_2009`,`flt_2008`,`flt_2007`,`flt_2006`,`flt_2005`,`flt_2004`,`flt_2003`,`flt_2002`,`flt_2001`,`flt_2000`,
                        `flt_1999`,`flt_1998`,`flt_1997`,`flt_1996`,`flt_1995`,`flt_1994`,`flt_1993`,`flt_1992`,`flt_1991`,`flt_1990`), na.rm = TRUE))

od_bad_from <- od_bad_from %>% group_by(airport1, airport1_country) %>%
  summarise(pass = sum(`2018`, na.rm = TRUE))

names(airports) <- c("airport", "country", "geom")

airports_bad <- airports[!airports$airport %in% od$airport2, ]
airports_bad <- airports_bad[airports_bad$country != "United Kingdom", ]

message(paste(round(sum(rowSums(od_bad[,as.character(1990:2018)]), na.rm = T) / 
                      sum(rowSums(od[,as.character(1990:2018)]), na.rm = T) * 100,4),"% missing passengers"))

#od_bad <- as.data.frame(table(od_bad$airport2))

line <- lapply(1:nrow(od_good), function(i){
  st_cast(st_combine(x = c(od_good$geom_from[i], od_good$geom_to[i])), "LINESTRING")
})
line <- do.call(c, line)
line <- st_segmentize(line, units::set_units(1, km))
od_good <- as.data.frame(od_good)
od_good$geometry <- line
#stop()
#od_good$length2 <- round(geodist::geodist(st_coordinates(od_good$geom_from), st_coordinates(od_good$geom_to), paired = TRUE, measure = "geodesic") / 1000, 1)
od_good$geom_from <- NULL
od_good$geom_to <- NULL

od_good <- st_as_sf(od_good, crs = 4326)

od_good$length_km <- round(as.numeric(st_length(od_good)) / 1000, 1)

 


od_good$pass_km_2018 <- od_good$`2018` * od_good$length_km

od_top <- od_good[!is.na(od_good$pass_km_2018),]
od_top <- od_top[od_top$pass_km_2018 > 1e10,]


pass_km <- st_drop_geometry(od_good)
pass_km <- group_by(pass_km, airport2, airport2_country) %>%
  summarise(pass_km_2018 = sum(pass_km_2018, na.rm = TRUE))
pass_km <- pass_km[!is.na(pass_km$pass_km_2018),]
pass_km <- pass_km[pass_km$pass_km_2018 > 1e6,]
pass_km <- pass_km[order(pass_km$pass_km_2018, decreasing = TRUE),]

paris <- od_good[od_good$airport2_country == "France",]
paris <- st_drop_geometry(paris)
paris <- paris[,c("airport1","airport1_country", "airport2","airport2_country",as.character(1990:2018), "length_km","pass_km_2018")]
paris <- paris[paris$airport2 %in% c("Paris (Charles De Gaulle)","Paris (Le Bourget)","Paris (Orly)"),]
paris_sum <- paris %>%
  group_by(airport1) %>%
  summarise_at(as.character(1990:2018), sum, na.rm = TRUE)
paris_sum$airport1[paris_sum$`2018` < 100000] <- "Other"
paris_sum <- paris_sum %>%
  group_by(airport1) %>%
  summarise_at(as.character(1990:2018), sum, na.rm = TRUE)
paris_sum <- tidyr::pivot_longer(paris_sum, cols = as.character(1990:2018))
names(paris_sum) <- c("Airport","Year","Passengers")
paris_sum$Year <- as.numeric(paris_sum$Year)
library(ggplot2)

eurostar <- data.frame(Airport = rep("Eurostar", 24),
                       Year = 1995:2018,
                       Passengers = 1e6 *c(3,4.9,6,6.3,6.6,7.1,7,6.6,6.3,
                                            7.3,7.5,7.9,8.3,9.1,9.2,9.5,9.7,9.9,
                                            10,10.2,10.2,10, 10.1,11)  )

paris_sum <- rbind(paris_sum, eurostar)
ggplot(paris_sum, aes(Year, Passengers, colour = Airport)) +
  geom_line(lwd = 1) +
  scale_x_continuous(breaks = seq(1990,2018,2))

qtm(od_top, lines.col = "pass_km_2018")
write_sf(od_good, "data/clean/od_flights_pass_v2.gpkg")
