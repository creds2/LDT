# Make simple map for flow data

library(sf)
library(tmap)
library(dplyr)
library(ggplot2)
library(tidyr)

flow <- readRDS("flow_final.Rds")
flow$flightkm_2018 <- flow$`2018` * flow$length_km




# Classify as domestic, short hall, long hall etc

flow$range <- ifelse(grepl("United Kingdom", flow$fromAirportOF) & grepl("United Kingdom", flow$toAirportOF),
                     "Domestic",NA)

flow_domestic <- flow[!is.na(flow$range),]

london <- st_sf(data.frame(id = 1, 
                           geometry = st_sfc(st_point(c(-0.1275, 51.507222))) 
                                              ),crs = 4326)
london <- st_transform(london, 27700)
near <- st_buffer(london, 800000)
near <- st_transform(near, 4326)
qtm(near)

is_near <- st_within(flow, near)
is_near <- ifelse(sapply(is_near, length) == 0, 0, 1)
flow$range <- ifelse(is.na(flow$range) & is_near == 1,"Near",flow$range)
flow$range <- ifelse(is.na(flow$range) & flow$length_km < 1500,"Shorthall",flow$range)
flow$range <- ifelse(is.na(flow$range) & flow$length_km < 4000,"Mediumhall",flow$range)
flow$range <- ifelse(is.na(flow$range) & flow$length_km > 4000,"Longhall",flow$range)


range_summary <- flow %>%
  st_drop_geometry() %>%
  group_by(range) %>%
  summarise_at(c(as.character(1993:2018)),sum, na.rm = TRUE) %>%
  gather(key = "year", value = "flights", -range)

range_summary$range <- factor(range_summary$range, levels = c("Domestic","Near","Shorthall","Mediumhall","Longhall"))
  
ggplot(range_summary, aes(x=year, y=flights, fill=range)) +
  geom_bar(stat="identity") +
  ggtitle("Flights per year to & from the UK, 1993 -2018") +
  ggsave("plots/number_of_flights.jpg")


flow_km <- flow
flow_km <- st_drop_geometry(flow_km)
for(i in 3:29){
  flow_km[,i] <- flow_km[,i] * flow_km$length_km
}

range_summary <- flow_km %>%
  group_by(range) %>%
  summarise_at(c(as.character(1993:2018)),sum, na.rm = TRUE) %>%
  gather(key = "year", value = "flight_km", -range)

range_summary$range <- factor(range_summary$range, levels = c("Domestic","Near","Shorthall","Mediumhall","Longhall"))

ggplot(range_summary, aes(x=year, y=flight_km, fill=range)) +
  geom_bar(stat="identity") +
  ggtitle("Flights km year to & from the UK, 1993 -2018") +
  ggsave("plots/flight_km.jpg")

object.size(flow)
flow_plot <- st_simplify(flow, dTolerance = 0.1)
as.numeric(object.size(flow_plot) / object.size(flow)) * 100

map <- tm_shape(flow_plot[flow_plot$`2018` > 5,]) +
  tm_lines(lwd = "2018", col = "range", scale = 2, palette = "Dark2",
           popup.vars = c("fromAirportOF","toAirportOF","1991","2000","2010","2018","length_km")) 
map
