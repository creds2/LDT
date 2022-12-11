# Add emissions
library(sf)
library(dplyr)
library(furrr)

emissions_factors <- readxl::read_excel("data/emissions_factors.xlsx", sheet = "Final")

pass_od <- read_sf("data/clean/od_flights_pass_2021.gpkg")
names(pass_od) <- c("airport1","airport1_country","airport2","airport2_country",
                    paste0("pass_",1990:2021),
                    paste0("flt_",1990:2021),
                    "length_km","pass_km_2018","pass_km_2019","pass_km_2020","pass_km_2021","geom")

# Detour Factors https://www.sciencedirect.com/science/article/pii/S0966692318305544

# Short Haul (<1000km) 14.98 - 14.3%
# Medium Haul (1000 - 4000km) 24.58 - 7.3%
# Long Haul (> 4000 km) 10.31 - 4.8%

weighted_dist <- function(x){
  
  if(x < 1000){
    y <- x * 1.143
  } else if(x > 4000){
    y <- x * 1.048
  } else {
    y <- x * 1.073
  }
  y <- round(y,1)
  y
}

pass_od$length_km_detour <- sapply(pass_od$length_km, weighted_dist)

# Work out Passenger KM
pass_od <- as.data.frame(pass_od)

for(i in 1990:2021){
  pass_od[paste0("pass_km_",i)] <- pass_od[,paste0("pass_",i)] * pass_od$length_km_detour
}

distbnd <- function(x){
  if(x %in% c("Irish Republic","Kosovo")){
    return("shorthaul")
  }
  
  if(x %in% c("Guernsey","Isle of Man","Jersey","United Kingdom","Oil Rigs")){
    return("domestic")
  }
  
  if(x %in% c("Ascension Island","Virgin Islands (U.s.a)","Somali Republic")){
    return("longhaul")
  }
  
  cont <- countrycode::countrycode(sourcevar = x,
                      origin = "country.name",
                      destination = "continent")
  
  if(cont == "Europe"){
    return("shorthaul")
  } else {
    return("longhaul")
  }
}

plan(multisession, workers = 28)
distance_band <- future_map_chr(pass_od$airport2_country,distbnd, .progress = TRUE)
plan(sequential)
summary(is.na(distance_band))
table(distance_band)

pass_od$distance_band <- distance_band


for(i in 1990:2021){
  ef = emissions_factors[emissions_factors$year == i, ]
  ef = data.frame(distance_band = c("domestic","shorthaul","longhaul"),
                  emissions_factor = t(ef[2:4]))
  ef2 = data.frame(distance_band = distance_band)
  ef2 = left_join(ef2, ef, by = "distance_band")
  pass_od[paste0("emissions_",i)] <- pass_od[,paste0("pass_km_",i)] * ef2$emissions_factor
  rm(ef, ef2)
}

# Country Bands

pass_od$continent <- countrycode::countrycode(sourcevar = pass_od$airport2_country,
                                     origin = "country.name",
                                     destination = "continent")
pass_od$continent[pass_od$airport2_country == "Irish Republic"] <- "Europe"
pass_od$continent[pass_od$airport2_country == "Kosovo"] <- "Europe"
pass_od$continent[pass_od$airport2_country == "Oil Rigs"] <- "Europe"
pass_od$continent[pass_od$airport2_country == "Virgin Islands (U.s.a)"] <- "Americas"
pass_od$continent[pass_od$airport2_country == "Ascension Island"] <- "Africa"
pass_od$continent[pass_od$airport2_country == "Somali Republic"] <- "Africa"

#Country/Continent
topdests <- c("Spain","Italy","Germany","France","Irish Republic",
              "Netherlands","Portugal","Poland","Greece","Turkey",
              "United States of America","Canada",
              "United Arab Emirates","India","Hong Kong","Singapore","United Kingdom")


pass_od$country_region <- if_else(pass_od$airport2_country %in% topdests,
                                  pass_od$airport2_country,
                                  pass_od$continent)
pass_od$country_region[pass_od$country_region == "United Kingdom"] <- "Domestic"
pass_od$country_region[pass_od$country_region == "United States of America"] <- "USA"
pass_od$country_region[pass_od$country_region == "United Arab Emirates"] <- "UAE"
pass_od$country_region[pass_od$country_region == "Europe"] <- "Europe Other"
pass_od$country_region[pass_od$country_region == "Americas"] <- "Americas Other"
pass_od$country_region[pass_od$country_region == "Asia"] <- "Asia Other"
pass_od$country_region[pass_od$country_region == "Irish Republic"] <- "Ireland"

pass_od <- pass_od[,c("airport1","airport1_country","airport2","airport2_country",
                      "length_km","length_km_detour","continent","country_region","distance_band",
                      paste0("pass_",1990:2021),
                      paste0("flt_",1990:2021),
                      paste0("pass_km_",1990:2021),
                      paste0("emissions_",1990:2021),
                      "geom")]


write_sf(pass_od, "data/clean/od_emissions_2021.gpkg")

