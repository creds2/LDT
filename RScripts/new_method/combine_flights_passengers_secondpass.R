# Second Pass Cleaning
library(sf)
library(dplyr)

pass_od <- readRDS("data/clean/passenger_od_first_clean_2021.Rds")
flight_od <- readRDS("data/clean/flighs_od_first_clean_2021.Rds")
airports <- read_sf("data/clean/airports_clean_first_pass_2021.gpkg")
airports <- airports[airports$airport != "Vary (Chalons Sur Marne)",]
airports_2021 <- readRDS("data/airports_missing_2021.Rds")

airports_extra <- data.frame(airport = c("Tabarka","St Maarten","Sydney Canada","Lanseria","Izmir (Cumaovasi)","Islamabad"),
                             country = c("Tunisia","St Maarten","Canada","Republic of South Africa","Turkey","Pakistan"),
                             geometry = st_sfc(list(
                               st_point(c(8.876944, 36.98)),
                               st_point(c(-63.109444, 18.040833)),
                               st_point(c(-60.048056, 46.161389)),
                               st_point(c(27.926111, -25.938611)),
                               st_point(c(27.33114, 37.95034)),
                               st_point(c(72.82565, 33.549083))
                             )))
names(airports_extra) <- names(airports)
names(airports_2021) <- names(airports)

airports_extra <- st_as_sf(airports_extra, crs = 4326)
airports_2021 <- st_as_sf(airports_2021, crs = 4326)
airports <- rbind(airports, airports_extra)
airports <- rbind(airports, airports_2021)
names(airports$geom) <- NULL


airports$geom[airports$airport == "Alverca"] <- st_point(c(-9.0300999, 38.8833008))
airports$geom[airports$airport == "Satenas"] <- st_point(c(12.7144003, 58.4263992))
airports$geom[airports$airport == "Seoul Afb"] <- st_point(c(127.113889, 37.445833))
airports$geom[airports$airport == "Seoul (Kimpo)"] <- st_point(c(126.790556, 37.558056))
airports$geom[airports$airport == "Tollerton Nottingham"] <- st_point(c(-1.080855, 52.91872))
airports$geom[airports$airport == "Vagar"] <- st_point(c(-7.27546, 62.06333))
airports$geom[airports$airport == "Benazir Bhutto"] <- st_point(c(73.099167, 33.616389))
airports$geom[airports$airport == "Hong Kong"] <- st_point(c(113.9185, 22.30805))
airports$geom[airports$airport == "Camp Springs (Andrews Afb)"] <- st_point(c(-76.88363, 38.79652))
airports$geom[airports$airport == "Oil Rigs"] <- st_point(c(0.953009, 58.238252))
airports$geom[airports$airport == "Dakar"] <- st_point(c(-17.49, 14.739444))
airports$geom[airports$airport == "Istanbul"] <- st_point(c(28.727778, 41.262222))

tidy_airports <- function(from,to){
  flight_od$airport1[flight_od$airport1 %in% from] <-  to
  flight_od$airport2[flight_od$airport2 %in% from] <-  to
  pass_od$airport1[pass_od$airport1 %in% from] <-  to
  pass_od$airport2[pass_od$airport2 %in% from] <-  to
  airports$airport[airports$airport %in% from] <-  to
  assign('flight_od',flight_od,envir=.GlobalEnv)
  assign('pass_od',pass_od,envir=.GlobalEnv)
  assign('airports',airports,envir=.GlobalEnv)
}

tidy_countries <- function(from,to){
  flight_od$airport1_country[flight_od$airport1_country %in% from] <-  to
  flight_od$airport2_country[flight_od$airport2_country %in% from] <-  to
  pass_od$airport1_country[pass_od$airport1_country %in% from] <-  to
  pass_od$airport2_country[pass_od$airport2_country %in% from] <-  to
  airports$country[airports$country %in% from] <- to
  assign('flight_od',flight_od,envir=.GlobalEnv)
  assign('pass_od',pass_od,envir=.GlobalEnv)
  assign('airports',airports,envir=.GlobalEnv)
}

tidy_countries("Rumania","Romania")
tidy_countries(c("Portugal(Madeira)","Portugal (Excluding Madeira)"),"Portugal")
tidy_countries("Oil Rigs","United Kingdom")

tidy_airports("Lerwick  (Tingwall)","Lerwick (Tingwall)")
tidy_airports(c("Baghdad Int","Baghdad (Saddam)"),"Baghdad")
tidy_airports("Verona","Verona Villafranca")
tidy_airports("Bydgoszcz/Szweredowo","Bydgoszcz")
tidy_airports("Sherchenko","Aktau")
tidy_airports("Al Maktoum","Dubai (World Central)")
tidy_airports("Kishinev","Chisinau (Kishinev)")
tidy_airports("Kiev","Kiev (Zhulyany)")
tidy_airports("Chicago","Chicago (O'hare))")
tidy_airports("Washington","Washington (Dulles)")
tidy_airports("Nevsehir Kapadokya ","Nevsehir Kapadokya")
tidy_airports(c("Bangkok Suvarnabhumi Airport","Bangkok Suvarnabhumi "),"Bangkok Suvarnabhumi")
tidy_airports("Bangkok","Bangkok (Don Muang)")
tidy_airports("St Lucia","St Lucia (Hewanorra)")
tidy_airports("Castellã“n Costa Azahar","Castellon Costa Azahar")
tidy_airports("Porto","Oporto (Portugal)")
tidy_airports("Dubia","Dubai")
tidy_airports("Castletown","Isle of Man")
tidy_airports("Cotswold Apt - Kemble","Cotswold - Kemble")
tidy_airports("Forres","Kinloss")
tidy_airports("Dallas","Dallas/Fort Worth")
tidy_airports("Basle Mulhouse","EuroAirport Basel-Mulhouse-Freiburg")
tidy_airports("Goteborg","Goteborg (Landvetter)")
tidy_airports("Izmir (Adnam Menderes)","Izmir (Adnan Menderes)")
tidy_airports("Goteborg City","Goteborg (Save)")
tidy_airports("Connaught","Ireland West(Knock)")
tidy_airports("Port Lotniczy Szczecin-Goleniow","Szczecin (Golenow)")
tidy_airports("Kavalla","Kavala")
tidy_airports("Agadir (Al Massira)","Agadir")
tidy_airports("Mytilini","Mitilini")
tidy_airports("La Coruna","a Coruna")
tidy_airports("Imam Khomieni","Tehran Imam Khomeini")
tidy_airports("Modlin Masovia","Warsaw (Modlin Masovia)")
tidy_airports("Bangalore","Bangalore (Bengaluru)")
tidy_airports("Volos","Volos Nea Anchilos")
tidy_airports(c("Cunagua ( Cayo Coco)","Cunagua"),"Cunagua (Cayo Coco)")
tidy_airports("Alma Ata","Almaty")
tidy_airports(c("Baghdad","Bagdhad (Geca)"),"Baghdad (Geca)")
tidy_airports("Tranpani","Trapani")
tidy_airports("Angers- Marce","Angers")
tidy_airports("Shanghai","Shanghai (Pu Dong)")
tidy_airports("Hyderabad","Hyderabad ( Rajiv Ghandi )")
tidy_airports("Baden Baden","Karlsruhe/Baden Baden")
tidy_airports("Georgetown","Georgetown (Guyana)")
tidy_airports("St Moritz","Samedan/St Moritz")
tidy_airports(c("Varry (Chalons Sur Marne)","Vary (Chalons Sur Marne)"),"Chalons Sur Marne")
tidy_airports("Belize","Belize City")
tidy_airports("Ingolstadt","Ingolstadt-Manching")
tidy_airports("Valley","Anglesey (Valley)")
tidy_airports("Chateauroux","Chateauroux Deols")
tidy_airports("Rouzyne","Prague")
tidy_airports("Lapeenranta","Lappeenranta")
tidy_airports("Moline","Moline (Quad City)")
tidy_airports("Beek","Maastricht Aachen")
tidy_airports("Ras Nasrani","Sharm El Sheikh (Ophira)")
tidy_airports("Doncaster","Doncaster Sheffield")

tidy_airports("Oslo (Gardemoen)","Oslo (Gardermoen)")
tidy_airports("Volos Nea Anchilos","Nea Anchialos")
tidy_airports("Madras/Chennai","Chennai")
tidy_airports("Castellon De La Plana","Castellon Costa Azahar")
tidy_airports("Nuremburg","Nuremberg")
tidy_airports("Nykoping","Stockholm (Skavsta)")
tidy_airports("Bursa/Yenisehir","Bursa Yenisehir")
tidy_airports("Corlu (Afb)","Tekirdag (Corlu)")
tidy_airports("Cagliari","Cagliari (Elmas)")

tidy_airports("Los Angeles International","Los Angeles")
tidy_airports("Abu Dhabi International","Abu Dhabi")
tidy_airports("Miami International","Miami")
tidy_airports("Philadelphia International","Philadelphia")

tidy_airports("Denver International","Denver")
tidy_airports("Benazir Bhutto International","Benazir Bhutto")
tidy_airports("Guangzhou Baiyun International","Guangzhou Baiyun")
tidy_airports("Halifax Int","Halifax")
tidy_airports("Ottawa International","Ottawa")
tidy_airports("Tarbes-Lourdes International","Tarbes-Lourdes")
tidy_airports("Islamabad International","Islamabad")
tidy_airports("Jakarta (Soekarno-Hatta Intnl)","Jakarta (Soekarno-Hatta)")
tidy_airports("Jakarta (Soekarno-Hattanl)","Jakarta (Soekarno-Hatta)")
tidy_airports("Changsha Huanghua International","Changsha Huanghua")
tidy_airports("Wuhan Tianhe International","Wuhan Tianhe")

tidy_airports("Male International","Male")
tidy_airports("Chongqing Jiangbei International","Chongqing Jiangbei")
tidy_airports("Auckland International","Auckland")
tidy_airports("Phu Quoc International","Phu Quoc")
tidy_airports("Sanya Phoenix International","Sanya Phoenix")
tidy_airports("Kharkov Osnova Intl","Kharkov Osnova")
tidy_airports("Windsor Locks Bradley Intl","Windsor Locks Bradley")
tidy_airports("East Midlands International","East Midlands")
tidy_airports("Nottingham East Midlands Int'l","East Midlands")
tidy_airports("Manston (Kent Int)","Kent")
tidy_airports("Bali International","Bali")
tidy_airports("Minot International","Minot")
tidy_airports("Erbil International","Erbil")

tidy_airports("Jorge Chavez","Lima")
tidy_airports("Nagoya (Afb)","Nagoya")
tidy_airports("Calcutta","Kolkata")
tidy_airports("Medan","Kualanamu")
tidy_airports("Albert - Bray","Albert - Picardie")
tidy_airports("Chalons (Vatry)","Chalons Sur Marne")
tidy_airports("Astana","Nursultan Nazerbayev")


pass_od$airport2_country[pass_od$airport2 == "Pristina"] <- "Kosovo"
pass_od$airport2_country[pass_od$airport2 == "Bahrain"] <- "Bahrain"
pass_od$airport2_country[pass_od$airport2 == "Podgorica"] <- "Republic of Montenegro"
flight_od$airport2_country[flight_od$airport2 == "Podgorica"] <- "Republic of Montenegro"
flight_od$airport2_country[flight_od$airport2 == "St Maarten"] <- "St Maarten"

pass_od$airport2_country[pass_od$airport2 == "EuroAirport Basel-Mulhouse-Freiburg"] <- "France"
flight_od$airport2_country[flight_od$airport2 == "EuroAirport Basel-Mulhouse-Freiburg"] <- "France"

pass_od$airport2_country[pass_od$airport2 == "Castletown"] <- "Isle of Man"
pass_od$airport1_country[pass_od$airport1 == "Castletown"] <- "Isle of Man"
pass_od$airport1_country[pass_od$airport1 == "Isle of Man"] <- "Isle of Man"
pass_od$airport2_country[pass_od$airport2 == "Isle of Man"] <- "Isle of Man"
flight_od$airport1_country[flight_od$airport1 == "Isle of Man"] <- "Isle of Man"
flight_od$airport2_country[flight_od$airport2 == "Isle of Man"] <- "Isle of Man"

pass_od$airport2_country[pass_od$airport2_country == "Zaire"] <- "Democratic Republic of Congo"
flight_od$airport2_country[flight_od$airport2_country == "Zaire"] <- "Democratic Republic of Congo"




pass_od2 <- pass_od %>%
  group_by(airport1,airport1_country,airport2,airport2_country) %>%
  summarise_all(sum, na.rm = TRUE)

flight_od2 <- flight_od %>%
  group_by(airport1,airport1_country,airport2,airport2_country) %>%
  summarise_all(sum, na.rm = TRUE)

pass_od2 <- pass_od2[rowSums(pass_od2[,as.character(1990:2021)]) != 0,]
flight_od2 <- flight_od2[rowSums(flight_od2[,paste0("flt_",1990:2021)]) != 0,]

# Check by country
cnts <- unique(c(pass_od2$airport2_country, flight_od2$airport2_country))
cnts <- cnts[order(cnts)]

res <- vector(mode = "list", length = length(cnts))
names(res) <- cnts
for(i in 1:length(cnts)){
  message(cnts[i])
  sub_pass <- pass_od2$airport2[pass_od2$airport2_country == cnts[i]]
  sub_flt <- flight_od2$airport2[flight_od2$airport2_country == cnts[i]]
  

  sub_pass <- unique(sub_pass)
  sub_flt <- unique(sub_flt)
  sub_flt <- sub_flt[sub_flt != "Unknown"]
  sub_pass <- sub_pass[sub_pass != "Unknown"]
  sub_flt <- sub_flt[order(sub_flt)]
  sub_pass <- sub_pass[order(sub_pass)]
  sub_res <- list(nopass = sub_flt[!sub_flt %in% sub_pass],
              noflt = sub_pass[!sub_pass %in% sub_flt])
  res[[i]] <- sub_res
}

res <- lapply(res, function(x){
  if(sum(lengths(x)) == 0){
    return(NULL)
  }
  return(x)
})
res <- res[lengths(res) != 0]




all_od <- full_join(pass_od2, flight_od2, by= c("airport1","airport1_country","airport2","airport2_country") )
saveRDS(all_od,"data/clean/pass_flighs_od_2021.Rds")
saveRDS(pass_od2, "data/clean/passenger_od_second_clean_2021.Rds")
saveRDS(flight_od2, "data/clean/flighs_od_second_clean_2021.Rds")
airports <- airports[!duplicated(airports),]

airports <- airports[!duplicated(st_drop_geometry(airports[,c("airport","country")])),]
write_sf(airports, "data/clean/airports_clean_second_pass_2021.gpkg")


stop(" End of code")
# Check Airport Locations
ap_miss <- unique(all_od[,c("airport1","airport1_country")])
ap_miss2 <- unique(all_od[,c("airport2","airport2_country")])
names(ap_miss2) <- names(ap_miss)
ap_miss <- rbind(ap_miss, ap_miss2)
ap_miss <- left_join(ap_miss, airports, by = c("airport1" = "airport", "airport1_country" = "country"))
ap_miss <- ap_miss[st_is_empty(ap_miss$geom),]
ap_miss <- ap_miss[ap_miss$airport1 != "Unknown",]
pass_total <- pass_od2 %>%
  group_by(airport2, airport2_country) %>%
  summarise(total = sum(`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`,`2001`,`2002`,`2003`,`2004`,`2005`,
                        `2006`,`2007`,`2008`,`2009`,`2010`,`2011`,`2012`,`2013`,`2014`,`2015`,`2016`,`2017`,`2018`,`2019`,`2020`,`2021`, na.rm = TRUE))
ap_miss <- left_join(ap_miss, pass_total, by = c("airport1" = "airport2", "airport1_country" = "airport2_country"))

#saveRDS(ap_miss,"data/airports_missing_2021.Rds")

all_missing <- all_od[is.na(all_od$`2019`) | is.na(all_od$flt_2019),]
all_missing <- all_missing[with(all_missing, order(airport2_country, airport2)),]
all_missing <- all_missing[all_missing$airport1 != "Unknown",]
all_missing <- all_missing[all_missing$airport2 != "Unknown",]
all_missing <- all_missing[all_missing$airport1 %in% 
                             c("Gatwick", "Heathrow","Aberdeen",
                               "Belfast City (George Best)", "Belfast",
                               "Birmingham","Bournemouth","Cardiff",
                               "Doncaster Sheffield","East Midlands","Edinburgh",
                               "Exeter","Gatwick","Glasgow","Heathrow","Luton",
                               "Liverpool (John Lennon)","London City","Manchester",
                               "Newcastle","Stansted"), ]
all_missing_top <- all_missing
all_missing_top$rowsums <- rowSums(all_missing_top[5:ncol(all_missing_top)], na.rm = TRUE)
all_missing_top$missing_flights <- is.na(all_missing_top$flt_2018)
all_missing_top$missing_fpassengers <- is.na(all_missing_top$`2018`)
all_missing_top <- all_missing_top[,c(1:4,seq(ncol(all_missing_top)-2,ncol(all_missing_top)))]
all_missing_top <- all_missing_top[all_missing_top$rowsums > 1000,]

# Missing airports
ap_flights <- unique(flight_od2$airport2)
ap_pass <- unique(pass_od2$airport2)
ap_pass2 <- ap_pass[!ap_pass %in% ap_flights]
ap_flights2 <- ap_flights[!ap_flights %in% ap_pass]

ap_all <- all_od[all_od$airport2 %in% c(ap_pass2, ap_flights2),]
ap_all <- ap_all[,c("airport2","airport2_country","2018","flt_2018")]
ap_all <- ap_all[ap_all$airport2_country != "Oil Rigs",]
ap_all <- unique(ap_all)

# check for odd results
all_missing_flights <- all_od[is.na(all_od$flt_2020), ]
foo <- as.data.frame(table(all_od$airport1[is.na(all_od$flt_2020)]))
bar <- as.data.frame(table(all_od$airport1[!is.na(all_od$flt_2020)]))
table(flight_od2$airport1)

all_missing_flights <- all_missing_flights[all_missing_flights$airport1 %in% 
                                             c("Gatwick", "Heathrow","Aberdeen",
                                               "Belfast City (George Best)", "Belfast",
                                               "Birmingham","Bournemouth","Cardiff",
                                               "Doncaster Sheffield","East Midlands","Edinburgh",
                                               "Exeter","Gatwick","Glasgow","Heathrow","Luton",
                                               "Liverpool (John Lennon)","London City","Manchester",
                                               "Newcastle","Stansted"), ]


all_missing_flights <- all_missing_flights %>%
  group_by(airport2, airport2_country) %>%
  summarise(total = sum(`2018`),
            count = n())

all_missing_passengers <- left_join(flight_od2, pass_od2 , by= c("airport1","airport1_country","airport2","airport2_country") )
all_missing_passengers <- all_missing_passengers[is.na(all_missing_passengers$`2020`), ]
all_missing_passengers <- all_missing_passengers %>%
  group_by(airport2, airport2_country) %>%
  summarise(total = sum(flt_2021,flt_2020,flt_2019,flt_2018, flt_2017, flt_2016,flt_2015,flt_2014,
                        flt_2013, flt_2012,flt_2011,flt_2010,flt_2009,
                        flt_2008, flt_2007,flt_2006,flt_2005,flt_2004,
                        flt_2003, flt_2002,flt_2001,flt_2000,flt_1999,
                        flt_1998,flt_1997,flt_1996,flt_1995,flt_1994,
                        flt_1993,flt_1992,flt_1991,flt_1990),
            count = n())


foo <- all_od[all_od$airport2_country == "Russia",]
foo <- foo[is.na(foo$flt_2018) | is.na(foo$`2018`),]
foo <- foo[,c("airport1","airport1_country", "airport2","airport2_country","2018","flt_2018")]
foo <- as.data.frame(table(foo$airport2))
