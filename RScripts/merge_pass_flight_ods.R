pass_od <- readRDS("data/passenger_od_wide.Rds")
pass_od <- pass_od[!pass_od$airport2 %in% c("=","Unknown"),]

tidy3 <- function(from,to){
  pass_od$airport1[pass_od$airport1 %in% from] <-  to
  pass_od$airport2[pass_od$airport2 %in% from] <-  to
  assign('pass_od',pass_od,envir=.GlobalEnv)
}

tidy3("Durham Tees Valley", "Teesside Airport")
tidy3("Wick John o Groats", "Wick")
tidy3("City of Derry (Eglinton)", "Londonderry")
tidy3("Manston (Kent Int)", "Kent")
tidy3("Belfast City","Belfast City (George Best)")
tidy3("Liverpool",	"Liverpool (John Lennon)")
tidy3("Cardiff Wales","Cardiff")
tidy3("Teesside Airport","Teesside")

pass_od <- pass_od %>%
  group_by(airport1,airport1_country,airport2,airport2_country) %>%
  summarise_all(sum, na.rm = TRUE)


#TODO: check for mismatched in uk airport names between sf onject and pass_od, seems to be some mixing of names in pass_od, so check over

#pass_od <- pivot_wider(pass_od, names_from = "year", values_from = "total_pax", names_prefix = "pass_")

flight_od <- readRDS("flow_final.Rds")
pts <- lapply(flight_od$geometry, function(x){
  x <- st_coordinates(x)
  res <- data.frame(fx = x[1,1], fy = x[1,2], tx = x[nrow(x),1], ty = x[nrow(x),2])
  return(res)
})
pts <- bind_rows(pts)
flight_od <- bind_cols(flight_od,pts)

flight_od$geometry <- NULL
flight_od$length_km <- NULL

flight_od_from <- strsplit(flight_od$fromAirportOF, ", ")
flight_od_to <- strsplit(flight_od$toAirportOF, ", ")
flight_od$airport1 <- sapply(flight_od_from, `[[`, 1)
flight_od$airport1_country <- sapply(flight_od_from, `[[`, 2)
flight_od$airport2 <- sapply(flight_od_to, `[[`, 1)
flight_od$airport2_country <- sapply(flight_od_to, `[[`, 2)

flight_od <- flight_od[,c("airport1","airport1_country","airport2","airport2_country",
                          "1991","1993","1994",
                          "1995","1996","1997","1998","1999",
                          "2000","2001","2002","2003","2004",
                          "2005","2006","2007","2008","2009",
                          "2010","2011","2012","2013","2014",
                          "2015","2016","2017","2018","fx","fy","tx","ty")]

names(flight_od) <- c("airport1","airport1_country","airport2","airport2_country",
             "flt_1991","flt_1993","flt_1994",
             "flt_1995","flt_1996","flt_1997","flt_1998","flt_1999",
             "flt_2000","flt_2001","flt_2002","flt_2003","flt_2004",
             "flt_2005","flt_2006","flt_2007","flt_2008","flt_2009",
             "flt_2010","flt_2011","flt_2012","flt_2013","flt_2014",
             "flt_2015","flt_2016","flt_2017","flt_2018","fx","fy","tx","ty")

flight_od$airport1 <- gsub(" Airport","",flight_od$airport1)
flight_od$airport2 <- gsub(" Airport","",flight_od$airport2)
flight_od$airport1 <- gsub(" International","",flight_od$airport1)
flight_od$airport2 <- gsub(" International","",flight_od$airport2)
pass_od$airport1 <- gsub(" International","",pass_od$airport1)
pass_od$airport2 <- gsub(" International","",pass_od$airport2)

flight_od$airport1 <- iconv(flight_od$airport1, from="UTF-8", to="ASCII//TRANSLIT")
flight_od$airport2 <- iconv(flight_od$airport2, from="UTF-8", to="ASCII//TRANSLIT")

tidy <- function(from,to){
  flight_od$airport1[flight_od$airport1 %in% from] <-  to
  flight_od$airport2[flight_od$airport2 %in% from] <-  to
  assign('flight_od',flight_od,envir=.GlobalEnv)
}

tidy("Aberdeen Dyce", "Aberdeen")
tidy("Birmingham International", "Birmingham")
tidy("Cardiff International", "Cardiff")
tidy("George Best Belfast City", "Belfast City (George Best)")
tidy("Glasgow International", "Glasgow")
tidy("London Gatwick", "Gatwick")
tidy("London Heathrow", "Heathrow")
tidy("London Luton", "Luton")
tidy("London Stansted", "Stansted")
tidy("Robin Hood Doncaster Sheffield", "Doncaster Sheffield")
tidy("Liverpool John Lennon", "Liverpool (John Lennon)")


tidy("A Coruna","a Coruna")
tidy("Aarhus","Aarhus (Tirstrup)")
#tidy("Abadan","Ibadan")
tidy("Adnan Menderes","Izmir (Adnan Menderes)")
#tidy("Agra","Agadir")
tidy("Albert-Bray","Albert - Bray")
tidy("Alghero-Fertilia","Alghero (Fertilia)")
#tidy("Alta","Malta")
#tidy("Angads","Oujda Angad")
tidy("Anglesey","Anglesey (Valley)")
tidy("Ascension","Ascension Island")
tidy("Asturias","Asturias (Aviles)")
#tidy("Atar","Mostar")
tidy("Augsburg","Augsburg/Muelhausen")
tidy("Austin Bergstrom","Austin (Bergstrom)")
tidy("Baghdad","Baghdad Int")
tidy("Bahias de Huatulco","Bahias De Huatulco")
tidy("Banak","Bangkok (Don Muang)")
tidy("Bateen","Abu Dhabi - Bateen")
#tidy("Benina","Benin City")
tidy("Berlin-Schonefeld","Berlin (Schonefeld)")
tidy("Berlin-Tegel","Berlin (Tegel)")
tidy("Borg El Arab","Alexandria (Borg El Arab)")
tidy("Bradley","Windsor Locks Bradley Intl")
tidy("Brescia","Brescia/Montichiari")
tidy("Brno-Turany","Brno (Turany)")
tidy("Cagliari Elmas","Cagliari (Elmas)")
tidy("Castellon-Costa Azahar","Castellón Costa Azahar")
tidy("Catania-Fontanarossa","Catania (Fontanarossa)")
tidy("Charles de Gaulle","Paris (Charles De Gaulle)")
tidy("Chicago Midway","Chicago (Midway)")
tidy("Chicago O'Hare","Chicago (O'hare)")
tidy("Chisinau","Chisinau (Kishinev)")
tidy("Cluj-Napoca","Cluj Napoca")
tidy("Cochstedt","Magdeburg Cochstedt")
tidy("Cotswold","Cotswold Apt - Kemble")
tidy("Deer Lake","Deer Lake (Newfoundland)")
tidy("Dnipropetrovsk","Dnepropetrovsk")
tidy("Domodedovo","Moscow (Domodedovo)")
tidy("Don Mueang","Bangkok (Don Muang)")
tidy("Eduardo Gomes","Manaus-Eduardo Gomes")
#tidy("Eelde","Den Helder")
tidy("Egilssta?ir","Egilsstadir")
#tidy("Eilat","Elat")
tidy("Enfidha - Hammamet","Enfidha - Hammamet Intl")
#tidy("Eros","Roros")
tidy("Es Senia","Oran Es Senia")
tidy("Esenboga","Ankara (Esenboga)")
tidy("Frank Pais","Holguin (Frank Pais)")
tidy("Geilo Dagali","Geilo (Dagali)")
#tidy("Gimpo","Seoul (Kimpo)")
tidy("Gold Coast","Coolangatta (Gold Coast)")
tidy("Gorna Oryahovitsa","Gorna Orechovitsa")
#tidy("Ha'il","Hamilton (Canada)")
tidy("Halim Perdanakusuma","Jakarta (Halim Perdana Kusuma)")
tidy("Hamad","Doha Hamad")
tidy("Hannover","Hanover")
#tidy("Hato","Hanover")
tidy("Hewanorra","St Lucia (Hewanorra)")
tidy("Hong Kong","Hong Kong (Chek Lap Kok)")
tidy("Horta","Azores Horta")
tidy("Ibn Batouta","Tangiers (Ibn Batuta)")
tidy("Imam Khomeini","Tehran Imam Khomeini")
tidy("Imsik","Bodrum (Imsik)")
tidy("Incheon","Seoul (Incheon)")
tidy("Ingolstadt Manching","Ingolstadt-Manching")
tidy("Ireland West Knock","Ireland West(Knock)")
tidy("Ivano-Frankivsk","Ivano-Frankovsk")
#tidy("Ivato","Ivalo")
#tidy("Kaduna","Kaunas")
#tidy("Kansai","Kansas City")
tidy("Karlsruhe Baden-Baden","Karlsruhe/Baden Baden")
#tidy("Kerry","Londonderry")
tidy("Khanty Mansiysk","Khanty-Mansiysk")
#tidy("Kharkiv","Kharkov Osnova Intl")
tidy("Kiev Zhuliany","Kiev (Zhulyany)")
tidy("Kisumu","Kisuma") #<new spleeing wrong??
tidy("Kristiansand","Kristiansand (Kjevik)")
tidy("Kristiansund (Kvernberget)","Kristiansund (Kuernberget)")
tidy("Kuala Lumpur","Kuala Lumpur (Sepang)")
tidy("La Guardia","New York (La Guardia)")
tidy("La Palma","Las Palmas")
tidy("Lajes","Azores Lajes Terceira Island")
tidy("Lamezia Terme","Lametia-Terme")
tidy("Leirin","Fagernes/Leirin")
tidy("Lerwick / Tingwall","Lerwick (Tingwall)")
tidy("Limnos","Lemnos")
#tidy("Lista","Boa Vista (Rabil)")
#tidy("London","Londonderry")
#tidy("Lublin","Dublin")
#tidy("Lviv","Liverpool (John Lennon)")
tidy("Lyon-Bron","Lyon(Bron)")
#tidy("Macau","Manaus-Eduardo Gomes")
tidy("Malpensa","Milan (Malpensa)")
#tidy("Manas","Monastir")
#tidy("Maun","Stauning")
tidy("Mersa Matruh","Mersa Matrouh")
tidy("Milano Linate","Milan (Linate)")
tidy("Mineralnyye Vody","Mineralnye Vody")
#tidy("Modlin","Moline (Quad City)")
tidy("Monchengladbach","Moenchengladbach")
#tidy("Moss","Kinloss")
tidy("Mukalla","Riyan Mukalla")
tidy("Munster Osnabruck","Munster-Osnabruck")
#tidy("Naha","Omaha")
tidy("Narita","Tokyo (Narita)")
tidy("Nashville","Nashville Metropolitan")
tidy("Ndjili","Kinshasa Ndjili")
tidy("Nottingham","Tollerton Nottingham")
#tidy("Obock","Lubbock")
#tidy("Osaka","Lusaka")
tidy("Oslo","Oslo (Fornebu)")
tidy("Pajala","Pajala Yllas")
tidy("Palm Beach","West Palm Beach")
tidy("Paris-Le Bourget","Paris (Le Bourget)")
tidy("Perigueux-Bassillac","Perigeux/Bassillac")
tidy("Perth","Perth (Uk)")
tidy("Portland","Portland (Oregon)")
tidy("Quad City","Moline (Quad City)")
tidy("Rabil","Boa Vista (Rabil)")
tidy("Rajiv Gandhi","Hyderabad ( Rajiv Ghandi )")
tidy("Rickenbacker","Columbus Rickenbacker Afb")
tidy("Roberts","Monrovia (Roberts)")
tidy("Rochester","Rochester (Uk)")
tidy("Sabha","Istanbul (Sabiha Gokcen)")
tidy("Sabiha Gokcen","Istanbul (Sabiha Gokcen)")
tidy("Salerno Costa d'Amalfi","Salerno Costa Amalfi")
tidy("Samana El Catey","Samana (El Catey)")
tidy("Samedan","Samedan/St Moritz")
tidy("San Bernardino","San Bernardino (Norton Afb)")
tidy("San Javier","Murcia San Javier")
tidy("Sana'a","Sanaa")
tidy("Sandefjord","Sandefjord(Torp)")
tidy("Santa Maria","Azores Santa Maria")
tidy("Santiago de Compostela","Santiago De Compostela")
tidy("Santorini","Thira (Santorini)")
tidy("Santos Dumont","Rio De Janeiro (Santos Dumont)")
tidy("Sao Pedro","San Pedro (Cape Verde)")
tidy("Sarasota Bradenton","Sarasota (Bradenton)")
tidy("Sarmellek","Sarmellek/Balaton")
tidy("Schwerin Parchim","Schwerin/Parchim")
tidy("Seattle Tacoma","Seattle (Tacoma)")
tidy("Sevilla","Seville")
tidy("Sharm El Sheikh","Sharm El Sheikh (Ophira)")
tidy("Sheremetyevo","Moscow (Sheremetyevo)")
tidy("Siegerland","Siegerlands")
tidy("Sochi","Adler / Sochi")
tidy("Soekarno-Hatta","Jakarta (Soekarno-Hatta Intnl)")
tidy("St Louis Lambert","St Louis (Lambert)")
tidy("Stockholm-Arlanda","Stockholm (Arlanda)")
tidy("Stockholm-Bromma","Stockholm (Bromma)")
tidy("Stockholm Skavsta","Stockholm (Skavsta)")
tidy("Sulaymaniyah","Sulaymaniyah Int")
tidy("Suvarnabhumi","Bangkok Suvarnabhumi")
tidy("Svalbard","Longyearbyen (Svalbard)")
tidy("Tekirdag Corlu","Tekirdag (Corlu)")
tidy("Tenerife Norte","Tenerife (Norte Los Rodeos)")
tidy("Tokyo Haneda","Tokyo (Haneda)")
tidy("Tolmachevo","Novosibirsk (Tolmachevo)")
tidy("Toulouse-Blagnac","Toulouse (Blagnac)")
tidy("Tromso,","Tromsoe")
tidy("Trondheim Varnes","Trondheim (Vaernes)")
tidy("Twente","Enschede (Twente)")
tidy("U-Tapao","u-Tapao")
#tidy("Victoria","Vitoria")
tidy("Vnukovo","Moscow (Vnukovo)")
tidy("Warsaw Chopin","Warsaw (Chopin)")
tidy("Washington Dulles","Washington (Dulles)")
tidy("Westerland Sylt","Westerland (Sylt)")
tidy("Wevelgem","Kortrijk/Wevelgem")
tidy("Yangon","Yangon/Rangoon")
tidy("Yaounde Nsimalen","Yaounde (Nsimalen)")
tidy("Zweibrucken","Zweibruken")


airports_flights <- flight_od[,c("airport2","airport2_country","tx","ty")]
airports_flights <- unique(airports_flights)
airports_flights <- airports_flights[!airports_flights$airport2 %in% pass_od$airport2,]
airports_flights <- st_as_sf(airports_flights, coords = c("tx","ty"), crs = 4326)

airports <- read_sf("data/airports_pass.gpkg")
airports$full <- NULL
airports$airport <- tools::toTitleCase(airports$airport)
airports$country <- tools::toTitleCase(airports$country)
airports_extra <- data.frame(airport = "Coningsby", country = "United Kingdom", lon = -0.166111, lat = 53.093056, stringsAsFactors = FALSE)
airports_extra <- st_as_sf(airports_extra, coords = c("lon","lat"), crs = 4326)
names(airports_extra) <- names(airports)
st_geometry(airports_extra) <- "geom"
airports <- rbind(airports, airports_extra)

tidy2 <- function(from,to){
  airports$airport[airports$airport %in% from] <-  to
  assign('airports',airports,envir=.GlobalEnv)
}

tidy2("Aberdeen International","Aberdeen")
tidy2("Belfast International","Belfast")
#tidy2("Teesside","Durham Tees Valley")
tidy2("Exeter International","Exeter")
tidy2("Kent International","Kent")              
#tidy2("Liverpool (John Lennon)","Liverpool")
#tidy2("Teesside","Teesside")
tidy2("londonderry","City of Derry (Eglinton)")
tidy2("Belfast City","Belfast City (George Best)")
tidy2("Tollerton Nottingham","Nottingham East Midlands Int'l")
#tidy2("","Manston (Kent Int)")
#tidy2("Wick","Wick John o Groats")
#tidy2("","Coningsby")


summary(pass_od$airport1_country %in% airports$country)
summary(pass_od$airport2_country %in% airports$country)
summary(pass_od$airport1 %in% airports$airport)
summary(pass_od$airport2 %in% airports$airport)
unique(pass_od$airport2[!pass_od$airport2 %in% airports$airport])

stop()

# Match locations
mtch <- match(airports_flights$geometry, airports$geom)
airports_flights$airport_match <- airports$airport[mtch]
airports_flights$country_match <- airports$country[mtch]


dists <- st_distance(airports_flights, airports)
dists <- matrix(as.numeric(dists), ncol = ncol(dists))
rownames(dists) <- airports_flights$airport2
colnames(dists) <- airports$airport
dists[dists == 0] <- NA
dists[dists > 1000] <- NA

dists <- stplanr::odmatrix_to_od(dists)
qtm(airports[airports$airport %in% dists$dest,]) +
  qtm(airports_flights[airports_flights$airport2 %in% dists$orig,], dots.col = "red")

stop()

foo = flight_od$airport2[!flight_od$airport2 %in% pass_od$airport2]
foo = unique(foo)
foo = foo[order(foo)]

# try fuzzy match
# match_vec <- unique(pass_od$airport2)
# bar = pbapply::pblapply(foo, function(x){
#   x <- agrep(x, match_vec, ignore.case = FALSE, value = TRUE, max.distance = 0.1)
#   if(length(x) == 0){
#     x <- NA
#   } else{
#     x <- x[1]
#   }
#   return(x)
# })
# bar = unlist(bar)
# comp <- data.frame(foo = foo, bar = bar)
#comp$tidy <- paste0('tidy("',comp$foo,'","',comp$bar,'")')
#comp <- comp[!is.na(comp$bar),]
#comp <- comp[,3, drop = FALSE]



dists <- st_distance(airports_flights, airports)
dists <- matrix(as.numeric(st_distance(airports_flights, airports)), nrow = nrow(airports_flights))
dists[dists == 0] <- NA
dists[dists > 10000] <- NA
colnames(dists) <- airports$airport
rownames(dists) <- airports_flights$airport2
min(dists, na.rm = TRUE)

dists <- dists[rowSums(is.na(dists)) != ncol(dists), ]
dists <- dists[,colSums(is.na(dists)) != nrow(dists)]

airports_close <- airports_flights[airports_flights$airport2 %in% rownames(dists),]
qtm(airports_close)


all_od <- left_join(pass_od, flight_od, by= c("airport1","airport1_country","airport2","airport2_country") )



airports$airport <- tools::toTitleCase(airports$airport)
airports$country <- tools::toTitleCase(airports$country)