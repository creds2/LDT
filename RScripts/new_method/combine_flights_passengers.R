library(sf)
library(dplyr)

pass_od <- readRDS("data/clean/passenger_od_wide.Rds")
flight_od <- readRDS("data/clean/flights_od_prepped.Rds")

# Load Airports
airports <- read_sf("data/airports_pass.gpkg")
airports$full <- NULL
airports$airport <- tools::toTitleCase(airports$airport)
airports$country <- tools::toTitleCase(airports$country)
airports_extra <- data.frame(airport = c("Coningsby","Tollerton Nottingham", "Elstree","Colonsay","Cotswold Apt - Kemble",
                                         "Gamston","Coll"), 
                             country = c("United Kingdom","United Kingdom","United Kingdom","United Kingdom","United Kingdom",
                                         "United Kingdom","United Kingdom"),
                             lon = c(-0.166111,-1.081156,-0.327250,-6.2430556,-2.056944,-0.957807,-6.617778), 
                             lat = c(53.093056,52.919126,51.655607,56.0575,51.668056, 53.277311,56.601944 ), 
                             stringsAsFactors = FALSE)
airports_extra <- st_as_sf(airports_extra, coords = c("lon","lat"), crs = 4326)
names(airports_extra) <- names(airports)
st_geometry(airports_extra) <- "geom"
airports <- rbind(airports, airports_extra)

airports$airport <- gsub(" International","",airports$airport)
airports$airport <- gsub(" Int'l","",airports$airport)
airports$airport <- gsub(" Intl","",airports$airport)
airports$airport <- gsub(" Int","",airports$airport)
airports$airport <- iconv(airports$airport, from="UTF-8", to="ASCII//TRANSLIT")


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

# Clean Values

tidy_airports("Durham Tees Valley", "Teesside")
tidy_airports("Wick John o Groats", "Wick")
tidy_airports("City of Derry (Eglinton)", "Londonderry")
tidy_airports("Manston (Kent)", "Kent")
tidy_airports("Belfast City","Belfast City (George Best)")
tidy_airports("Liverpool",	"Liverpool (John Lennon)")
tidy_airports("Cardiff Wales","Cardiff")
tidy_airports("Teesside Airport","Teesside")

tidy_airports("Aberdeen Dyce", "Aberdeen")
tidy_airports("Birmingham International", "Birmingham")
tidy_airports("Cardiff International", "Cardiff")
tidy_airports("George Best Belfast City", "Belfast City (George Best)")
tidy_airports("Glasgow International", "Glasgow")
tidy_airports("London Gatwick", "Gatwick")
tidy_airports("London Heathrow", "Heathrow")
tidy_airports("London Luton", "Luton")
tidy_airports("London Stansted", "Stansted")
tidy_airports("Robin Hood Doncaster Sheffield", "Doncaster Sheffield")
tidy_airports("Liverpool John Lennon", "Liverpool (John Lennon)")
tidy_airports("RAF Ascension Island", "Ascension Island")
tidy_airports("Nottingham East Midlands", "East Midlands")

tidy_airports("A Coruna","a Coruna")
tidy_airports("Aarhus","Aarhus (Tirstrup)")
tidy_airports("Adnan Menderes","Izmir (Adnan Menderes)")
tidy_airports("Albert-Bray","Albert - Bray")
tidy_airports("Alghero-Fertilia","Alghero (Fertilia)")
tidy_airports("Anglesey","Anglesey (Valley)")
tidy_airports("Ascension","Ascension Island")
tidy_airports("Asturias","Asturias (Aviles)")
tidy_airports("Augsburg","Augsburg/Muelhausen")
tidy_airports("Austin Bergstrom","Austin (Bergstrom)")
tidy_airports("Baghdad","Baghdad Int")
tidy_airports("Bahias de Huatulco","Bahias De Huatulco")
tidy_airports("Banak","Bangkok (Don Muang)")
tidy_airports("Bateen","Abu Dhabi - Bateen")
tidy_airports("Berlin-Schonefeld","Berlin (Schonefeld)")
tidy_airports("Berlin-Tegel","Berlin (Tegel)")
tidy_airports("Borg El Arab","Alexandria (Borg El Arab)")
tidy_airports("Bradley","Windsor Locks Bradley Intl")
tidy_airports("Brescia","Brescia/Montichiari")
tidy_airports("Brno-Turany","Brno (Turany)")
tidy_airports("Cagliari Elmas","Cagliari (Elmas)")
tidy_airports("Castellon-Costa Azahar","Castellón Costa Azahar")
tidy_airports("Catania-Fontanarossa","Catania (Fontanarossa)")
tidy_airports("Charles de Gaulle","Paris (Charles De Gaulle)")
tidy_airports("Chicago Midway","Chicago (Midway)")
tidy_airports("Chicago O'Hare","Chicago (O'hare)")
tidy_airports("Chisinau","Chisinau (Kishinev)")
tidy_airports("Cluj-Napoca","Cluj Napoca")
tidy_airports("Cochstedt","Magdeburg Cochstedt")
tidy_airports("Cotswold","Cotswold Apt - Kemble")
tidy_airports("Deer Lake","Deer Lake (Newfoundland)")
tidy_airports("Dnipropetrovsk","Dnepropetrovsk")
tidy_airports("Domodedovo","Moscow (Domodedovo)")
tidy_airports("Don Mueang","Bangkok (Don Muang)")
tidy_airports("Eduardo Gomes","Manaus-Eduardo Gomes")
tidy_airports("Egilssta?ir","Egilsstadir")
tidy_airports("Enfidha - Hammamet","Enfidha - Hammamet Intl")
tidy_airports("Es Senia","Oran Es Senia")
tidy_airports("Esenboga","Ankara (Esenboga)")
tidy_airports("Frank Pais","Holguin (Frank Pais)")
tidy_airports("Geilo Dagali","Geilo (Dagali)")
tidy_airports("Gold Coast","Coolangatta (Gold Coast)")
tidy_airports("Gorna Oryahovitsa","Gorna Orechovitsa")
tidy_airports("Halim Perdanakusuma","Jakarta (Halim Perdana Kusuma)")
tidy_airports("Hamad","Doha Hamad")
tidy_airports("Hannover","Hanover")
tidy_airports("Hewanorra","St Lucia (Hewanorra)")
tidy_airports("Hong Kong","Hong Kong (Chek Lap Kok)")
tidy_airports("Horta","Azores Horta")
tidy_airports("Ibn Batouta","Tangiers (Ibn Batuta)")
tidy_airports("Imam Khomeini","Tehran Imam Khomeini")
tidy_airports("Imsik","Bodrum (Imsik)")
tidy_airports("Incheon","Seoul (Incheon)")
tidy_airports("Ingolstadt Manching","Ingolstadt-Manching")
tidy_airports("Ireland West Knock","Ireland West(Knock)")
tidy_airports("Ivano-Frankivsk","Ivano-Frankovsk")
tidy_airports("Karlsruhe Baden-Baden","Karlsruhe/Baden Baden")
tidy_airports("Khanty Mansiysk","Khanty-Mansiysk")
tidy_airports("Kiev Zhuliany","Kiev (Zhulyany)")
tidy_airports("Kisuma","Kisumu")
tidy_airports("Kristiansand","Kristiansand (Kjevik)")
tidy_airports("Kristiansund (Kvernberget)","Kristiansund (Kuernberget)")
tidy_airports("Kuala Lumpur","Kuala Lumpur (Sepang)")
tidy_airports("La Guardia","New York (La Guardia)")
tidy_airports("La Palma","Las Palmas")
tidy_airports("Lajes","Azores Lajes Terceira Island")
tidy_airports("Lamezia Terme","Lametia-Terme")
tidy_airports("Leirin","Fagernes/Leirin")
tidy_airports("Lerwick / Tingwall","Lerwick (Tingwall)")
tidy_airports("Limnos","Lemnos")
tidy_airports("Lyon-Bron","Lyon(Bron)")
tidy_airports("Malpensa","Milan (Malpensa)")
tidy_airports("Mersa Matruh","Mersa Matrouh")
tidy_airports("Milano Linate","Milan (Linate)")
tidy_airports("Mineralnyye Vody","Mineralnye Vody")
tidy_airports("Monchengladbach","Moenchengladbach")
tidy_airports("Mukalla","Riyan Mukalla")
tidy_airports("Munster Osnabruck","Munster-Osnabruck")
tidy_airports("Narita","Tokyo (Narita)")
tidy_airports("Nashville","Nashville Metropolitan")
tidy_airports("Ndjili","Kinshasa Ndjili")
tidy_airports("Nottingham","Tollerton Nottingham")
tidy_airports("Oslo","Oslo (Fornebu)")
tidy_airports("Pajala","Pajala Yllas")
tidy_airports("Palm Beach","West Palm Beach")
tidy_airports("Paris-Le Bourget","Paris (Le Bourget)")
tidy_airports("Perigueux-Bassillac","Perigeux/Bassillac")
tidy_airports("Perth (Uk)","Perth")
tidy_airports("Portland","Portland (Oregon)")
tidy_airports("Quad City","Moline (Quad City)")
tidy_airports("Rabil","Boa Vista (Rabil)")
tidy_airports("Rajiv Gandhi","Hyderabad ( Rajiv Ghandi )")
tidy_airports("Rickenbacker","Columbus Rickenbacker Afb")
tidy_airports("Roberts","Monrovia (Roberts)")
tidy_airports("Rochester","Rochester (Uk)")
tidy_airports("Sabha","Istanbul (Sabiha Gokcen)")
tidy_airports("Sabiha Gokcen","Istanbul (Sabiha Gokcen)")
tidy_airports("Salerno Costa d'Amalfi","Salerno Costa Amalfi")
tidy_airports("Samana El Catey","Samana (El Catey)")
tidy_airports("Samedan","Samedan/St Moritz")
tidy_airports("San Bernardino","San Bernardino (Norton Afb)")
tidy_airports("San Javier","Murcia San Javier")
tidy_airports("Sana'a","Sanaa")
tidy_airports("Sandefjord","Sandefjord(Torp)")
tidy_airports("Santa Maria","Azores Santa Maria")
tidy_airports("Santiago de Compostela","Santiago De Compostela")
tidy_airports("Santorini","Thira (Santorini)")
tidy_airports("Santos Dumont","Rio De Janeiro (Santos Dumont)")
tidy_airports("Sao Pedro","San Pedro (Cape Verde)")
tidy_airports("Sarasota Bradenton","Sarasota (Bradenton)")
tidy_airports("Sarmellek","Sarmellek/Balaton")
tidy_airports("Schwerin Parchim","Schwerin/Parchim")
tidy_airports("Seattle Tacoma","Seattle (Tacoma)")
tidy_airports("Sevilla","Seville")
tidy_airports("Sharm El Sheikh","Sharm El Sheikh (Ophira)")
tidy_airports("Sheremetyevo","Moscow (Sheremetyevo)")
tidy_airports("Siegerland","Siegerlands")
tidy_airports("Sochi","Adler / Sochi")
tidy_airports("Soekarno-Hatta","Jakarta (Soekarno-Hatta Intnl)")
tidy_airports("St Louis Lambert","St Louis (Lambert)")
tidy_airports("Stockholm-Arlanda","Stockholm (Arlanda)")
tidy_airports("Stockholm-Bromma","Stockholm (Bromma)")
tidy_airports("Stockholm Skavsta","Stockholm (Skavsta)")
tidy_airports("Sulaymaniyah","Sulaymaniyah Int")
tidy_airports("Suvarnabhumi","Bangkok Suvarnabhumi")
tidy_airports("Svalbard","Longyearbyen (Svalbard)")
tidy_airports("Tekirdag Corlu","Tekirdag (Corlu)")
tidy_airports("Tenerife Norte","Tenerife (Norte Los Rodeos)")
tidy_airports("Tokyo Haneda","Tokyo (Haneda)")
tidy_airports("Tolmachevo","Novosibirsk (Tolmachevo)")
tidy_airports("Toulouse-Blagnac","Toulouse (Blagnac)")
tidy_airports("Tromso,","Tromsoe")
tidy_airports("Trondheim Varnes","Trondheim (Vaernes)")
tidy_airports("Twente","Enschede (Twente)")
tidy_airports("U-Tapao","u-Tapao")
tidy_airports("Vnukovo","Moscow (Vnukovo)")
tidy_airports("Warsaw Chopin","Warsaw (Chopin)")
tidy_airports("Washington Dulles","Washington (Dulles)")
tidy_airports("Westerland Sylt","Westerland (Sylt)")
tidy_airports("Wevelgem","Kortrijk/Wevelgem")
tidy_airports("Yangon","Yangon/Rangoon")
tidy_airports("Yaounde Nsimalen","Yaounde (Nsimalen)")
tidy_airports("Zweibrucken","Zweibruken")

tidy_airports("Aberdeen International","Aberdeen")
tidy_airports("Belfast International","Belfast")
tidy_airports("Exeter International","Exeter")
tidy_airports("Kent International","Kent")              
tidy_airports("Belfast City","Belfast City (George Best)")
tidy_airports("Rygge","Moss")
tidy_airports("Evenes","Harstad/Narvik")
tidy_airports("Singapore","Singapore Changi")
tidy_airports("Frankfurt Main","Frankfurt am Main")
tidy_airports("Adolfo Suarez Madrid-Barajas","Madrid")
tidy_airports("Geneva Cointrin","Geneva")
tidy_airports("Nice","Nice-Cote d'Azur")
tidy_airports("George Bushercontinental Houston","Houston")
tidy_airports("Murtala Muhammed","Lagos")	
tidy_airports("Humberto Delgado (Lisbon Portela)","Lisbon")	
tidy_airports(c("Francisco de Sa Carneiro","Oporto"),"Porto")	
tidy_airports("Bordeaux","Bordeaux-Merignac")	
tidy_airports("Beirut Rafic Hariri","Beirut")
tidy_airports("Bale Mulhouse","EuroAirport Basel-Mulhouse-Freiburg")
tidy_airports("Lyon","Lyon Saint-Exupery")
tidy_airports("King Abdulaziz","Jeddah")
tidy_airports("Phoenix Sky Harbor","Phoenix")
tidy_airports("Addis Ababa","Addis Ababa Bole")
tidy_airports("Amman","Amman-Marka")
tidy_airports("Luxembourg","Luxembourg-Findel")
tidy_airports("Prestwick","Glasgow Prestwick")
tidy_airports("Chambery","Chambery-Savoie")
tidy_airports("Plymouth City","Plymouth")
tidy_airports("Maastricht","Maastricht Aachen")
tidy_airports("Ostend","Ostend-Bruges")
tidy_airports("Prestwick","Glasgow Prestwick")
tidy_airports("Northolt","RAF Northolt")
tidy_airports("V.C. Bird","Antigua")
tidy_airports("Mahon","Menorca")
tidy_airports("Montpellier","Montpellier-Mediterranee")
tidy_airports("Sir Seewoosagur Ramgoolam","Mauritius")
tidy_airports("Reus","Reus Air Base")
tidy_airports("Casablanca","Casablanca Mohamed v")
tidy_airports("Islamabad","Benazir Bhutto")
tidy_airports("Castellón Costa Azahar","Castellon Costa Azahar")
tidy_airports("Queen Beatrix","Aruba")
tidy_airports("Nimes","Nimes-Arles-Camargue")
tidy_airports("Brive-La-Gaillarde","Brive Souillac")
tidy_airports("Perth (Australia)","Perth")
tidy_airports("Hong Kong (Chep Lap Kok)","Hong Kong (Chek Lap Kok)")
tidy_airports("Ireland West Airport Knock","Ireland West(Knock)")
tidy_airports("Camp Springs","Camp Springs (Andrews Afb)")



tidy_countries("Isle of Curacao Neth.antilles","Netherlands Antilles")
tidy_countries("Burkino Faso","Burkina Faso")
tidy_countries("United States","United States of America")
tidy_countries("Ireland","Irish Republic")
tidy_countries("Bosnia and Herzegovina","Bosnia-Herzegovina")
tidy_countries("South Korea","Republic of Korea")
tidy_countries("Serbia","Republic of Serbia")
tidy_countries("Slovakia","Slovak Republic")
tidy_countries("Cape Verde","Cape Verde Islands")
tidy_countries("Moldova","Republic of Moldova")
tidy_countries("Montenegro","Republic of Montenegro")
tidy_countries("Saint Lucia","St Lucia")
tidy_countries("Maldives","Maldive Islands")
tidy_countries("South Africa","Republic of South Africa")
tidy_countries("Cote d'Ivoire","Ivory Coast")
tidy_countries("Yemen","Republic of Yemen")
tidy_countries("Congo (Brazzaville)","Congo")
tidy_countries("Djibouti","Djibouti Republic")
tidy_countries("Congo (Kinshasa)","Democratic Republic of Congo")
tidy_countries("French Polynesia","Tahiti")
tidy_countries("Virgin Islands","Virgin Islands (U.s.a)")
tidy_countries("Burma","Myanmar")
tidy_countries("North Korea","Democratic People Rep.of Korea")


tidy_countries(c("Spain(Canary Islands)", "Spain (Canary Islands)", 
                 "Spain (Excluding Canary Islands)", "Spain(Excluding Canary Islands)"),"Spain")

tidy_countries(c("USA","Usa"),"United States of America")
tidy_countries(c("Portugal(Excluding Madeira)", "Portugal(Excluding Madeira)", "Portugal (Madeira)"),"Portugal")
tidy_countries("Nationalist China (Taiwan)","Taiwan")


tidy_countries(c("Fed Rep Yugosl',s'bia,m'enegro","Fed Rep Yugo Serbia M'enegro","Fed Rep Yugo Serbia M'enegr0"),"Republic of Serbia")
tidy_countries("Evenes","Norway")
tidy_countries("Rygge","Norway")
tidy_countries("Longyear","Norway")
tidy_countries("Fornebu Airport","Norway")
tidy_countries("Torp","Norway")
tidy_countries("Point Salines","Grenada")
tidy_countries("Belorus","Belarus")



flight_od$airport2_country[flight_od$airport2 == "Ascension Island"] <- "Ascension Island"
flight_od$airport2_country[flight_od$airport2 == "Pristina"] <- "Kosovo"
pass_od$airport1_country[pass_od$airport1 == "Jersey"] <- "Jersey"
pass_od$airport2_country[pass_od$airport2 == "Jersey"] <- "Jersey"
airports$country[airports$airport == "Jersey"] <- "Jersey"
pass_od$airport1_country[pass_od$airport1 == "Guernsey"] <- "Guernsey"
pass_od$airport2_country[pass_od$airport2 == "Guernsey"] <- "Guernsey"
airports$country[airports$airport == "Guernsey"] <- "Guernsey"
pass_od$airport1_country[pass_od$airport1 == "Isle of Man"] <- "Isle of Man"
pass_od$airport2_country[pass_od$airport2 == "Isle of Man"] <- "Isle of Man"
airports$country[airports$airport == "Isle of Man"] <- "Isle of Man"


flight_od$airport1_country[flight_od$airport1 == "EuroAirport Basel-Mulhouse-Freiburg"] <- "France"
flight_od$airport2_country[flight_od$airport2 == "EuroAirport Basel-Mulhouse-Freiburg"] <- "France"
pass_od$airport1_country[pass_od$airport1 == "EuroAirport Basel-Mulhouse-Freiburg"] <- "France"
pass_od$airport2_country[pass_od$airport2 == "EuroAirport Basel-Mulhouse-Freiburg"] <- "France"
airports$country[airports$airport == "EuroAirport Basel-Mulhouse-Freiburg"] <- "France"

pass_od$airport1_country[pass_od$airport1 == "Asmara"] <- "Eritrea"
pass_od$airport2_country[pass_od$airport2 == "Asmara"] <- "Eritrea"
airports$country[airports$airport == "Asmara"] <- "Eritrea"

pass_od$airport1_country[pass_od$airport1 == "Bangkok (Don Muang)"] <- "Thailand"
pass_od$airport2_country[pass_od$airport2 == "Bangkok (Don Muang)"] <- "Thailand"
flight_od$airport1_country[flight_od$airport1 == "Bangkok (Don Muang)"] <- "Thailand"
flight_od$airport2_country[flight_od$airport2 == "Bangkok (Don Muang)"] <- "Thailand"
airports$country[airports$airport == "Bangkok (Don Muang)"] <- "Thailand"

pass_od$airport1_country[pass_od$airport1 == "Aruba"] <- "Aruba"
pass_od$airport2_country[pass_od$airport2 == "Aruba"] <- "Aruba"
flight_od$airport1_country[flight_od$airport1 == "Aruba"] <- "Aruba"
flight_od$airport2_country[flight_od$airport2 == "Aruba"] <- "Aruba"
airports$country[airports$airport == "Aruba"] <- "Aruba"


pass_od$airport1_country[pass_od$airport1 == "Tivat"] <- "Republic of Serbia"
pass_od$airport2_country[pass_od$airport2 == "Tivat"] <- "Republic of Serbia"
flight_od$airport1_country[flight_od$airport1 == "Tivat"] <- "Republic of Serbia"
flight_od$airport2_country[flight_od$airport2 == "Tivat"] <- "Republic of Serbia"
airports$country[airports$airport == "Tivat"] <- "Republic of Serbia"

flight_od$airport2_country[flight_od$airport2 == "Istanbul (Sabiha Gokcen)"] <- "Turkey"

# Sort order of doemstic flights

for(i in 1:nrow(pass_od)){
  if(pass_od$airport1_country[i] %in% c("United Kingdom","Jersey","Isle of Man","Guernsey") & 
     pass_od$airport2_country[i] %in% c("United Kingdom","Jersey","Isle of Man","Guernsey")){
    ap1 <- pass_od$airport1[i]
    ap2 <- pass_od$airport2[i]
    apc1 <- pass_od$airport1_country[i]
    apc2 <- pass_od$airport2_country[i]
    
    if(ap2 < ap1){
      pass_od$airport1_country[i] <- apc2
      pass_od$airport2_country[i] <- apc1
      pass_od$airport1[i] <- ap2
      pass_od$airport2[i] <- ap1
    }
  }
}

for(i in 1:nrow(flight_od)){
  if(flight_od$airport1_country[i] %in% c("United Kingdom","Jersey","Isle of Man","Guernsey") & 
     flight_od$airport2_country[i] %in% c("United Kingdom","Jersey","Isle of Man","Guernsey")){
    ap1 <- flight_od$airport1[i]
    ap2 <- flight_od$airport2[i]
    apc1 <- flight_od$airport1_country[i]
    apc2 <- flight_od$airport2_country[i]
    
    if(ap2 < ap1){
      flight_od$airport1_country[i] <- apc2
      flight_od$airport2_country[i] <- apc1
      flight_od$airport1[i] <- ap2
      flight_od$airport2[i] <- ap1
    }
  }
}
rm(ap1,ap2,apc1,apc2,i)

# CLean duplicated flows now we know the idetified airports

pass_od2 <- pass_od %>%
  group_by(airport1,airport1_country,airport2,airport2_country) %>%
  summarise_all(sum, na.rm = TRUE)

flight_od2 <- flight_od %>%
  group_by(airport1,airport1_country,airport2,airport2_country) %>%
  summarise_all(sum, na.rm = TRUE)



all_od <- left_join(pass_od2, flight_od2, by= c("airport1","airport1_country","airport2","airport2_country") )

# check for odd results
all_missing_flights <- all_od[is.na(all_od$flt_2018), ]
foo <- as.data.frame(table(all_od$airport1[is.na(all_od$flt_2018)]))
bar <- as.data.frame(table(all_od$airport1[!is.na(all_od$flt_2018)]))
table(flight_od$airport1)

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
all_missing_passengers <- all_missing_passengers[is.na(all_missing_passengers$`2018`), ]
all_missing_passengers <- all_missing_passengers %>%
  group_by(airport2, airport2_country) %>%
  summarise(total = sum(flt_2018),
            count = n())
