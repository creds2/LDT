library(dplyr)
library(sf)
library(ggmap)
library(tmap)
tmap_mode("view")
# Read in

#pass_transit <- readRDS("data/CAA_transit.Rds")
pass_int_od <- readRDS("data/CAA_int_od.Rds")
pass_dom_od <- readRDS("data/CAA_dom_od.Rds")

airports_fixed <- readRDS("airports_final.Rds")
airports2 <- readRDS("airports.Rds")
#flights_flow <- readRDS("flow_final.Rds")

# Remove totals
pass_int_od <- pass_int_od[!grepl("Total",pass_int_od$uk_airport),]

# get the locations of the passenger airports
pass_int_od$foreign_airport <- tolower(pass_int_od$foreign_airport)
pass_int_od$foreign_country <- tolower(pass_int_od$foreign_country)

pass_int_od$foreign_airport <- gsub(" airport","",pass_int_od$foreign_airport)

# fix typo differences
pass_int_od$foreign_country[pass_int_od$foreign_airport == "aarhus"] <- "denmark"
pass_int_od$foreign_country[pass_int_od$foreign_country == "rumania"] <- "romania"
pass_int_od$foreign_country[pass_int_od$foreign_country == "fed rep yugo serbia m'enegro"] <- "republic of serbia"

pass_int_od$foreign_country[pass_int_od$foreign_airport == "podgorica"] <- "republic of montenegro"
pass_int_od$foreign_country[pass_int_od$foreign_airport == "pristina"] <- "kosovo"
pass_int_od$foreign_country[pass_int_od$foreign_airport == "podgorica"] <- "republic of montenegro"
pass_int_od$foreign_country[pass_int_od$foreign_airport == "tivat"] <- "republic of montenegro"
pass_int_od$foreign_country[pass_int_od$foreign_airport == "kosovo"] <- "kosovo"
pass_int_od$foreign_country[pass_int_od$foreign_airport == "republic of montenegro"] <- "republic of montenegro"
pass_int_od$foreign_country[pass_int_od$foreign_airport == "taipei"] <- "taiwan"

pass_int_od$foreign_airport[is.na(pass_int_od$foreign_airport)] <- "="

pass_int_od$foreign_airport[pass_int_od$foreign_airport == "ireland west knock"] <- "ireland west(knock)"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "cologne (bonn)"] <- "cologne bonn"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "kavalla"] <- "kavala"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "gerona"] <- "girona"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "la coruna"] <- "a coruna"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "basle mulhouse"] <- "bale mulhouse" #### should be basle mulhouse
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "izmir (adnam menderes)"] <- "izmir (adnan menderes)"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "izmir (adnam menderes)"] <- "International Airport Kharkiv"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "hong kong (chep lap kok)"] <- "hong kong (chek lap kok)"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "szczecin (golenow)"] <- "Port Lotniczy Szczecin-Goleniow"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "kerry coun2001"] <- "kerry county"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "santiago de compostela"] <- "Santiago de Compostela"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "baden baden"] <- "karlsruhe/baden baden"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "villafranca"] <- "verona villafranca"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "st moritz"] <- "samedan/st moritz"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "kishinev"] <- "chisinau (kishinev)"
pass_int_od$foreign_airport[pass_int_od$foreign_airport == "imam khomieni"] <- "tehran imam khomeini"

#fix missing countires
pass_int_od$foreign_country[is.na(pass_int_od$foreign_country) & pass_int_od$foreign_airport %in% c("innsbruck","salzburg","vienna")] <- "austria"
pass_int_od$foreign_country[is.na(pass_int_od$foreign_country) & pass_int_od$foreign_airport %in% c("antwerp","brussels")] <- "belgium"
pass_int_od$foreign_country[is.na(pass_int_od$foreign_country) & pass_int_od$foreign_airport %in% c("copenhagen","billund","esbjerg","aarhus (tirstrup)")] <- "denmark"

pass_int_od$foreign_country[pass_int_od$foreign_country == "usa"] <- "United States of America"
pass_airport_int <- unique(pass_int_od[,c("foreign_airport","foreign_country")])

foo <- pass_airport_int$foreign_airport[duplicated(pass_airport_int$foreign_airport)]
foo <- foo[foo != "="]
if(length(foo) > 0){
  stop("Duplicated airport names")
  foo  <- pass_airport_int[pass_airport_int$foreign_airport %in% foo,]
}

pass_airport_int$full <- paste0(pass_airport_int$foreign_airport," airport, ",pass_airport_int$foreign_country)

match1 <- left_join(pass_airport_int, airports2, by = "full")
match_succ <- match1[!is.na(match1$origin_destination), ]
match_fail <- match1[is.na(match1$origin_destination), ]

# remove the unknown airports "="
match_fail <- match_fail[match_fail$foreign_airport != "=",]

# try a fuzzy text match
match_fail$approx_match <- sapply(match_fail$full, function(pattern){
  agrep(pattern, airports2$full, ignore.case = TRUE, value = TRUE,
        max.distance = 0.1)
  
})

match_fuzzy <- match_fail[lengths(match_fail$approx_match) > 0,]
match_fuzzy <- match_fuzzy[,c("foreign_airport","foreign_country", "approx_match")]
match_fuzzy$approx_match <- sapply(match_fuzzy$approx_match, function(x){
  gsub("airport","",x)
})

# try google geocode
if(FALSE){ # don't run by accident it costs money
  register_google(key = Sys.getenv("GOOGLE")) # Get Google Key
  airports_missing <- match_fail$full
  coords_google <- ggmap::geocode(airports_missing)
  airports_missing <- cbind(airports_missing, coords_google)
  saveRDS(airports_missing, "data/CAA_passengers_airports_google.Rds")
}else{
  airports_missing <- readRDS("data/CAA_passengers_airports_google.Rds")
  airports_missing$airports_missing <- as.character(airports_missing$airports_missing)
}

airports_missing$airports_missing[airports_missing$airports_missing == "aarhus airport, NA"] = "aarhus airport, denmark"
# check it is good
ap <- st_as_sf(airports_missing[!is.na(airports_missing$lon),], coords = c("lon","lat"), crs = 4326)
qtm(ap)

match_fail <- match_fail[,c("foreign_airport","foreign_country","full")]
match_fail <- left_join(match_fail, airports_missing, by = c("full" = "airports_missing"))

match_final <- bind_rows(match_succ, match_fail)
match_final$lon[match_final$foreign_airport == "nimes"] <- match_final$`1`[match_final$foreign_airport == "nimes"]
match_final$lat[match_final$foreign_airport == "nimes"] <- match_final$`2`[match_final$foreign_airport == "nimes"]

match_final <- match_final[,c("foreign_airport","foreign_country","full","lon","lat")]

match_final$lon[match_final$foreign_airport == "al-udeid usafb"] <- 51.314415
match_final$lat[match_final$foreign_airport == "al-udeid usafb"] <- 25.117309

match_final$lon[match_final$foreign_airport == "rochester municipal"] <- -92.495464
match_final$lat[match_final$foreign_airport == "rochester municipal"] <- 43.909981





# TODO: join on UK airports
pass_dom_od$airport1 <- tolower(pass_dom_od$airport1)
pass_dom_od$airport2 <- tolower(pass_dom_od$airport2)
airports2$origin_destination <- tolower(airports2$origin_destination)

pass_dom_od$airport1[pass_dom_od$airport1 == "aberdeen"] <- "aberdeen international"
pass_dom_od$airport2[pass_dom_od$airport2 == "aberdeen"] <- "aberdeen international"
pass_dom_od$airport1[pass_dom_od$airport1 == "cardiff wales"] <- "cardiff"
pass_dom_od$airport2[pass_dom_od$airport2 == "cardiff wales"] <- "cardiff"
pass_dom_od$airport1[pass_dom_od$airport1 == "exeter"] <- "exeter international"
pass_dom_od$airport2[pass_dom_od$airport2 == "exeter"] <- "exeter international"
pass_dom_od$airport1[pass_dom_od$airport1 == "city of derry"] <- "londonderry"
pass_dom_od$airport2[pass_dom_od$airport2 == "city of derry"] <- "londonderry"



airports_dom <- unique(c(pass_dom_od$airport1, pass_dom_od$airport2))
airports_dom <- airports_dom[order(airports_dom)]
summary(airports_dom %in% airports2$origin_destination)

dom_succ <- airports2[airports2$origin_destination %in% airports_dom,]
dom_fail <- airports_dom[!airports_dom %in% airports2$origin_destination]

if(FALSE){ # don't run by accident it costs money
  register_google(key = Sys.getenv("GOOGLE")) # Get Google Key
  dom_airports_missing <- paste0(dom_fail," airport, united kingdom")
  coords_google <- ggmap::geocode(dom_airports_missing)
  dom_airports_missing <- cbind(dom_airports_missing, coords_google)
  saveRDS(dom_airports_missing, "data/CAA_passengers_airports_dom_google.Rds")
}else{
  dom_airports_missing <- readRDS("data/CAA_passengers_airports_domgoogle.Rds")
  dom_airports_missing$dom_airports_missing <- as.character(dom_airports_missing$dom_airports_missing)
}

dom_fail <- as.data.frame(dom_airports_missing)
names(dom_fail) <- c("full","lon","lat")
dom_fail$origin_destination_country <- "united kingdom"
dom_fail$origin_destination <- gsub(" airport, united kingdom","",dom_fail$full)

# fix google errors
dom_fail$lon[dom_fail$full == "wrexham airport, united kingdom"] <- -2.9504
dom_fail$lat[dom_fail$full == "wrexham airport, united kingdom"] <- 53.0668

dom_fail$lon[dom_fail$full == "woodvale airport, united kingdom"] <- -3.055556
dom_fail$lat[dom_fail$full == "woodvale airport, united kingdom"] <- 53.581667

dom_fail$lon[dom_fail$full == "portsmouth (uk) airport, united kingdom"] <- -1.05
dom_fail$lat[dom_fail$full == "portsmouth (uk) airport, united kingdom"] <- 50.828333

dom_fail$lon[dom_fail$full == "plymouth airport, united kingdom"] <- -4.105833
dom_fail$lat[dom_fail$full == "plymouth airport, united kingdom"] <- 50.422778

dom_fail$lon[dom_fail$full == "hatfield airport, united kingdom"] <- -0.250833
dom_fail$lat[dom_fail$full == "hatfield airport, united kingdom"] <- 51.765833

dom_fail$lon[dom_fail$full == "kinross airport, united kingdom"] <- -3.462222
dom_fail$lat[dom_fail$full == "kinross airport, united kingdom"] <- 56.211944

dom_fail$lon[dom_fail$full == "st kilda airport, united kingdom"] <- -8.583333
dom_fail$lat[dom_fail$full == "st kilda airport, united kingdom"] <- 57.816667

dom_fail$lon[dom_fail$full == "flotta airport, united kingdom"] <- -3.116667
dom_fail$lat[dom_fail$full == "flotta airport, united kingdom"] <- 58.833333

dom_fail$lon[dom_fail$full == "whalsay airport, united kingdom"] <- -0.927356
dom_fail$lat[dom_fail$full == "whalsay airport, united kingdom"] <- 60.376648

dom_fail$lon[dom_fail$full == "papa stour airport, united kingdom"] <- -1.7
dom_fail$lat[dom_fail$full == "papa stour airport, united kingdom"] <- 60.316667

dom_fail$lon[dom_fail$full == "peterhead airport, united kingdom"] <- -1.875
dom_fail$lat[dom_fail$full == "peterhead airport, united kingdom"] <- 57.517

# bind together

dom_final <- bind_rows(dom_succ, dom_fail)
dom_final$`1` <- NULL
dom_final$`2` <- NULL

airports_all <- match_final
names(airports_all) <- c("airport", "country", "full","lon","lat")
names(dom_final) <- c("airport", "country", "full","lon","lat")
airports_all <- bind_rows(airports_all, dom_final)

airports_all <- st_as_sf(airports_all[!is.na(airports_all$lon),], coords = c("lon","lat"), crs = 4326)
qtm(airports_all)

# clean up duplicated
geom_dup <- airports_all$geometry[duplicated(airports_all$geometry)]

airports_dup <- airports_all[geom_dup,]
qtm(airports_dup)

# loads of duplicates
# some have been fixed before
airports_dup$approx_match <- sapply(airports_dup$full, function(pattern){
  agrep(pattern, airports_fixed$nameOF, ignore.case = TRUE, value = TRUE,
        max.distance = 0.05)
  
})




#"dallas/fort worth airport, United States of America" "dallas airport, United States of America"
#"moline airport, United States of America" "moline (quad city) airport, United States of America"
#"belize airport, belize" "belize city airport, belize"
#"cunagua airport, cuba" "cunagua ( cayo coco) airport, cuba" "cunagua (cayo coco) airport, cuba"
#"agadir (al massira) airport, morocco" "agadir airport, morocco"
#"cranmore airport, irish republic" "sligo airport, irish republic"
#"connaught airport, irish republic" "galway airport, irish republic"
# "londonderry airport, united kingdom" "city of derry (eglinton) airport, united kingdom"
# "asturias airport, spain" "asturias (aviles) airport, spain"
#"belfast city airport, united kingdom" "belfast city (george best) airport, united kingdom"
# "valley airport, united kingdom" "anglesey (valley) airport, united kingdom"
# "madrid airport, spain"
# "wick john o groats airport, united kingdom"  "Wick airport, united kingdom"
# "liverpool" "liverpool (john lennon)"
# "nottingham east midlands int'l" "east midlands international" "east midlands"
# "lerwick (tingwall) airport, united kingdom" "lerwick (tingwall) airport, united kingdom"
# "oran airport, algeria" "oran es senia"
# "angers- marce airport, france" "angers airport, france"
# "manston (kent int) airport, united kingdom" "kent international airport, united kingdom"
# "toulouse airport, france" "toulouse (blagnac) airport, france"
# "chateauroux airport, france" "chateauroux deols airport, france"
# "varry (chalons sur marne)" "chalons sur marne airport, france"
# "enfidha airport, tunisia" "enfidha - hammamet intl airport, tunisia"
# "aarhus airport, denmark" "aarhus (tirstrup) airport, denmark"
# "verona airport, italy" "verona villafranca airport, italy"
# "ingolstadt airport, germany" "ingolstadt-manching airport, germany"
# "goteborg (save) airport, sweden" "goteborg city airport, sweden"
# "goteborg airport, sweden" "goteborg (landvetter) airport, sweden"
#  tranpani airport, italy"  "trapani airport, italy"
# "kobenhavn airport, denmark" "copenhagen airport, denmark"
# "rouzyne airport, czech republic" "prague airport, czech republic"
# "bydgoszcz airport, poland" "bydgoszcz/szweredowo airport, poland"
# "modlin masovia airport, poland"  "warsaw (modlin masovia) airport, poland"
# "warsaw" "warsaw (chopin)"
# "volos airport, greece" "nea anchialos airport, greece"

# some fix locations
airports_all$geometry[airports_all$full == "ciego de avila airport, cuba"] <- st_point(c(-78.789444,22.026944))
airports_all$geometry[airports_all$full == "tobago airport, trinidad and tobago"] <- st_point(c(-60.832222, 11.149722))
airports_all$geometry[airports_all$full == "keflavik airport, iceland"] <- st_point(c(-22.605556, 63.985))

airports_all$geometry[airports_all$full == "faro airport, portugal(excluding madeira)"] <- st_point(c(-7.965833,37.014444))
airports_all$geometry[airports_all$full == "lisbon airport, portugal(excluding madeira)"] <- st_point(c(-9.134167, 38.774167))
airports_all$geometry[airports_all$full == "funchal airport, portugal(madeira)"] <- st_point(c( -16.778056, 32.694167))
airports_all$geometry[airports_all$full == "azores santa maria airport, portugal(excluding madeira)"] <- st_point(c(-25.171111, 36.973889))
airports_all$geometry[airports_all$full == "oporto (portugal) airport, portugal(excluding madeira)"] <- st_point(c(-8.678056, 41.235556))
airports_all$geometry[airports_all$full == "oporto airport, portugal(excluding madeira)"] <- st_point(c(-8.678056, 41.235556))
airports_all$geometry[airports_all$full == "alverca airport, portugal(excluding madeira)"] <- st_point(c(-9.030097,38.883278))

airports_all$geometry[airports_all$full == "ireland west(knock) airport, irish republic"] <- st_point(c(-8.818611, 53.910278))
#airports_all$geometry[airports_all$full == "cranmore airport, irish republic"] <- st_point(c())
#airports_all$geometry[airports_all$full == "connaught airport, irish republic"] <- st_point(c(-8.941111, 53.300278))

airports_all$geometry[airports_all$full == "casablanca anfa airport, morocco"] <- st_point(c( -7.660556, 33.556944))
airports_all$geometry[airports_all$full == "oronsay airport, united kingdom"] <- st_point(c(-6.216667,  56.066667))
airports_all$geometry[airports_all$full == "penzance heliport airport, united kingdom"] <- st_point(c(-5.518333, 50.128056))
airports_all$geometry[airports_all$full == "brawdy airport, united kingdom"] <- st_point(c(-5.123889, 51.883611))
airports_all$geometry[airports_all$full == "pembrey airport, united kingdom"] <- st_point(c(-4.312222, 51.713889))
airports_all$geometry[airports_all$full == "torrejon de ardoz airport, spain"] <- st_point(c(-3.445833, 40.496667))
airports_all$geometry[airports_all$full == "leuchars airport, united kingdom"] <- st_point(c(-2.868611, 56.373056))
airports_all$geometry[airports_all$full == "kinloss airport, united kingdom"] <- st_point(c(-3.560556, 57.649444))

airports_all$geometry[airports_all$full == "dounreay airport, united kingdom"] <- st_point(c(-3.727778, 58.583333))
airports_all$geometry[airports_all$full == "barrow-in-furness airport, united kingdom"] <- st_point(c(-3.2675, 54.128611))
airports_all$geometry[airports_all$full == "haydock park airport, united kingdom"] <- st_point(c(-2.621944, 53.478056))
airports_all$geometry[airports_all$full == "shawbury airport, united kingdom"] <- st_point(c(-2.668056, 52.798056))

airports_all$geometry[airports_all$full == "filton airport, united kingdom"] <- st_point(c(-2.593611, 51.519444))
airports_all$geometry[airports_all$full == "lyneham airport, united kingdom"] <- st_point(c(-1.993333, 51.505278))
airports_all$geometry[airports_all$full == "coal aston airport, united kingdom"] <- st_point(c(-1.430556, 53.304722))
airports_all$geometry[airports_all$full == "south marston airport, united kingdom"] <- st_point(c(-1.722, 51.59))
airports_all$geometry[airports_all$full == "fetlar airport, united kingdom"] <- st_point(c(-0.866667, 60.616667))

airports_all$geometry[airports_all$full == "benson airport, united kingdom"] <- st_point(c(-1.095833, 51.616389))
airports_all$geometry[airports_all$full == "odiham airport, united kingdom"] <- st_point(c(-0.942778, 51.234167))
airports_all$geometry[airports_all$full == "cognac airport, france"] <- st_point(c(-0.3175, 45.658333))

airports_all$geometry[airports_all$full == "moret airport, france"] <- st_point(c(2.7994399, 48.3418999))
airports_all$geometry[airports_all$full == "guyancourt airport, france"] <- st_point(c(2.0625, 48.7602997))
airports_all$geometry[airports_all$full == "meaux airport, france"] <- st_point(c(2.834254, 48.925959))

airports_all$geometry[airports_all$full == "bourg st maurice airport, france"] <- st_point(c(6.704709, 45.605897))
airports_all$geometry[airports_all$full == "ouargla airport, algeria"] <- st_point(c(5.406667, 31.931389))
airports_all$geometry[airports_all$full == "nancy airport, france"] <- st_point(c(6.226111, 48.692222))

airports_all$geometry[airports_all$full == "wildenrath airport, germany"] <- st_point(c(6.221667, 51.114444))
airports_all$geometry[airports_all$full == "bruggen airport, germany"] <- st_point(c(6.129444, 51.2))
airports_all$geometry[airports_all$full == "oslo (fornebu) airport, norway"] <- st_point(c(10.616667, 59.883333))

airports_all$geometry[airports_all$full == "vicenza airport, italy"] <- st_point(c(11.529722, 45.573333))
airports_all$geometry[airports_all$full == "gatow airport, germany"] <- st_point(c(13.138056, 52.474444))
airports_all$geometry[airports_all$full == "capri airport, italy"] <- st_point(c(14.240273, 40.551988))
airports_all$geometry[airports_all$full == "mora airport, sweden"] <- st_point(c(14.499944, 60.960972))
airports_all$geometry[airports_all$full == "bucharest (baneasa) airport, romania"] <- st_point(c(26.103611, 44.503611))

write_sf(airports_all,"data/airports_pass.gpkg")
