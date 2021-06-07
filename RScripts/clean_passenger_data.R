library(dplyr)
library(sf)
library(ggmap)
library(tmap)
tmap_mode("view")
# Read in

#pass_transit <- readRDS("data/CAA_transit.Rds")
pass_int_od <- readRDS("data/CAA_int_od_v2.Rds")
pass_dom_od <- readRDS("data/CAA_dom_od_v2.Rds")

airports_fixed <- readRDS("airports_final.Rds")
airports2 <- readRDS("airports.Rds")

#flights_flow <- readRDS("flow_final.Rds")

airports2$origin_destination_country[airports2$origin_destination_country == "portugal(excluding madeira)"] <- "portugal"
airports2$origin_destination_country[airports2$origin_destination_country == "portugal(madeira)"] <- "portugal"

# Remove totals
pass_int_od <- pass_int_od[!grepl("Total",pass_int_od$uk_airport),]

# get the locations of the passenger airports
pass_int_od$foreign_airport <- tolower(pass_int_od$foreign_airport)
pass_int_od$foreign_country <- tolower(pass_int_od$foreign_country)

pass_int_od$foreign_airport <- gsub(" airport","",pass_int_od$foreign_airport)

# fix typo differences
pass_int_od$foreign_airport[is.na(pass_int_od$foreign_airport)] <- "="

pass_int_od$foreign_country[pass_int_od$foreign_airport == "aarhus"] <- "denmark"
pass_int_od$foreign_country[pass_int_od$foreign_country == "rumania"] <- "romania"
pass_int_od$foreign_country[pass_int_od$foreign_country == "fed rep yugo serbia m'enegro"] <- "republic of serbia"
pass_int_od$foreign_country[pass_int_od$foreign_country == "fed rep yugosl',s'bia,m'enegro"] <- "republic of serbia"
pass_int_od$foreign_country[pass_int_od$foreign_country == "fed rep yugo serbia m'enegr0"] <- "republic of serbia"
pass_int_od$foreign_country[pass_int_od$foreign_country == "belorus"] <- "belarus"
pass_int_od$foreign_country[pass_int_od$foreign_country == "burma"] <- "myanmar"

pass_int_od$foreign_country[pass_int_od$foreign_airport == "podgorica"] <- "republic of montenegro"
pass_int_od$foreign_country[pass_int_od$foreign_airport == "pristina"] <- "kosovo"
pass_int_od$foreign_country[pass_int_od$foreign_airport == "podgorica"] <- "republic of montenegro"
pass_int_od$foreign_country[pass_int_od$foreign_airport == "tivat"] <- "republic of montenegro"
pass_int_od$foreign_country[pass_int_od$foreign_airport == "kosovo"] <- "kosovo"
pass_int_od$foreign_country[pass_int_od$foreign_airport == "republic of montenegro"] <- "republic of montenegro"
pass_int_od$foreign_country[pass_int_od$foreign_airport == "taipei"] <- "taiwan"
pass_int_od$foreign_country[pass_int_od$foreign_airport == "bahrain"] <- "bahrain"

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
pass_int_od$foreign_country[pass_int_od$foreign_country == "united states of america"] <- "United States of America"

pass_int_od$foreign_country[pass_int_od$foreign_country == "spain (excluding canary islands)"] <- "spain"
pass_int_od$foreign_country[pass_int_od$foreign_country == "spain (canary islands)"] <- "spain"
pass_int_od$foreign_country[pass_int_od$foreign_country == "spain(canary islands)"] <- "spain"

pass_int_od$foreign_country[pass_int_od$foreign_country == "portugal(excluding madeira)"] <- "portugal"
pass_int_od$foreign_country[pass_int_od$foreign_country == "portugal(madeira)"] <- "portugal"
pass_int_od$foreign_country[pass_int_od$foreign_country == "portugal (excluding madeira)"] <- "portugal"
pass_int_od$foreign_country[pass_int_od$foreign_country == "portugal (madeira)"] <- "portugal"

# standerdise names
tidy2 <- function(from,to){
  pass_int_od$foreign_airport[pass_int_od$foreign_airport %in% from] <-
    to
  assign('pass_int_od',pass_int_od,envir=.GlobalEnv)
}

tidy2("belize", "belize city")
tidy2("dallas/fort worth", "dallas")
tidy2("moline", "moline (quad city)")
tidy2(c("cunagua", "cunagua ( cayo coco)"), "cunagua (cayo coco)")
tidy2("agadir (al massira)", "agadir")
tidy2("cranmore", "sligo")
tidy2("connaught", "galway")
tidy2( "londonderry", "city of derry (eglinton)")
tidy2( "asturias", "asturias (aviles)")
tidy2("belfast city", "belfast city (george best)")
tidy2( "valley", "anglesey (valley)")
tidy2( "wick john o groats",  "Wick")
tidy2( "liverpool", "liverpool (john lennon)")
tidy2(c("nottingham east midlands int'l", "east midlands international"), "east midlands")
tidy2( "lerwick (tingwall)", "lerwick (tingwall)")
tidy2( "oran", "oran es senia")
tidy2( "angers- marce", "angers")
tidy2( "manston (kent int)", "kent")
tidy2( "toulouse", "toulouse (blagnac)")
tidy2( "chateauroux", "chateauroux deols")
tidy2( "varry (chalons sur marne)", "chalons sur marne")
tidy2( "enfidha", "enfidha - hammamet intl")
tidy2( "aarhus", "aarhus (tirstrup)")
tidy2( "verona", "verona villafranca")
tidy2( "ingolstadt", "ingolstadt-manching")
tidy2( "goteborg (save)", "goteborg city")
tidy2( "goteborg", "goteborg (landvetter)")
tidy2(  "tranpani",  "trapani")
tidy2( "kobenhavn", "copenhagen")
tidy2( "rouzyne", "prague")
tidy2( "bydgoszcz", "bydgoszcz/szweredowo")
tidy2( "modlin masovia",  "warsaw (modlin masovia)")
tidy2( "warsaw", "warsaw (chopin)")
tidy2( "volos", "nea anchialos")

tidy2( "oporto (portugal)", "oporto")
tidy2( "mitilini", "mytilini")
tidy2( "izmir (cumaovasi)", "izmir (adnan menderes)")
tidy2( "minsk int'l", "minsk")
tidy2( "lapeenranta", "lappeenranta")
tidy2( "alexandria ( nouzha )", "alexandria (nouzha)")
tidy2( "alexandria", "alexandria (borg el arab)")
tidy2( "kiev", "kiev (borispol)")
tidy2( c("bagdhad (geca)","baghdad (saddam int)","baghdad (geca)"), "baghdad int")
tidy2( c("baku (heyder aliyev int'l)","baku ( heyder aliyev int'l )"), "baku")
tidy2( "bangalore", "bangalore (bengaluru)")
tidy2( "hyderabad", "hyderabad ( rajiv ghandi )")
tidy2( "kolkata", "calcutta")
tidy2( "dacca", "dhakha")
tidy2( "perth", "perth (australia)")
tidy2( "shanghai", "shanghai (pu dong)")
tidy2( "bangkok", "bangkok suvarnabhumi")
tidy2( "madras/chennai", "chennai")
tidy2( "ras nasrani", "sharm el sheikh (ophira)")
tidy2( "alma ata", "almaty")
tidy2( "santiago de compostela (spain)", "Santiago de Compostela")
tidy2( "alghero/sassari", "alghero (fertilia)")
tidy2( "beek", "maastricht")

tidy2("chicago", "chicago (o'hare)")
tidy2("washington", "washington (dulles)")
tidy2("kuala lumpur", "kuala lumpur (sepang)")
tidy2("georgetown", "georgetown (guyana)")




pass_airport_int <- unique(pass_int_od[,c("foreign_airport","foreign_country")])


summary(duplicated(pass_airport_int$foreign_airport))
bar <- pass_airport_int$foreign_airport[duplicated(pass_airport_int$foreign_airport)]
bar <- pass_airport_int[pass_airport_int$foreign_airport %in% bar,]

foo <- pass_airport_int$foreign_airport[duplicated(pass_airport_int$foreign_airport)]
foo <- foo[foo != "="]
if(length(foo) > 0){
  foo  <- pass_airport_int[pass_airport_int$foreign_airport %in% foo,]
  stop("Duplicated airport names")
}

pass_airport_int$full <- paste0(pass_airport_int$foreign_airport," airport, ",pass_airport_int$foreign_country)

match1 <- left_join(pass_airport_int, airports2, by = "full")
match_succ <- match1[!is.na(match1$origin_destination), ]
match_fail <- match1[is.na(match1$origin_destination), ]

# remove the unknown airports "="
match_fail <- match_fail[match_fail$foreign_airport != "=",]

# # try a fuzzy text match
# match_fail$approx_match <- sapply(match_fail$full, function(pattern){
#   agrep(pattern, airports2$full, ignore.case = TRUE, value = TRUE,
#         max.distance = 0.1)
#   
# })
# 
# match_fuzzy <- match_fail[lengths(match_fail$approx_match) > 0,]
# match_fuzzy <- match_fuzzy[,c("foreign_airport","foreign_country", "approx_match")]
# match_fuzzy$approx_match <- sapply(match_fuzzy$approx_match, function(x){
#   gsub("airport","",x)
# })

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

airports_missing$lon[airports_missing$airports_missing == "al-udeid usafb airport, qatar"] <- 51.314415
airports_missing$lat[airports_missing$airports_missing == "al-udeid usafb airport, qatar"] <- 25.117309

airports_missing$lon[airports_missing$airports_missing == "rochester municipal airport, United States of America"] <- -92.495464
airports_missing$lat[airports_missing$airports_missing == "rochester municipal airport, United States of America"] <- 43.909981

# check it is good
ap <- st_as_sf(airports_missing[!is.na(airports_missing$lon),], coords = c("lon","lat"), crs = 4326)
qtm(ap)

match_fail <- match_fail[,c("foreign_airport","foreign_country","full")]
match_fail <- left_join(match_fail, airports_missing, by = c("full" = "airports_missing"))

match_final <- bind_rows(match_succ, match_fail)
match_final$lon[match_final$foreign_airport == "nimes"] <- match_final$`1`[match_final$foreign_airport == "nimes"]
match_final$lat[match_final$foreign_airport == "nimes"] <- match_final$`2`[match_final$foreign_airport == "nimes"]
match_final <- match_final[,c("foreign_airport","foreign_country","full","lon","lat")]

# join on UK airports
pass_dom_od$airport1 <- tolower(pass_dom_od$airport1)
pass_dom_od$airport2 <- tolower(pass_dom_od$airport2)
airports2$origin_destination <- tolower(airports2$origin_destination)


# standerdise names
tidy3 <- function(from,to){
  pass_dom_od$airport1[pass_dom_od$airport1 %in% from] <- to
  pass_dom_od$airport2[pass_dom_od$airport2 %in% from] <- to
  assign('pass_dom_od',pass_dom_od,envir=.GlobalEnv)
}

tidy3("aberdeen", "aberdeen international")
tidy3("cardiff wales", "cardiff")
tidy3("exeter", "exeter international")
tidy3(c("city of derry","city of derry (eglinton)"), "londonderry")
tidy3(c("city of derry","city of derry (eglinton)"), "londonderry")
tidy3("belfast city", "belfast city (george best)")
tidy3("valley", "anglesey (valley)")
tidy3("liverpool", "liverpool (john lennon)")
tidy3(c("nottingham east midlands int'l","east midlands international"), "east midlands")
tidy3("lerwick  (tingwall)", "lerwick (tingwall)")
tidy3("manston (kent int)", "kent")
tidy3("doncaster", "doncaster sheffield")
tidy3("durham tees valley", "teesside airport")
tidy3("wick john o groats", "Wick")

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
  dom_airports_missing <- readRDS("data/CAA_passengers_airports_dom_google.Rds")
  dom_airports_missing$dom_airports_missing <- as.character(dom_airports_missing$dom_airports_missing)
}

dom_airports_missing$airport <- gsub(" airport, united kingdom","",dom_airports_missing$dom_airports_missing)
#dom_airports_missing <- as.data.frame(dom_airports_missing)
length(dom_fail)
dom_fail <- dom_airports_missing[dom_airports_missing$airport %in% dom_fail, ]
nrow(dom_fail)

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

dom_fail$lon[dom_fail$full == "cottesmore(raf) airport, united kingdom"] <- -0.651389
dom_fail$lat[dom_fail$full == "cottesmore(raf) airport, united kingdom"] <- 52.729444


# bind together

dom_final <- bind_rows(dom_succ, dom_fail)
dom_final$`1` <- NULL
dom_final$`2` <- NULL

airports_all <- match_final
names(airports_all) <- c("airport", "country", "full","lon","lat")
names(dom_final) <- c("airport", "country", "full","lon","lat")
airports_all <- bind_rows(airports_all, dom_final)

airports_all <- st_as_sf(airports_all[!is.na(airports_all$lon),], coords = c("lon","lat"), crs = 4326)
airports_all <- airports_all[,c("airport","country", "full","geometry")]

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

airports_all$geometry[airports_all$full == "minsk loshitsa airport, belarus"] <- st_point(c(27.539722, 53.864444))

airports_all$geometry[airports_all$full == "ankara (etmesgut) airport, turkey"] <- st_point(c(32.688622, 39.949831))

airports_all$geometry[airports_all$full == "reykjavik airport, iceland"] <- st_point(c(-21.940556, 64.13))

airports_all$geometry[airports_all$full == "andravida airport, greece"] <- st_point(c(21.283333, 37.933333 ))

airports_all$geometry[airports_all$full == "volkel airport, netherlands"] <- st_point(c(5.690833, 51.657222))

# clean up duplicated
geom_dup <- airports_all$geometry[duplicated(airports_all$geometry)]

airports_dup <- airports_all[geom_dup,]
#qtm(airports_dup)
airports3 <- readRDS("data/airports_missing.Rds")
airports3 <- st_as_sf(airports3[!is.na(airports3$lon),], coords = c("lon","lat"), crs = 4326)
names(airports3) <- names(airports_all)
airports_all <- rbind(airports_all, airports3)

#foo = unique(airports_all)
# chack for very nearby airports

dists <- matrix(as.numeric(st_distance(airports_all)), nrow = nrow(airports_all))
dists[dists == 0] <- NA
dists[dists > 10000] <- NA
rownames(dists) <- airports_all$airport
colnames(dists) <- airports_all$airport
min(dists, na.rm = TRUE)

dists <- dists[rowSums(is.na(dists)) != ncol(dists), ]
dists <- dists[,colSums(is.na(dists)) != nrow(dists)]

airports_close <- airports_all[airports_all$airport %in% rownames(dists),]
qtm(airports_close)

airports_all[airports_all$full != "private strips/helipads airport, united kingdom",]

# final missing airpots
airports_missing2 <- pass_int_od[!pass_int_od$foreign_airport %in% airports_all$airport,]
airports_missing2 <- airports_missing2[!duplicated(airports_missing2$foreign_airport),]
airports_missing2 <- airports_missing2[,c("foreign_airport","foreign_country")]
airports_missing2 <- airports_missing2[airports_missing2$foreign_airport != "londtrop",]
if(nrow(airports_missing2) > 0){
  stop("missing airports")
}

airports_all <- airports_all[airports_all$airport != "unknown",]
airports_all <- airports_all[airports_all$airport != "=",]
airports_all <- unique(airports_all)

#foo <- airports_all$airport[duplicated(airports_all$airport)]
#foo <- airports_all[airports_all$airport %in% foo,]

# Dom data has and AB BA problems
pass_dom_od$key <- stplanr::od_id_szudzik(pass_dom_od$airport1, pass_dom_od$airport2)

pass_dom_od <- pass_dom_od %>%
  group_by(year, key) %>%
  summarise(airport1 = airport1[1],
            airport2 = airport2[1],
            total_pax = max(total_pax, na.rm = TRUE),
            scheduled_pax = max(scheduled_pax, na.rm = TRUE),
            charter_pax = max(charter_pax, na.rm = TRUE))

pass_dom_od$key <- NULL


write_sf(airports_all,"data/airports_pass_v2.gpkg")
saveRDS(pass_int_od, "data/CAA_int_od_clean_v2.Rds")
saveRDS(pass_dom_od, "data/CAA_dom_od_clean_v2.Rds")

qtm(airports_all)

int_summ <- pass_int_od[pass_int_od$year == 2018,]
int_summ <- int_summ %>%
  group_by(foreign_airport) %>%
  summarise(total_pax = sum(total_pax))


int_summ$percent <- int_summ$total_pax / sum (int_summ$total_pax) * 100

int_summ <- left_join(int_summ, airports_all, by = c("foreign_airport" = "airport"))
int_summ <- st_as_sf(int_summ)

int_summ_top <- int_summ[int_summ$percent > 0.64,]
sum(int_summ_top$percent)
nrow(int_summ_top)

tm_shape(int_summ_top) +
  tm_bubbles("percent", "red", border.col = "black", border.lwd=1, size.lim = c(0, 2e5))
