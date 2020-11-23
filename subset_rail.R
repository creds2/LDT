# Find the high speed network
library(sf)
library(tmap)
tmap_mode("view")

rail <- read_sf("data/europe-rail.gpkg") 

summary(is.na(rail$maxspeed))

clean_maxspeed <- function(x){
  mph <- grepl("mph",x)
  y <- trimws(gsub("mph"," ", x))
  y <- as.numeric(y)
  y <- ifelse(mph, y * 1.60934, y)
  y <- round(y)
  y
} 

rail$maxspeed <- clean_maxspeed(rail$maxspeed)
summary(is.na(rail$maxspeed))
table(rail$highspeed)

rail_hs <- rail[is.na(rail$maxspeed) | rail$maxspeed > 180 | rail$highspeed %in% "yes",]
#rail_hs <- rail_hs[!rail_hs$highspeed %in% "no", ]

hsgood <- !is.na(rail_hs$maxspeed) | !is.na(rail_hs$highspeed)
rail_hs_good <- rail_hs[hsgood, ]
rail_hs_bad <- rail_hs[!hsgood, ]

rail_hs_buff <- st_transform(rail_hs_good, 3035)
rail_hs_buff <- st_buffer(rail_hs_buff, 100000)
rail_hs_buff <- st_union(rail_hs_buff)
qtm(rail_hs_buff)
sf::write_sf(rail_hs_buff,"data/europe-rail-highspeed-buffer.gpkg")
sf::write_sf(rail_hs_good,"data/europe-rail-highspeed.gpkg")
sf::write_sf(rail,"data/europe-rail-clean.gpkg")


lines <- rail_hs_good[1:100,]
lines <- st_transform(lines, 3035)
attrib <- c("gauge","electrified","voltage","frequency","ref","maxspeed")
centrline <- function(lines, attrib, dist = 100){
  lines <- lines[,attrib]
  lines <- dplyr::group_by_at(lines, attrib)
  lines <- sf::st_buffer(lines, dist = 100, endCapStyle = "SQUARE")
  lines <- dplyr::group_split(lines)
  res <- list()
  for(i in seq_len(length(res))){
    sub_poly <- lines[[i]]
    sub_poly <- sf::st_segmentize(sub_poly, dist * 2)
    sub_point <- sf::st_combine(sf::st_cast(sf::st_geometry(sub_poly), "POINT"))
    sub_voro <- sf::st_voronoi(sub_point)
    sub_voro <- sf::st_collection_extract(sub_voro)
    sub_voro <- sf::st_as_sf(sub_voro)
    #sub_voro <- sf::st_intersection(sub_voro, sub_poly)
    sub_centr <- sf::st_cast(sub_voro, "POINT")
    sub_centr <- sub_centr[sub_poly, op = sf::st_within]
    sub_centr <- sf::st_cast(sub_centr, "LINESTRING")
    qtm(sub_voro) + qtm(sub_poly, fill = NULL) + qtm(sub_centr) + qtm(rail_hs_good[1,])
  }
}





rail_hs_buff <- st_transform(rail_hs_good, 3035)
rail_hs_buff <- st_buffer(rail_hs_buff, dist = 100)
rail_hs_buff_union <- st_union(rail_hs_buff)
rail_hs_buff_union <- st_cast(rail_hs_buff_union, "POLYGON")
rail_hs_buff_union <- st_sf(data.frame(id = 1:length(rail_hs_buff_union), geometry = rail_hs_buff_union))

rail_hs <- rail_hs[,c("gauge")]

rail_hs1 <- rail_hs[!is.na(rail_hs$highspeed),]
qtm(rail_hs_buff_union)


rail_hs
qtm(rail_hs)

library(rgrass7)

foo <- as(rail_hs_good[1:100,],"Spatial")
execGRASS("v.centerline")
