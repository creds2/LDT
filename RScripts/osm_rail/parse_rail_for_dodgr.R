
remotes::install_github("itsleeds/osmtools")
library(OSMtools)
library(osmextract)
library(dodgr)
library(tmap)
tmap_mode("view")

iso_final <- st_read("data/isochrones/iso_hours.geojson")
bbox = st_bbox(iso_final)

osmt_convert("D:/Big Data/europe-latest.osm.pbf", "o5m",
             bbox = bbox)
osmt_filter("D:/Big Data/europe-latest.osm.o5m", 
            "D:/Big Data/case-study-rail.osm.o5m",
            keep = "railway=" )
osmt_convert("D:/Big Data/case-study-rail.osm.o5m", "pbf")




rail = oe_read("D:/Big Data/case-study-rail.osm.pbf", 
               extra_tags = c("railway", "usage","gauge", "electrified",
                              "service", "operator", "voltage",
                              "frequency","ref", "maxspeed", "importance",
                              "highspeed"))

sf::write_sf(rail,"data/case-study-rail.gpkg")

rail = rail[rail$railway %in% c("rail","bridge","yes",""),]

rail = rail[is.na(rail$highway), ]
rail = rail[is.na(rail$barrier), ]
rail = rail[!rail$service %in% c("siding","yard","spur"), ]

rail = rail[,c("osm_id","railway","usage",
               "gauge","electrified","service","operator","voltage","frequency","ref","maxspeed","importance",
               "highspeed","geom")]
names(rail)[names(rail) == "geom"] = "geometry"
st_geometry(rail) = "geometry"

clean_maxspeed = function(x){
  x = ifelse(grepl("mph", x),
             as.numeric(gsub(" ","",gsub("mph","",x))) * 1.609344,
             as.numeric(x)
  )
  x
}

rail$maxspeed = clean_maxspeed(rail$maxspeed)
rail$maxspeed[is.na(rail$maxspeed)] = 10

sf::write_sf(rail,"data/case-study-rail-cleaned.gpkg")


