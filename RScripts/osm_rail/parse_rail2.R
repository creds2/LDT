
remotes::install_github("itsleeds/osmtools")
library(OSMtools)

osmt_convert("D:/Big Data/europe-latest.osm.pbf", "o5m")
osmt_filter("D:/Big Data/europe-latest.osm.o5m", 
            "D:/Big Data/europe-rail.osm.o5m",
            keep = "railway=" )
osmt_convert("D:/Big Data/europe-rail.osm.o5m", "pbf")

# make_ini_attributes = function(x, defaults = c("name", "highway", "waterway", "aerialway", "barrier", "man_made"), append = TRUE) {
#   attributes_default_ini = paste0("attributes=", paste(defaults, collapse = ","))
#   if(append) {
#     x = c(defaults, x)
#   }
#   attributes_default_ini_new = paste0("attributes=", paste(x, collapse = ","))
#   ini_file = readLines("https://github.com/OSGeo/gdal/raw/master/gdal/data/osmconf.ini")
#   sel_attributes = grepl(pattern = attributes_default_ini, x = ini_file)
#   message("Old attributes: ", ini_file[sel_attributes])
#   message("New attributes: ", attributes_default_ini_new)
#   ini_file[sel_attributes] = attributes_default_ini_new
#   ini_file
# }
# 
# ini_new = make_ini_attributes(x = c("railway", "gauge", "electrified",
#                                     "name","service","usage", "operator", "voltage", "frequency",
#                                     "ref", "maxspeed", "importance", "usage"), append = FALSE)
# 
# writeLines(ini_new, "data/rail.ini")
library(osmextract)

rail = oe_read("D:/Big Data/europe-rail.osm.pbf", 
               extra_tags = c("railway", "usage","gauge", "electrified",
                                    "service", "operator", "voltage",
                                   "frequency","ref", "maxspeed", "importance",
                                   "highspeed"))


rail = rail[,c("name","railway","usage", "gauge", "electrified",
               "service", "operator", "voltage",
               "frequency","ref", "maxspeed", "importance",
               "highspeed")]
rail = rail[rail$railway %in% c("rail"),]
rail <- rail[rail$usage %in% c("main",NA),]

rail = rail[,c("name", "gauge", "electrified",
               "service", "operator", "voltage",
               "frequency","ref", "maxspeed", "importance",
               "highspeed")]

sf::write_sf(rail,"data/europe-rail.gpkg")



rail = oe_read("D:/Big Data/europe-rail.osm.pbf", 
               extra_tags = c("railway", "usage","gauge", "electrified",
                              "service", "operator", "voltage",
                              "frequency","ref", "maxspeed", "importance",
                              "highspeed"))


rail = rail[,c("name","railway","usage", "gauge", "electrified",
               "service", "operator", "voltage",
               "frequency","ref", "maxspeed", "importance",
               "highspeed")]
rail = rail[rail$railway %in% c("rail"),]
rail <- rail[rail$usage %in% c("main",NA),]

rail = rail[,c("name", "gauge", "electrified",
               "service", "operator", "voltage",
               "frequency","ref", "maxspeed", "importance",
               "highspeed")]

sf::write_sf(rail,"data/europe-rail.gpkg")





library(tmap)
tmap_mode("view")


clean_mph <- function(x){
  x <- gsub("mph","",x)
  x <- gsub("+","",x)
  x <- gsub(" ","",x)
  x <- round(as.numeric(x) * 1.60934)
  x <- as.character(x)
  x
}


rail$maxspeed <- ifelse(grepl("mph",rail$maxspeed),clean_mph(rail$maxspeed),rail$maxspeed)
rail$maxspeed <- as.numeric(rail$maxspeed)
rail$maxspeed[rail$maxspeed > 400] <- NA
rail <- rail[!is.na(rail$maxspeed),]
rail <- rail[rail$maxspeed > 150,]

sf::write_sf(rail,"data/europe-rail-150kph.gpkg")

qtm(rail, lines.col = "maxspeed", style = "cobalt")


