# Parse European Rail network
remotes::install_github("itsleeds/osmextract", ref = "read_pbf_gpkg")
library(osmtools)

rail <- oe_get()




# rail <- oe_read("E:/OneDrive - University of Leeds/Data/LDT/europe-latest.osm.pbf",
#                 extra_attributes = c("railway", "gauge", "electrified",
#                   "name","service","usage", "operator", "voltage", "frequency",
#                   "ref", "maxspeed") )




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
#                                     "ref", "maxspeed"))
# 
# writeLines(ini_new, "data/rail.ini")
# res = sf::st_read("E:/OneDrive - University of Leeds/Data/LDT/europe-latest.osm.pbf",
#                   layer = "lines", options = "CONFIG_FILE=data/rail.ini")
