# Estimate rail distance between airports

library(sf)
library(dodgr)

rail = read_sf("data/europe-rail.gpkg")
names(rail)[names(rail) == "geom"] = "geometry"
st_geometry(rail) = "geometry"
rail$railway = "rail"
rail$id = seq(1,nrow(rail))


# Clean MPH
# clean_mph = function(x){
#   has_mph = grepl(" mph",x)
#   x2 = ifelse(has_mph,
#               round(as.numeric(gsub(" mph","",x)) * 1.60934),
#               as.numeric(x))
#   x2
# }



graph <- dodgr::weight_railway(rail, id_col  = "id")
graph <- dodgr_contract_graph(graph)
subgraphs <- graph[graph$component > 1,]
subgraphs <- dodgr_to_sf(subgraphs)

