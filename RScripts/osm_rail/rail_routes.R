library(sf)
library(dplyr)
library(dodgr)
library(tmap)
tmap_mode("view")

airports <- read_sf("data/clean/airports_clean_second_pass_2021.gpkg")
pass_od <- read_sf("data/clean/od_emissions_2021.gpkg")

rail = read_sf("data/case-study-rail-cleaned-mod.gpkg") # modified with extra walking routes
names(rail)[names(rail) == "geom"] <- "geometry"
st_geometry(rail) = "geometry"

rail$is_electrified <- rail$electrified %in% c("4th_rail","construction","contact_line","contact_line;4th_rail",
                                               "contact_line;rail","rail","rail;contact_line","yes")



grid_emissions <- read.csv("data/co2-grid-emission-intensity-15.csv") # https://www.eea.europa.eu/en/analysis/indicators/greenhouse-gas-emission-intensity-of-1
names(grid_emissions) = c("year","country","emissions","emissions_high","emissions_low")
grid_emissions = grid_emissions[grid_emissions$year == 2022,]
grid_emissions = grid_emissions[grid_emissions$country != "EU-27",]
grid_emissions$country[grid_emissions$country == "Czechia"] = "Czech Republic"
grid_emissions = grid_emissions[,c("year","country","emissions")]
grid_emissions_extra = data.frame(year = 2022, country = c("Switzerland","United Kingdom"), emissions = c(0.115,0.19338) * 1000)
grid_emissions = rbind(grid_emissions, grid_emissions_extra)
grid_emissions$emissions = grid_emissions$emissions/1000

# https://www.kth.se/polopoly_fs/1.179879.1600688474!/Menu/general/column-content/attachment/Energy_060925_full_pdf.pdf
# Trains use 0.07 - 0.18 kWh per passenger km (average 0.08 kWh/pkm)
# https://dataportal.orr.gov.uk/media/1993/rail-emissions-2020-21.pdf
# UK 4189 mill KWh + 476 Mill L Diesel ~ 8949 Mill KWh, 550 mill vkm and 66800 mill pkw ~ 0.13 kWh/pkm


grid_emissions$co2perpkm = grid_emissions$emissions * 0.08 # NB CO2 not CO2e

grid_emissions$emissions <- NULL


#https://hub.arcgis.com/datasets/esri::world-countries-generalized/explore?location=49.986315%2C8.226981%2C6.88 
bounds = st_read("data/World_Countries_(Generalized)_-4149901768586864241.geojson")
bounds = bounds[bounds$COUNTRY %in% c(grid_emissions$country,"United Kingdom"),]


bounds = left_join(bounds, grid_emissions, by = c("COUNTRY" = "country"))
bounds = st_as_sf(bounds)
bounds = st_transform(bounds, 4326)
bounds$co2perpkm[is.na(bounds$co2perpkm)] = 0.036
bounds = bounds[,c("COUNTRY","co2perpkm")]

rail2 = st_join(rail, bounds, largest = TRUE)
rail2$co2perpkm = ifelse(rail2$is_electrified, rail2$co2perpkm, 0.025)
rail2$co2perpkm[is.na(rail2$co2perpkm)] <- 0.036

graph <- dodgr::weight_railway(rail2, keep_cols = c("maxspeed","co2perpkm")) # 9,228,902 rows 9,226,784
graph$maxspeed[graph$maxspeed < 1] = 1
graph$d_weighted = graph$d / (graph$maxspeed * 0.277778)
graph = graph[graph$component == 1,] # 8,841,306 rows 8,839,134

graph$kgco2 = graph$d * graph$co2perpkm / 1000

if(anyNA(graph$kgco2)){
  stop("NAs in kgco2")
}

dodgr::clear_dodgr_cache()

# graph_contract <- dodgr_contract_graph(graph) # 402,927
# graph_contract$contract_id <- seq(1, nrow(graph_contract))
# summary(graph_contract)
# graph_uncontract <- dodgr_uncontract_graph(graph_contract)

# main_graph = graph[graph$component == 1,]
# saveRDS(main_graph,"data/rail_graph.Rds")
# 
# foo = as.data.frame(table(graph$component))
# 
# subgraph = graph[graph$component > 1,]
# subgraph = subgraph[subgraph$component <= 20,]
# pts = sf::st_as_sf(subgraph[!is.na(subgraph$from_lon),], coords = c("from_lon","from_lat"))
# plot(pts$geometry)
# 
# main_sf = dodgr_to_sf(main_graph)
# 
# qtm(main_sf) + qtm(pts, dots.col = "red")



verts = dodgr_vertices(graph)
verts = verts[!is.na(verts$x),]
verts = verts[!is.na(verts$y),]

path_to_sf <- function(dp, verts, simplify = FALSE) {
  # Check for emplyr paths
  if (length(dp) > 0) {
    path <- verts[match(dp, verts$id), ]
    path <- matrix(c(path$x, path$y), ncol = 2)
    path <- sf::st_linestring(path)
    
    if (simplify) {
      path <- sf::st_as_sfc(list(path), crs = 4326)
      path <- sf::st_transform(path, 27700)
      path <- sf::st_simplify(path, 5, preserveTopology = TRUE)
      path <- sf::st_transform(path, 4326)
      path <- path[[1]]
    }
    return(path)
  } else {
    return(NA)
  }
}

path_to_sf2 <- function(dp, graph) {
  # Check for emplyr paths
  if (length(dp) > 0) {
    path <- graph[dp, ]
    vals = colSums(path[,c("d","d_weighted","kgco2")])
    vals = as.data.frame(as.list(vals))
    #foo = st_as_sf(path, coords = c("from_lon","from_lat"), crs = 4326)
    path <- matrix(c(path$from_lon, path$from_lat), ncol = 2)
    path <- sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(path), crs = 4326))
    path <- cbind(path, vals)

    return(path)
  } else {
    return(NA)
  }
}


from = st_coordinates(airports[airports$airport == "Barcelona",])
#to = st_coordinates(airports[airports$airport == "Alicante",])

to = matrix(c(-0.8410890848410684, 38.84107834968663), ncol = 2)
colnames(to) = c("X","Y")

dp = dodgr_paths(graph, from, to, vertices = FALSE)
dp = unlist(dp, recursive = FALSE)

path_sf = path_to_sf2(dp[[1]], graph)
#path_sf = st_as_sfc(list(path_sf), crs = 4326)
#path_sf = st_as_sf(path_sf)

# plot(path_sf)
# 
# qtm(path_sf)

pass_od = pass_od[,c("airport1","airport1_country","airport2","airport2_country","length_km","length_km_detour")]
isochrones = read_sf("data/isochrones/iso_hours.geojson")
sf_use_s2(FALSE)
isochrones = st_union(isochrones)

airports_study = airports[isochrones,]

pass_od_study = pass_od[pass_od$airport1 %in% airports_study$airport,]
pass_od_study = pass_od_study[pass_od_study$airport2 %in% airports_study$airport,]
pass_od_study = pass_od_study[pass_od_study$airport2_country != "Australia",]

from = airports_study[match(pass_od_study$airport1, airports_study$airport),]
from = st_coordinates(from)

to = airports_study[match(pass_od_study$airport2, airports_study$airport),]
to = st_coordinates(to)

dp = dodgr_paths(graph, from, to, pairwise = TRUE, quiet = FALSE, vertices = FALSE)
dp = unlist(dp, recursive = FALSE)
path_sf = pbapply::pblapply(dp, path_to_sf2, graph = graph)
#path_sf = st_as_sfc(path_sf, crs = 4326)
#path_sf = st_as_sf(path_sf)
names(path_sf) = paste0(pass_od_study$airport1,"_",pass_od_study$airport1_country,"_",
                        pass_od_study$airport2,"_",pass_od_study$airport2_country)
path_sf = path_sf[lengths(path_sf) == 4]
path_sf = dplyr::bind_rows(path_sf, .id = "route")
#qtm(path_sf)

# graph_sf = dodgr_to_sf(graph)
# st_write(graph_sf,"data/rail_graph.gpkg")

path_sf$length_rail_km = round(as.numeric(st_length(path_sf))/1000,1)

pass_od_study$route = paste0(pass_od_study$airport1,"_",pass_od_study$airport1_country,"_",
                             pass_od_study$airport2,"_",pass_od_study$airport2_country)

pass_od_study = left_join(pass_od_study, st_drop_geometry(path_sf), by = "route")

names(pass_od_study)[names(pass_od_study) == "kgco2" ] = "kgco2_railpsngr"
names(path_sf)[names(path_sf) == "kgco2" ] = "kgco2_railpsngr"

#pass_od_study$length_rail_km = path_sf$length_rail_km

saveRDS(path_sf,"data/clean/rail_routes_newkgco2.Rds")
st_write(pass_od_study,"data/clean/od_emissions_2021_rail_newkgco2.gpkg", delete_dsn = TRUE)

summary(pass_od_study$length_rail_km / pass_od_study$length_km)

foo = pass_od_study[is.na(pass_od_study$length_rail_km),]
qtm(foo)
