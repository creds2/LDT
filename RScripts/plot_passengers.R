library(sf)
library(dplyr)
library(tmap)
tmap_mode("view")

airports_all <- read_sf("data/airports_pass.gpkg")

pass_int_od <- readRDS("data/CAA_int_od_clean.Rds")
pass_dom_od <- readRDS("data/CAA_dom_od_clean.Rds")

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
