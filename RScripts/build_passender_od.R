library(dplyr)
library(sf)
library(tidyr)

airports <- read_sf("data/airports_pass.gpkg")
pass_int_od <- readRDS("data/CAA_int_od_clean.Rds")
pass_dom_od <- readRDS("data/CAA_dom_od_clean.Rds")


head(pass_dom_od)
head(pass_int_od)

names(pass_int_od) <- c("year","airport1","airport2_country", "airport2", "total_pax","scheduled_pax","charter_pax" )
pass_int_od$airport1_country <- "united kingdom"

pass_dom_od$airport1_country <- "united kingdom"
pass_dom_od$airport2_country <- "united kingdom"

pass_od <- bind_rows(pass_int_od, pass_dom_od)

pass_od$airport1 <- tolower(pass_od$airport1)
pass_od$airport1 <- tools::toTitleCase(pass_od$airport1)
pass_od$airport2 <- tools::toTitleCase(pass_od$airport2)
pass_od$airport1_country <- tools::toTitleCase(pass_od$airport1_country)
pass_od$airport2_country <- tools::toTitleCase(pass_od$airport2_country)




# foo <- pass_od[,c("year","airport1","airport1_country","airport2","airport2_country","airport2")]
# foo <- pass_od[duplicated(foo),]
# bar <- pass_od[pass_od$year == foo$year[1] & pass_od$airport1 == foo$airport1[1] & pass_od$airport2 == foo$airport2[1], ]

pass_od <- pass_od[,c("year","airport1","airport1_country","airport2","airport2_country","total_pax")]

# combine some duplicates
# come from two airports that seem to be the same location

pass_od <- pass_od %>%
  group_by(year,airport1,airport1_country,airport2,airport2_country,airport2) %>%
  summarise(total_pax = sum(total_pax))


pass_od_wide <- pivot_wider(pass_od, names_from = "year", values_from = "total_pax")

saveRDS(pass_od_wide, "data/passenger_od_wide.Rds")
