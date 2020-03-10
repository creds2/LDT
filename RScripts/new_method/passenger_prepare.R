library(dplyr)
library(tidyr)

pass_int_od <- readRDS("data/clean/passenger_int_od.Rds")
pass_dom_od <- readRDS("data/clean/passenger_dom_od.Rds")

names(pass_int_od) <- c("year","airport1","airport2_country", "airport2", "total_pax","scheduled_pax","charter_pax" )
pass_int_od$airport1_country <- "United Kingdom"

# Clean Dud Data
pass_int_od <- pass_int_od[!grepl("Total ",pass_int_od$airport1),]
pass_int_od <- pass_int_od[!is.na(pass_int_od$total_pax),]
pass_int_od$airport1[pass_int_od$airport1 == "OTHER ROUTES"] <- "Other Airport"
pass_int_od$airport2[pass_int_od$airport2_country == "OIL RIGS"] <- "Oil Rigs"
pass_int_od$airport2[is.na(pass_int_od$airport2)] <- "Other Airport"

pass_dom_od$airport1_country <- "United Kingdom"
pass_dom_od$airport2_country <- "United Kingdom"

pass_od <- bind_rows(pass_int_od, pass_dom_od)

pass_od$airport1 <- tools::toTitleCase(tolower(pass_od$airport1))
pass_od$airport2 <- tools::toTitleCase(tolower(pass_od$airport2))
pass_od$airport1_country <- tools::toTitleCase(tolower(pass_od$airport1_country))
pass_od$airport2_country <- tools::toTitleCase(tolower(pass_od$airport2_country))
pass_od <- pass_od[,c("year","airport1","airport1_country","airport2","airport2_country","total_pax")]


pass_od[duplicated(pass_od[,c("year","airport1","airport1_country","airport2","airport2_country")]),]

# combine some duplicates
# come from two airports that seem to be the same location
# In Istanbul

pass_od <- pass_od %>%
  group_by(year,airport1,airport1_country,airport2,airport2_country,airport2) %>%
  summarise(total_pax = sum(total_pax))

pass_od$airport1 <- gsub(" International","",pass_od$airport1)
pass_od$airport2 <- gsub(" International","",pass_od$airport2)
pass_od$airport1 <- gsub(" Int'l","",pass_od$airport1)
pass_od$airport2 <- gsub(" Int'l","",pass_od$airport2)
pass_od$airport1 <- gsub(" Intl","",pass_od$airport1)
pass_od$airport2 <- gsub(" Intl","",pass_od$airport2)
pass_od$airport1 <- gsub(" Int","",pass_od$airport1)
pass_od$airport2 <- gsub(" Int","",pass_od$airport2)
pass_od$airport1 <- gsub(" Airport","",pass_od$airport1)
pass_od$airport2 <- gsub(" Airport","",pass_od$airport2)
pass_od$airport1 <- gsub(" Apt","",pass_od$airport1)
pass_od$airport2 <- gsub(" Apt","",pass_od$airport2)

pass_od_wide <- pivot_wider(pass_od, names_from = "year", values_from = "total_pax")

saveRDS(pass_od_wide, "data/clean/passenger_od_wide.Rds")
