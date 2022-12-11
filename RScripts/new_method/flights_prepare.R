flight_od <- readRDS("data/clean/flights_od_2021.Rds")

# Flight has an AB BA problem in domestic data
flight_od$key <- stplanr::od_id_szudzik(flight_od$airport1, flight_od$airport2)

flight_od <- flight_od %>%
  group_by(key) %>%
  summarise(airport1 = airport1[1],
            airport1_country = airport1_country[1],
            airport2 = airport2[1],
            airport2_country = airport2_country[1],
            flt_1990 = max(c(flt_1990,0), na.rm = TRUE),
            flt_1991 = max(c(flt_1991,0), na.rm = TRUE),
            flt_1992 = max(c(flt_1992,0), na.rm = TRUE),
            flt_1993 = max(c(flt_1993,0), na.rm = TRUE),
            flt_1994 = max(c(flt_1994,0), na.rm = TRUE),
            flt_1995 = max(c(flt_1995,0), na.rm = TRUE),
            flt_1996 = max(c(flt_1996,0), na.rm = TRUE),
            flt_1997 = max(c(flt_1997,0), na.rm = TRUE),
            flt_1998 = max(c(flt_1998,0), na.rm = TRUE),
            flt_1999 = max(c(flt_1999,0), na.rm = TRUE),
            flt_2000 = max(c(flt_2000,0), na.rm = TRUE),
            flt_2001 = max(c(flt_2001,0), na.rm = TRUE),
            flt_2002 = max(c(flt_2002,0), na.rm = TRUE),
            flt_2003 = max(c(flt_2003,0), na.rm = TRUE),
            flt_2004 = max(c(flt_2004,0), na.rm = TRUE),
            flt_2005 = max(c(flt_2005,0), na.rm = TRUE),
            flt_2006 = max(c(flt_2006,0), na.rm = TRUE),
            flt_2007 = max(c(flt_2007,0), na.rm = TRUE),
            flt_2008 = max(c(flt_2008,0), na.rm = TRUE),
            flt_2009 = max(c(flt_2009,0), na.rm = TRUE),
            flt_2010 = max(c(flt_2010,0), na.rm = TRUE),
            flt_2011 = max(c(flt_2011,0), na.rm = TRUE),
            flt_2012 = max(c(flt_2012,0), na.rm = TRUE),
            flt_2013 = max(c(flt_2013,0), na.rm = TRUE),
            flt_2014 = max(c(flt_2014,0), na.rm = TRUE),
            flt_2015 = max(c(flt_2015,0), na.rm = TRUE),
            flt_2016 = max(c(flt_2016,0), na.rm = TRUE),
            flt_2017 = max(c(flt_2017,0), na.rm = TRUE),
            flt_2018 = max(c(flt_2018,0), na.rm = TRUE),
            flt_2019 = max(c(flt_2019,0), na.rm = TRUE),
            flt_2020 = max(c(flt_2020,0), na.rm = TRUE),
            flt_2021 = max(c(flt_2021,0), na.rm = TRUE)
            )

flight_od$key <- NULL


flight_od$airport1 <- gsub(" International","",flight_od$airport1)
flight_od$airport2 <- gsub(" International","",flight_od$airport2)
flight_od$airport1 <- gsub(" Int'l","",flight_od$airport1)
flight_od$airport2 <- gsub(" Int'l","",flight_od$airport2)
flight_od$airport1 <- gsub(" Intl","",flight_od$airport1)
flight_od$airport2 <- gsub(" Intl","",flight_od$airport2)
flight_od$airport1 <- gsub(" Int","",flight_od$airport1)
flight_od$airport2 <- gsub(" Int","",flight_od$airport2)
flight_od$airport1 <- gsub(" Airport","",flight_od$airport1)
flight_od$airport2 <- gsub(" Airport","",flight_od$airport2)
flight_od$airport1 <- gsub(" Apt","",flight_od$airport1)
flight_od$airport2 <- gsub(" Apt","",flight_od$airport2)

flight_od$airport1_country[flight_od$airport1 == "Jersey"] <- "Jersey"
flight_od$airport2_country[flight_od$airport2 == "Jersey"] <- "Jersey"
flight_od$airport1_country[flight_od$airport1 == "Guernsey"] <- "Guernsey"
flight_od$airport2_country[flight_od$airport2 == "Guernsey"] <- "Guernsey"
flight_od$airport1_country[flight_od$airport1 == "Isle of Man"] <- "Isle of Man"
flight_od$airport2_country[flight_od$airport2 == "Isle of Man"] <- "Isle of Man"

saveRDS(flight_od, "data/clean/flights_od_prepped_2021.Rds")
