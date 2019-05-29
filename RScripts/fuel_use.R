library(readxl)

fuel <- read_xlsx("data/Fuel Consumption - Wikipedia.xlsx")
sec <- strsplit(fuel$Sector, '(', fixed = TRUE)
sec <- sapply(sec, function(x){
  if(is.na(x[1])){
    return(NA)
  }else{
    return(x[2])
  }
})
sec <- gsub(",","",sec)
sec <- gsub("km)","",sec)
sec <- gsub("[[:space:]]","",sec)
fuel$Sector <- as.numeric(sec)
fuel <- fuel[,c("Model","First_flight","Seats","Sector","Type","fuel_burn","fuel_per_seat")]
saveRDS(fuel,"data/Fuel Consumption - Wikipedia.Rds")


routes_uk <- readRDS("data/OpenFlights/UKflights.Rds")
Aircraft <- as.data.frame(table(routes_uk$Aircraft))

fuel <- fuel[!duplicated(fuel$Model),]

foo <- as.data.frame(table(routes_uk$Aircraft))
foo <- left_join(foo, fuel, by = c("Var1" = "Model"))