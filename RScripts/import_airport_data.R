# list zips

path <- "../LDT/data/CAA_airport/"

zips <- list.files(path, pattern = "zip", full.names = TRUE)

res_transit <- list()
res_int_od <- list()
res_dom_od <- list()

unlink("tmp", recursive = TRUE)

for(i in 1:length(zips)){
  message(zips[i])
  dir.create("tmp")
  unzip(zips[i], exdir = "tmp")
  files_csv <- list.files("tmp", pattern = ".csv", full.names = TRUE)
  if(length(files_csv) == 0){
    files_csv <- list.dirs("tmp", full.names = TRUE, recursive = TRUE)
    files_csv <- files_csv[grep("Annual", files_csv)]
    files_csv <- list.files(files_csv, pattern = ".csv", full.names = TRUE)
  }
  files_xls <- list.files("tmp", pattern = ".xls", full.names = TRUE)
  if(length(files_xls) == 0){
    files_xls <- list.dirs("tmp", full.names = TRUE, recursive = TRUE)
    files_xls <- files_xls[grep("Annual", files_xls)]
    files_xls <- list.files(files_xls, pattern = ".xls", full.names = TRUE)
  }
  
  if(length(files_csv) > 0 & length(files_xls) == 0){
    mode <- "csv"
    files <- files_csv
  }else if(length(files_csv) == 0 & length(files_xls) > 0){
    mode <- "xls"
    files <- files_xls
  }else{
    stop("don' know where files are")
  }
  
  yr <- as.numeric(substr(strsplit(zips[i], "/", fixed = TRUE)[[1]][5],1,4))
  
  if(mode == "csv"){
    transit <- files[grep("transit", files,ignore.case = TRUE)]
    transit <- readr::read_csv(transit)
    if(all(c("this_period","rpt_apt_name","total_pax_tp","term_pax_tp","tran_pax_tp") %in% names(transit))){
      transit <- transit[,c("this_period","rpt_apt_name","total_pax_tp","term_pax_tp","tran_pax_tp")]
    
    } else if (all(c("this_period","airport",paste0("total_pax_",yr),paste0("terminal_pax_",yr),paste0("transit_pax_",yr)) %in% names(transit))){
      transit <- transit[,c("this_period","airport",paste0("total_pax_",yr),paste0("terminal_pax_",yr),paste0("transit_pax_",yr))]
    } else if (all(c("this_period","rpt_apt_name",paste0("total_pax_",yr),paste0("terminal_pax_",yr),paste0("transit_pax_",yr)) %in% names(transit))){
      transit <- transit[,c("this_period","rpt_apt_name",paste0("total_pax_",yr),paste0("terminal_pax_",yr),paste0("transit_pax_",yr))]
    } else {
      stop("unknown names in transit")
    }
    
    
    names(transit) <- c("year","airport","total_pax","terminating_pax","transit_pax")
    
    int_od <- files[grep("ntl_air_pax_route", files,ignore.case = TRUE)] 
    int_od <- readr::read_csv(int_od)
    if(all(c("report_period","uk_apt","foreign_country","foreign_apt","ty_t_pax", "ty_s_pax","ty_c_pax") %in% names(int_od))){
      int_od <- int_od[,c("report_period","uk_apt","foreign_country","foreign_apt","ty_t_pax", "ty_s_pax","ty_c_pax")]
    } else if (all(c("report_period","UK_airport","foreign_country","foreign_airport",paste0(yr,"_total_pax"), paste0(yr,"_scheduled_pax"),paste0(yr,"_charter_pax")) %in% names(int_od))) {
      int_od <- int_od[,c("report_period","UK_airport","foreign_country","foreign_airport",paste0(yr,"_total_pax"), paste0(yr,"_scheduled_pax"),paste0(yr,"_charter_pax"))]
    } else if (all(c("report_period","uk_apt","foreign_country","foreign_apt",paste0(yr,"_total_pax"), paste0(yr,"_scheduled_pax"),paste0(yr,"_charter_pax")) %in% names(int_od))){
      int_od <- int_od[,c("report_period","uk_apt","foreign_country","foreign_apt",paste0(yr,"_total_pax"), paste0(yr,"_scheduled_pax"),paste0(yr,"_charter_pax"))]
    } else {
      stop("unknown names in int_od")
    }
    names(int_od) <- c("year","uk_airport","foreign_country","foreign_airport","total_pax", "scheduled_pax","charter_pax")
    
    dom_od <- files[grep("dom_air_pax_route", files,ignore.case = TRUE)] 
    if(length(dom_od) > 1){
      dom_od <- dom_od[1]
    }
    dom_od <- readr::read_csv(dom_od)
    if(all(c("this_period","apt1_apt_name","apt2_apt_name","total_pax_tp", "total_pax_shd_tp", "total_pax_cht_tp") %in% names(dom_od))){
      dom_od <- dom_od[,c("this_period","apt1_apt_name","apt2_apt_name","total_pax_tp", "total_pax_shd_tp", "total_pax_cht_tp")]
    } else if (all(c("this_period","airport1","airport2",paste0("total_pax_",yr), paste0("total_pax_scheduled_",yr),paste0("total_pax_charter_",yr)) %in% names(dom_od))){
      dom_od <- dom_od[,c("this_period","airport1","airport2",paste0("total_pax_",yr), paste0("total_pax_scheduled_",yr),paste0("total_pax_charter_",yr))]
    } else if (all(c("this_period","apt1_apt_name","apt2_apt_name",paste0("total_pax_",yr), paste0("total_pax_shd_",yr),paste0("total_pax_cht_",yr)) %in% names(dom_od))){
      dom_od <- dom_od[,c("this_period","apt1_apt_name","apt2_apt_name",paste0("total_pax_",yr), paste0("total_pax_shd_",yr),paste0("total_pax_cht_",yr))]
    } else {
      stop("unknown names in dom_od")
    }
    names(dom_od) <- c("year","airport1","airport2","total_pax", "scheduled_pax","charter_pax")
  }
  
  if(mode == "xls"){
    transit <- files[grep("transit", files,ignore.case = TRUE)]
    transit <- readxl::read_excel(transit)
    transit <- transit[11:nrow(transit),]
    names(transit) <- c("airport","total_pax","dud1","dud2","terminating_pax","dud3","dud4","transit_pax","dud5","dud6")
    transit$year <- yr
    transit <- transit[,c("year","airport","total_pax","terminating_pax","transit_pax")]
    transit <- transit[!is.na(transit$total_pax),]
    transit$total_pax <- as.numeric(transit$total_pax)
    transit$terminating_pax <- as.numeric(transit$terminating_pax)
    transit$transit_pax <- as.numeric(transit$transit_pax)
    
    int_od <- files[grep("ntl_air_pax.*route", files,ignore.case = TRUE)] 
    int_od <- readxl::read_excel(int_od)
    int_od <- int_od[10:nrow(transit),]
    if(ncol(int_od) == 11 ){
      names(int_od) <- c("uk_airport","dud1","foreign_airport","dud6","total_pax", "scheduled_pax","charter_pax","dud2","dud3","dud4","dud5")
    }else{
      names(int_od) <- c("uk_airport","dud1","foreign_airport","total_pax", "scheduled_pax","charter_pax","dud2","dud3","dud4","dud5")
    }
    
    int_od$year <- yr
    int_od$foreign_country <- NA
    int_od <- int_od[,c("year","uk_airport","foreign_country","foreign_airport","total_pax", "scheduled_pax","charter_pax")]
    int_od <- int_od[!is.na(int_od$uk_airport),]
    int_od$total_pax <- as.numeric(int_od$total_pax)
    int_od$scheduled_pax <- as.numeric(int_od$scheduled_pax)
    int_od$charter_pax <- as.numeric(int_od$charter_pax)
    
    dom_od <- files[grep("dom_air_pax.*route", files,ignore.case = TRUE)]
    if(length(dom_od) == 0){
      message(paste0("No domestic OD data for ",yr))
    }else{
      stop()
      # dom_od <- readr::read_csv(dom_od)
      # dom_od <- dom_od[,c("this_period","apt1_apt_name","apt2_apt_name","total_pax_tp", "total_pax_shd_tp", "total_pax_cht_tp")]
      # names(dom_od) <- c("year","airport1","airport2","total_pax", "scheduled_pax","charter_pax")
    }

  }
  
  res_transit[[i]] <- transit
  res_int_od[[i]] <- int_od
  res_dom_od[[i]] <- dom_od
  
  rm(transit)
  rm(int_od)
  rm(dom_od)
  rm(files_xls, files_csv, files)
  
  unlink("tmp", recursive = TRUE)
}

for(i in seq(length(zips) + 1, length(zips) + 4)){
  dir <- i - length(zips) + 2014
  files_csv <- list.files(file.path(path, dir), pattern = ".csv", full.names = TRUE)
  files <- files_csv
  
  transit <- files[grep("transit", files,ignore.case = TRUE)]
  transit <- readr::read_csv(transit)
  if(all(c("this_period","rpt_apt_name","total_pax_tp","term_pax_tp","tran_pax_tp") %in% names(transit))){
    transit <- transit[,c("this_period","rpt_apt_name","total_pax_tp","term_pax_tp","tran_pax_tp")]
    
  } else if (all(c("this_period","airport",paste0("total_pax_",yr),paste0("terminal_pax_",yr),paste0("transit_pax_",yr)) %in% names(transit))){
    transit <- transit[,c("this_period","airport",paste0("total_pax_",yr),paste0("terminal_pax_",yr),paste0("transit_pax_",yr))]
  } else if (all(c("this_period","rpt_apt_name",paste0("total_pax_",yr),paste0("terminal_pax_",yr),paste0("transit_pax_",yr)) %in% names(transit))){
    transit <- transit[,c("this_period","rpt_apt_name",paste0("total_pax_",yr),paste0("terminal_pax_",yr),paste0("transit_pax_",yr))]
  } else {
    stop("unknown names in transit")
  }
  
  
  names(transit) <- c("year","airport","total_pax","terminating_pax","transit_pax")
  
  int_od <- files[grep("ntl_air_pax_route", files,ignore.case = TRUE)] 
  int_od <- readr::read_csv(int_od)
  if(all(c("report_period","uk_apt","foreign_country","foreign_apt","ty_t_pax", "ty_s_pax","ty_c_pax") %in% names(int_od))){
    int_od <- int_od[,c("report_period","uk_apt","foreign_country","foreign_apt","ty_t_pax", "ty_s_pax","ty_c_pax")]
  } else if (all(c("report_period","UK_airport","foreign_country","foreign_airport",paste0(yr,"_total_pax"), paste0(yr,"_scheduled_pax"),paste0(yr,"_charter_pax")) %in% names(int_od))) {
    int_od <- int_od[,c("report_period","UK_airport","foreign_country","foreign_airport",paste0(yr,"_total_pax"), paste0(yr,"_scheduled_pax"),paste0(yr,"_charter_pax"))]
  } else if (all(c("report_period","uk_apt","foreign_country","foreign_apt",paste0(yr,"_total_pax"), paste0(yr,"_scheduled_pax"),paste0(yr,"_charter_pax")) %in% names(int_od))){
    int_od <- int_od[,c("report_period","uk_apt","foreign_country","foreign_apt",paste0(yr,"_total_pax"), paste0(yr,"_scheduled_pax"),paste0(yr,"_charter_pax"))]
  } else {
    stop("unknown names in int_od")
  }
  names(int_od) <- c("year","uk_airport","foreign_country","foreign_airport","total_pax", "scheduled_pax","charter_pax")
  
  dom_od <- files[grep("dom_air_pax_route", files,ignore.case = TRUE)] 
  if(length(dom_od) > 1){
    dom_od <- dom_od[1]
  }
  dom_od <- readr::read_csv(dom_od)
  if(all(c("this_period","apt1_apt_name","apt2_apt_name","total_pax_tp", "total_pax_shd_tp", "total_pax_cht_tp") %in% names(dom_od))){
    dom_od <- dom_od[,c("this_period","apt1_apt_name","apt2_apt_name","total_pax_tp", "total_pax_shd_tp", "total_pax_cht_tp")]
  } else if (all(c("this_period","airport1","airport2",paste0("total_pax_",yr), paste0("total_pax_scheduled_",yr),paste0("total_pax_charter_",yr)) %in% names(dom_od))){
    dom_od <- dom_od[,c("this_period","airport1","airport2",paste0("total_pax_",yr), paste0("total_pax_scheduled_",yr),paste0("total_pax_charter_",yr))]
  } else if (all(c("this_period","apt1_apt_name","apt2_apt_name",paste0("total_pax_",yr), paste0("total_pax_shd_",yr),paste0("total_pax_cht_",yr)) %in% names(dom_od))){
    dom_od <- dom_od[,c("this_period","apt1_apt_name","apt2_apt_name",paste0("total_pax_",yr), paste0("total_pax_shd_",yr),paste0("total_pax_cht_",yr))]
  } else {
    stop("unknown names in dom_od")
  }
  names(dom_od) <- c("year","airport1","airport2","total_pax", "scheduled_pax","charter_pax")
  
  res_transit[[i]] <- transit
  res_int_od[[i]] <- int_od
  res_dom_od[[i]] <- dom_od
  
  rm(transit)
  rm(int_od)
  rm(dom_od)
  rm(files_csv, files)
  
}



transit <- dplyr::bind_rows(res_transit)
int_od <- dplyr::bind_rows(res_int_od)
dom_od <- dplyr::bind_rows(res_dom_od[lengths(res_dom_od) >0])

saveRDS(transit, "data/CAA_transit.Rds")
saveRDS(int_od, "data/CAA_int_od.Rds")
saveRDS(dom_od, "data/CAA_dom_od.Rds")
