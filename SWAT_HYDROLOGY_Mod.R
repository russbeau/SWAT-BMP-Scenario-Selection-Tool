SWAT_HYDROLOGY_Mod <- function(path,template,LATTIME,ESCO_Mod_Value,EPCO_Mod_Value) {
  
  lat_time <- sprintf("%07.5f", LATTIME)
  esco <- sprintf("%07.5f", ESCO_Mod_Value)
  epco <- sprintf("%07.5f", EPCO_Mod_Value)
  
  hydtable <- readLines(paste(template,"hydrology_template.txt",sep="")) |>
    stringr::str_replace_all(c("lattime"=lat_time, "escoval" = esco, "epcoval" = epco))
  writeLines(hydtable, con = paste(path,"/hydrology.hyd",sep=""))
  
}