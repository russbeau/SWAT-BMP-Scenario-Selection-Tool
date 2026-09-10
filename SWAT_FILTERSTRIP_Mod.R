SWAT_FILTERSTRIP_Mod <- function(path,filter_template,landuse_template,tilling,filter_amount) {
  
  if(filter_amount == "  0.00000"){
    filterstrp <- "        null"
  }else{
    filterstrp <- "field_border"
  }
  
  if(tilling == FALSE){
    ov_mann <- "notill_0.5-1res"
  }else{
    ov_mann <- "   convtill_res"
  }
  
  
  filter_str <- readLines(filter_template) |>
    stringr::str_replace_all(c("VFSVFSVFS"=filter_amount))
  writeLines(filter_str, con = paste(path,"/filterstrip.str",sep=""))
  
  land_lum <- readLines(landuse_template) |>
    stringr::str_replace_all(c("xxxxxxxxxxxxxxx"=ov_mann,"yyyyyyyyyyyy"=filterstrp))
  writeLines(land_lum, con = paste(path,"/landuse.lum",sep=""))
}