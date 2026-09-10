SWAT_AQUIFER_Mod <- function(path,template,ALPHA_Value,BF_MAX,FLO_MIN) {
  
  var1 <- sprintf("%07.5f", ALPHA_Value)
  var2 <- sprintf("%07.5f", BF_MAX)
  var3 <- sprintf("%07.5f", FLO_MIN)
  
  alphatable <- readLines(paste(template,"aquifer_template.txt",sep="")) |>
    stringr::str_replace_all(c("ALPHABF" = var1, "BFL_MAX" = var2, "FLO_MIN" = var3))
  writeLines(alphatable, con = paste(txtinoutpath,"/aquifer.aqu",sep=""))
  
}