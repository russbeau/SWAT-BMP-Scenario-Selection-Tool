SWAT_CHANHYD_Mod <- function(path,template,BED_K_VALUE) {
  
  var1 <- sprintf("%07.5f", BED_K_VALUE)
  
  alphatable <- readLines(paste(template,"hyd-sed-lte_template.txt",sep="")) |>
    stringr::str_replace_all(c("replace" = var1))
  writeLines(alphatable, con = paste(txtinoutpath,"/hyd-sed-lte.cha",sep=""))
  
}