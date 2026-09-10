SWAT_SOILS_Mod <- function(path,template,AWC_Value,K1_Value,K2_Value) {
  
  awc <- sprintf("%07.5f", AWC_Value)
  if (K1_Value < 10) {
      k1 <- as.character(sprintf("%07.5f", K1_Value))
    } else {
      k1 <- as.character(sprintf("%07.4f", K1_Value))
    }

  if (K1_Value < 10) {
      k2 <- as.character(sprintf("%07.5f", K2_Value))
    } else {
      k2 <- as.character(sprintf("%07.4f", K2_Value))
    }

  k1table <- readLines(paste(template,"soils_template.txt",sep="")) |>
    stringr::str_replace_all(c("changeme" = awc, "k1change"= k1, "k2change" = k2))
  writeLines(k1table, con = paste(path,"/soils.sol",sep=""))
  
}