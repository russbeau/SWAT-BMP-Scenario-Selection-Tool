SWAT_CN_Mod <- function(path,template,CN_Mod_Value) {
  
  CN_Table = read.csv(paste(template,"SWAT_CN_Table.csv",sep=""))
  CN_Table_Mod = CN_Table
  CN_Table_Mod[,2:5] = CN_Table_Mod[,2:5]*CN_Mod_Value

  CN_Table_Mod$cn_a[CN_Table_Mod$cn_a < 15] = 15
  CN_Table_Mod$cn_b[CN_Table_Mod$cn_b < 15] = 15
  CN_Table_Mod$cn_c[CN_Table_Mod$cn_c < 15] = 15
  CN_Table_Mod$cn_d[CN_Table_Mod$cn_d < 15] = 15
  
  CN_Table_Mod$cn_a[CN_Table_Mod$cn_a > 98] = 98
  CN_Table_Mod$cn_b[CN_Table_Mod$cn_b > 98] = 98
  CN_Table_Mod$cn_c[CN_Table_Mod$cn_c > 98] = 98
  CN_Table_Mod$cn_d[CN_Table_Mod$cn_d > 98] = 98
  
  Lines <- do.call("sprintf", c("%-22.22s%-14.5f%-14.5f%-14.5f%-14.5f%-72.60s%-42.30s%-18.30s", CN_Table_Mod))
  Lines[2:53] <- Lines[1:52]
  temp = as.list(paste(as.character(colnames(CN_Table)),sep = " "))
  Lines[1] = do.call("sprintf",c("%-26.26s%-14.14s%-14.14s%-14.14s%-10.10s%-72.11s%-42.42s%-18.18s",temp))
  write.table(Lines,paste(path,"/cntable.lum",sep=""),row.names=FALSE,sep="\t", quote = FALSE)
  
}