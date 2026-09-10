SWAT_SNOW_Mod <- function(path,template,FALL_TMP,MELT_TMP,MELT_MAX,MELT_MIN,MELT_LAG) {
  fall_tmp <- sprintf("%08.5f", FALL_TMP)
  melt_tmp <- sprintf("%08.5f", MELT_TMP)
  melt_max <- sprintf("%08.5f", MELT_MAX)
  melt_min <- sprintf("%08.5f", MELT_MIN)
  melt_lag <- sprintf("%07.5f", MELT_LAG)
  snowtable <- readLines(paste(template,"snow_template.txt",sep="")) |>
    stringr::str_replace_all(c("snowfall" = fall_tmp, "snowmelt" = melt_tmp, "snow.max" = melt_max, "snow.min" = melt_min, "snowlag" = melt_lag))
  writeLines(snowtable, con = paste(path,"/snow.sno",sep=""))
}