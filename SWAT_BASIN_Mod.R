SWAT_BASIN_Mod <- function(path,template,SURLAG_Mod,ORGN_MIN_Mod,N_UPTAKE_Mod,P_UPTAKE_Mod,N_PERC_Mod,P_PERC_Mod,P_SOIL_Mod,P_AVAIL_Mod,RSD_DECOMP_Mod,DENIT_EXP_Mod,DENIT_FRAC_Mod) {
  surlag <- sprintf("%08.5f", SURLAG_Mod)
  orgn_min <- sprintf("%07.5f", ORGN_MIN_Mod)
  n_uptake <- sprintf("%08.5f", N_UPTAKE_Mod)
  p_uptake <- sprintf("%08.5f", P_UPTAKE_Mod)
  n_perc <- sprintf("%07.5f", N_PERC_Mod)
  p_perc <- sprintf("%08.5f", P_PERC_Mod)
  p_soil <- sprintf("%09.5f", P_SOIL_Mod)
  p_avail <- sprintf("%07.5f", P_UPTAKE_Mod)
  rsd_decomp <- sprintf("%07.5f", RSD_DECOMP_Mod)
  denit_exp <- sprintf("%07.5f", DENIT_EXP_Mod)
  denit_frac <- sprintf("%07.5f", DENIT_FRAC_Mod)
  
  bsntable <- readLines(paste(template,"basin_template.txt",sep="")) |>
    stringr::str_replace_all(c("lag.surq"=surlag,"minorgn"=orgn_min,"nitruptk"=n_uptake,"phosuptk"=p_uptake,"percont"=n_perc,"percopho"=p_perc,"soilphosp"=p_soil,
                               "availph"=p_avail,"decompr"=rsd_decomp,"expdent"=denit_exp,"fracden"=denit_frac))
  writeLines(bsntable, con = paste(path,"/parameters.bsn",sep=""))
}