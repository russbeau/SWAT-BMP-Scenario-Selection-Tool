#Farming_Scenario_Runner
#Runs preassigned agricultural BMP scenarios in SWAT+ model with multiple parameter calibration sets

library(stringr)
library(data.table)

#Assign watershed to be run
watershed = "Watershed"

setwd(paste0("D:/SWAT_Watershed_Tool/", watershed, "/TxtInOut"))

source("D:/SWAT_Watershed_Tool/SWAT_SOILS_Mod.R")
source("D:/SWAT_Watershed_Tool/SWAT_AQUIFER_Mod.R")
source("D:/SWAT_Watershed_Tool/SWAT_CHANHYD_Mod.R")
source("D:/SWAT_Watershed_Tool/SWAT_CN_Mod.R")
source("D:/SWAT_Watershed_Tool/SWAT_HYDROLOGY_Mod.R")
source("D:/SWAT_Watershed_Tool/SWAT_SNOW_Mod.R")
source("D:/SWAT_Watershed_Tool/SWAT_BASIN_Mod.R")
source("D:/SWAT_Watershed_Tool/SWAT_MANAGEMENT_Mod.R")
source("D:/SWAT_Watershed_Tool/SWAT_FILTERSTRIP_Mod.R")

# Set paths for SWAT+ Input/Output Files
template = paste0("D:/SWAT_Watershed_Tool/", watershed, "/SWAT_par_temps/", watershed, "_")
txtinoutpath = paste0("D:/SWAT_Watershed_Tool/", watershed, "/TxtInOut")

#Update SWAT+ models with calibrated parameter sets
calibration_parameters <- read.csv(paste("D:/SWAT_Watershed_Tool/",watershed,"/ModelOutput/CDF/CDF_Simulations.csv",sep=""))
calibration_parameters <- calibration_parameters[,1:27]
calibration_parameters$calibration <- c(1:5)

#Set TxtInOut files to parameter values for given calibration
#Change calibration value for each run of the for loop
calibration <- 1 #this should be set to 1,2,3,4,&5 to complete this script

outpath = paste0("D:/SWAT_Watershed_Tool/", watershed, "/ModelOutput/FarmingScenarios/Calibration_",calibration,"/", sep="")

SWAT_SOILS_Mod(txtinoutpath,template,calibration_parameters$awc[calibration],calibration_parameters$k1[calibration],calibration_parameters$k2[calibration])
SWAT_AQUIFER_Mod(txtinoutpath,template,calibration_parameters$alpha[calibration],calibration_parameters$bf_max[calibration],calibration_parameters$flo_min[calibration])
SWAT_CHANHYD_Mod(txtinoutpath,template,calibration_parameters$bed_K[calibration])
SWAT_CN_Mod(txtinoutpath,template,calibration_parameters$cn2_mult[calibration])
SWAT_HYDROLOGY_Mod(txtinoutpath,template,calibration_parameters$lat_time[calibration],calibration_parameters$esco[calibration],calibration_parameters$epco[calibration])
SWAT_SNOW_Mod(txtinoutpath,template,calibration_parameters$fall_tmp[calibration],calibration_parameters$melt_tmp[calibration],calibration_parameters$melt_max[calibration],calibration_parameters$melt_min[calibration],calibration_parameters$melt_lag[calibration])
SWAT_BASIN_Mod(txtinoutpath,template,calibration_parameters$surlag[calibration],calibration_parameters$orgn_min[calibration],calibration_parameters$n_uptake[calibration],calibration_parameters$p_uptake[calibration],calibration_parameters$n_perc[calibration],
               calibration_parameters$p_perc[calibration],calibration_parameters$p_soil[calibration],calibration_parameters$p_avail[calibration],calibration_parameters$rsd_decomp[calibration],calibration_parameters$denit_exp[calibration],calibration_parameters$denit_frac[calibration])

#Read ag + hay HRU areas to compute output by land area
hru_list <- read.csv(paste("D:/SWAT_Watershed_Tool/", watershed, "/SWAT_par_temps/hru_list.csv",sep=""))
hru_area <- sum(hru_list$area)

#Read files to be edited and rewritten by the script
farming_scenarios <- read.csv(paste0(outpath,"FarmingScenarios.csv",sep=""))
filter_template <- paste0("D:/SWAT_Watershed_Tool/",watershed,"/SWAT_par_temps/filterstrip_template.txt",sep="")
landuse_template <- paste0("D:/SWAT_Watershed_Tool/",watershed,"/SWAT_par_temps/landusetemplate.txt",sep="")

#If you need to stop and restart this script at any point, set restart to the next iteration of the for loop
restart <- 1

#Run script
for(i in restart:7200){
  management_template <- paste0("D:/SWAT_Watershed_Tool/",watershed,"/SWAT_par_temps/",farming_scenarios$management_template[i],sep="")
  
  #Update BMP scenario
  SWAT_MANAGEMENT_Mod(
    txtinoutpath,
    management_template,
    farming_scenarios$spring_fertilizer_date[i],
    farming_scenarios$NP_ratio[i],
    sprintf("%10.5f", as.numeric(farming_scenarios$spring_fertilizer[i])),
    farming_scenarios$fertilizer_injection[i],
    sprintf("%10.5f", as.numeric(farming_scenarios$summer_fertilizer[i])),
    sprintf("%2s", farming_scenarios$tilling_date[i])
  )
  

  
  SWAT_FILTERSTRIP_Mod(txtinoutpath,filter_template,landuse_template,
                       farming_scenarios$tilling[i],sprintf("%9.5f",farming_scenarios$filter[i]))
  
  #Run SWAT+
  system(paste(txtinoutpath,"/rev60.5.7_64rel.exe",sep=""), intern = TRUE)
  
  #Read and format HRU outputs
  nutrients <- fread(paste(txtinoutpath,"/hru_ls_aa.csv",sep=""), skip = 1, header = TRUE)
  nutrients_filtered <- nutrients[nutrients$name %in% hru_list$name,]
  nutrients_filtered[, 9:13] <- lapply(nutrients_filtered[, 9:13], as.numeric)
  area_lookup <- setNames(hru_list$area, hru_list$name)
  nutrients_filtered[, 9:13] <- nutrients_filtered[, 9:13] * area_lookup[nutrients_filtered$name]
  nutrients_filtered <- nutrients_filtered[,c(7,9:13)]
  #Normalize values by HRU area
  sedorgn <- sum(nutrients_filtered$sedorgn) / hru_area
  sedorgp <- sum(nutrients_filtered$sedorgp) / hru_area
  surqsolp <- sum(nutrients_filtered$surqsolp) / hru_area
  surqno3 <- sum(nutrients_filtered$surqno3) / hru_area
  lat3no3 <- sum(nutrients_filtered$lat3no3) / hru_area
  
  totals_row <- data.frame(
    name = "TOTAL_AA",
    sedorgn = sedorgn,
    sedorgp = sedorgp,
    surqsolp = surqsolp,
    surqno3 = surqno3,
    lat3no3 = lat3no3
  )
  
  nutrients_filtered <- rbind(nutrients_filtered, totals_row)
  
  #Write nutrient outputs to csv
  write.csv(nutrients_filtered, paste(outpath, "Scenario_",i,"_hru_Total_Load.csv",sep = ""), row.names=FALSE,quote=FALSE)

  #Read crop yield output
  crop_yield <- fread(paste(txtinoutpath,"/basin_crop_yld_yr.txt",sep=""),skip=1)[plant_name == "NE_C"]
  crop_avg <- mean(crop_yield$`yld(t/ha)`[7:14])
  write.csv(crop_yield, paste(outpath, "Scenario_",i,"_CropYield.csv",sep=""), row.names=FALSE,quote=FALSE)
  
  
  farming_scenarios$TN[i] <- sedorgn + surqno3 + lat3no3
  farming_scenarios$TP[i] <- sedorgp + surqsolp
  farming_scenarios$TDP[i] <- surqsolp
  farming_scenarios$crop_yield[i] <- crop_avg
  #Write BMP scenario outputs into FarmingScenarios.csv
  write.csv(farming_scenarios, paste(outpath,"FarmingScenarios.csv",sep = ""),row.names=FALSE,quote=FALSE)

  rm(nutrients)
  rm(crop_yield)
  rm(sedorgn)
  rm(sedorgp)
  rm(surqno3)
  rm(surqsolp)
  rm(lat3no3)
  rm(nutrients_filtered)
  rm(totals_row)
  rm(crop_avg)
  gc()
  
  print(paste("Simulation", i, "of", length(farming_scenarios$scenario), sep = " "))
}
