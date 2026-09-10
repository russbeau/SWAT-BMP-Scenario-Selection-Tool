### Create FarmingScenariosMaster.csv ###
#Compile the outputs of FarmingScenarioRunner.R into one csv file

setwd("D:/SWAT_Watershed_Tool")

master <- read.csv("FarmingScenariosMasterTemplate.csv", check.names = FALSE)

#Assign watershed names and desired abbreviations for column naming
watersheds <- c("Watershed1", "Watershed2", "Watershed3")
abbrev <- c("WS1", "WS2", "WS3")
calibration <- c(1:5)

for (i in 1:3) {
  for (j in calibration) {
    
    temp <- read.csv(
      paste0("D:/SWAT_Watershed_Tool/",
             watersheds[i],
             "/ModelOutput/FarmingScenarios/Calibration_",
             j, "/FarmingScenarios_", abbrev[i], j, ".csv")
    )
    
    suffix <- paste0(abbrev[i], j)
    
    master[[paste0("TN_", suffix)]]  <- temp[["TN"]]
    master[[paste0("TDP_", suffix)]] <- temp[["TDP"]]
    master[[paste0("CropYield_", suffix)]] <- temp[["crop_yield"]]
  }
}


# Calculate row-wise mean/min/max across all watersheds and all calibration columns

variables <- c("TN", "TDP", "CropYield")

for (var in variables) {
  
  var_cols <- grep(paste0("^", var, "_"), colnames(master), value = TRUE)
  
  master[[paste0(var, "_mean")]] <- rowMeans(master[, var_cols], na.rm = TRUE)
  master[[paste0(var, "_min")]]  <- apply(master[, var_cols], 1, min, na.rm = TRUE)
  master[[paste0(var, "_max")]]  <- apply(master[, var_cols], 1, max, na.rm = TRUE)
}

write.csv(master, "D:/SWAT_Watershed_Tool/FarmingScenariosMaster.csv",
          row.names = FALSE, quote = FALSE)
