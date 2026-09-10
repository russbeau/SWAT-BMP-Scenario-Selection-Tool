SWAT_MANAGEMENT_Mod <- function(path,management_template,spring_fertilizer_date,NP_ratio,spring_fertilizer,fertilizer_injection,summer_fertilizer,till_date) {

  management_sch <- readLines(management_template)
  
  management_sch <- gsub("&", spring_fertilizer_date, management_sch, fixed = TRUE)
  management_sch <- gsub("FERTILIZ", NP_ratio, management_sch, fixed = TRUE)
  management_sch <- gsub("APPLICATIONXX", fertilizer_injection, management_sch, fixed = TRUE)
  management_sch <- gsub("FERT_AMNT1", spring_fertilizer, management_sch, fixed = TRUE)
  management_sch <- gsub("FERT_AMNT2", summer_fertilizer, management_sch, fixed = TRUE)
  management_sch <- gsub("@@", till_date, management_sch, fixed = TRUE)
  writeLines(management_sch, con = paste(path,"/management.sch",sep=""))
}
