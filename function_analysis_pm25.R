### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
###   NAME: AP3 PM2.5 Function
### AUTHOR: Chen Chen
### OUTPUT: Maps
### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

ap3_pm25_analysis <- function(file) {
  library(dplyr)
  library(data.table)

  # Read data
  x.ap3.county = readRDS('../base/ap3_county_fips.RDS')
  
  x.pm.original = fread('../model-ap3/ori_pm_25_ugm3.csv', col.names = 'pm_25_conc')
  x.pm.new.ef = fread(paste0('../model-ap3/', file), col.names = 'pm_25_conc')
  
  # Add county fips
  x.pm.original.county = x.pm.original %>% 
    cbind(x.ap3.county)
  x.pm.new.county = x.pm.new.ef %>% 
    cbind(x.ap3.county)
  
  # Join
  x.pm.compare = x.pm.new.county %>% 
    full_join(x.pm.original.county, by = 'ap3_county_fips', suffix = c('_new', '_ori')) %>% 
    select(ap3_county_fips, pm_25_conc_ori, pm_25_conc_new) %>% 
    mutate(diff = pm_25_conc_new - pm_25_conc_ori)
  
  return(x.pm.compare)
  
}

