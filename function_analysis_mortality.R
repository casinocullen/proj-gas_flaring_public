### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
###   NAME: AP3 Mortality Function
### AUTHOR: Chen Chen
### OUTPUT: Function
### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

ap3_mort_analysis <- function(file) {
  library(dplyr)
  library(stringr)
  library(data.table)
  library(janitor)
  library(sf)
  
  x.ap3.county = readRDS('../base/ap3_county_fips.RDS')
  
  # Base health stats
  WTP_Mort = 9186210
  DoseResponseAdult= 0.005826891; # Krewski
  DoseResponseInfant=0.006765865; # Woodruff 2006
  
  x.pop = read.table('../model-ap3/pop_2014.csv', sep = ',') %>% 
    data.matrix()
  
  x.pop.infant = x.pop
  x.pop.infant[,2:19] = 0
  
  x.pop.30 = x.pop
  x.pop.30[,1:7] = 0
  
  
  x.mort = read.table('../model-ap3/mort_2014.csv', sep = ',') %>% 
    data.matrix()
  
  x.death.total.ori = readRDS('./output/x.death.total.ori.RDS')
  ### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  # PM25 data
  x.pm.new = fread(paste0('../model-ap3/', file), col.names = 'pm_25_conc') %>% 
    pull()
  ### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  # Deaths + infant deaths from new
  x.death.new = x.mort * (1-(1/exp(DoseResponseAdult * x.pm.new))) * x.pop.30
  x.infant.death.new = x.mort * (1-(1/exp(DoseResponseInfant * x.pm.new))) * x.pop.infant
  x.death.total.new = rowSums(x.death.new) + rowSums(x.infant.death.new)
  
  ### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  # Combine to a df
  x.health.outcomes.county = data.frame(ap3_county_fips = x.ap3.county,
                                        mort_ori = x.death.total.ori, 
                                        mort_new = x.death.total.new
  ) %>% 
    mutate(mort_diff = mort_new - mort_ori)
  
  return(x.health.outcomes.county)
  
}

