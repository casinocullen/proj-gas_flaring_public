### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
###   NAME: Compare EF Function
### AUTHOR: Chen Chen
### OUTPUT: Maps
### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# input_ef = 0.57

ap3_compare_ef <- function(input_ef) {
  # library(dplyr)
  # library(data.table)
  # library(janitor)
  # library(sf)
  # library(tidyr)
  # library(readxl)
  # library(stringr)
  # library(matlabr)
  
  source('./function_analysis_pm25.R')
  source('./function_analysis_mortality.R')
  options(warn = - 1) 
  # Prepare data
  ap3.county = readRDS('../base/ap3_county_fips.RDS')
  all.county = st_read('../base/layer/us_county_geo.gpkg', quiet = T) %>% st_drop_geometry()
  
  x.raw.emission = read_xlsx('/Users/chenchen/Desktop/Research/Gas_Flaring/2019flares_bcm_g black carbon.xlsx', sheet = 'flareswstatesandcounties') %>% 
    clean_names() %>% 
    mutate(fips = paste0(str_pad(statefp, 2, 'left', '0'), str_pad(countyfp, 3, 'left', '0')), 
           ef = input_ef, 
           bc_ton = bcm2019 * ef * 1000) %>% 
    select(fips, county_state, id, longitude, latitude, starts_with('basin'), bcm2019, ef,  bc_ton)
  
  
  x.raw.emission.county = ap3.county %>% 
    left_join(x.raw.emission, by = c('ap3_county_fips' = 'fips')) %>% 
    group_by(ap3_county_fips) %>% 
    summarise(bc_ton = sum(bc_ton)) %>% 
    ungroup() %>% 
    mutate(bc_ton = replace_na(bc_ton, 0))
  
  x.raw.emission.county.tx = x.raw.emission.county %>% 
    filter(str_sub(ap3_county_fips, 0, 2) == '48')
  
  x.raw.emission.county.nd = x.raw.emission.county %>% 
    filter(str_sub(ap3_county_fips, 0, 2) == '38')
  
  
  annual_bc_emissions = sum(x.raw.emission.county$bc_ton)/1000
  annual_bc_emissions_tx = sum(x.raw.emission.county.tx$bc_ton)/1000
  annual_bc_emissions_nd = sum(x.raw.emission.county.nd$bc_ton)/1000
  
  ### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  # Modify emission
  # National Flaring
  ap3.original.new = read.csv('../model-ap3/area_sources_2014.csv', header = F)
  ap3.original.new[4] = ap3.original.new[4] + x.raw.emission.county$bc_ton
  write.table(ap3.original.new, '../model-ap3/area_sources_2014_new_ef.csv', sep = ',', 
              row.names = F, col.names=F)
  
  ### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  ### RUN Matlab AP3 (one_step_find_pm25.m) ###
  print("==================================")
  print("Running AP3...")
  run_matlab_script('../model-ap3/one_step_find_pm25.m', verbose = F, )
  ### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  
  
  
  ### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  # Summarise new emission
  ### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  x.compare.pm25 = ap3_pm25_analysis(file = 'new_ef_pm_25_ugm3.csv')
  x.compare.mort = ap3_mort_analysis(file = 'new_ef_pm_25_ugm3.csv') %>% 
    left_join(all.county, by = c('ap3_county_fips' = 'geoid'))
  
  x.compare.mort.tx = x.compare.mort %>% 
    filter(st_abbr == 'TX')
  
  x.compare.mort.max = x.compare.mort %>% 
    filter(mort_diff == max(mort_diff))
  
  print("==================================")
  print(paste0("BC emissions = ", round(annual_bc_emissions,1), ' ktons'))
  print(paste0("BC emissions in TX = ", round(annual_bc_emissions_tx,1), ' ktons'))
  print(paste0("BC emissions in ND = ", round(annual_bc_emissions_nd,1), ' ktons'))
  print(paste0("National deaths = ", round(sum(x.compare.mort$mort_diff),1)))
  print(paste0("Texas deaths = ", round(sum(x.compare.mort.tx$mort_diff),1)))
  print(paste0('Maximum deaths = ', round(x.compare.mort.max$mort_diff, 1), 
               " in ", x.compare.mort.max$name, ", ", x.compare.mort.max$st_abbr))
  
  return(sum(x.compare.mort$mort_diff))
}
