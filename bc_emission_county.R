### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
###   NAME: Prepare county-level gas flaring emissions
### AUTHOR: Chen Chen
### OUTPUT: CSV
# Start ----
### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(janitor)
library(readxl)
library(ggplot2)
library(ggrepel)
library(sf)

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Read county shp ---- 
x.county.geo = st_read('../base/layer/cb_2018_us_county_5m.shp')

# Read Gas flaring point ----
gas.flares = read_xlsx('./source/2019flares_bcm_g black carbon.xlsx', sheet = 'flareswstatesandcounties') %>% 
  clean_names() %>% 
  mutate(fips = paste0(str_pad(statefp, 2, 'left', '0'), str_pad(countyfp, 3, 'left', '0'))) %>% 
  select(fips, county_state, id, longitude, latitude, starts_with('basin'), starts_with('ef'), bcm2019, black_carbon_billion_g_thosand_metric_tonnes)

# Group by county
gas.flares.county = gas.flares %>% 
  group_by(fips) %>% 
  summarise(bc_ton = sum(black_carbon_billion_g_thosand_metric_tonnes)*1000) %>% 
  ungroup()

# Read AP3 county fips ----
ap3.county = readRDS('../base/ap3_county_fips.RDS')

gas.flares.county.ap3 = ap3.county %>% 
  left_join(gas.flares.county, by = c('ap3_county_fips' = 'fips')) %>% 
  mutate(bc_ton = replace_na(bc_ton, 0 ))

# TX Flares only
gas.flares.county.ap3.tx = ap3.county %>% 
  left_join(gas.flares.county, by = c('ap3_county_fips' = 'fips')) %>% 
  mutate(bc_ton = replace_na(bc_ton, 0), 
         bc_ton = ifelse(str_sub(ap3_county_fips, 0, 2) == '48', bc_ton, 0))

sum(gas.flares.county.ap3.tx$bc_ton)

# Create a shapefile ---- 
gas.flares.county.ap3.geo = x.county.geo %>% 
  right_join(gas.flares.county.ap3, by = c('GEOID' = 'ap3_county_fips')) %>% 
  select(PM2_5 = bc_ton) 

st_write(gas.flares.county.ap3.geo, './output/bc_gas_flaring.shp', delete_layer = T)

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Generate AP3 emission files - area source ----
ap3.original = read.csv('../model-ap3/area_sources_2014.csv', header = F)
# !!!Test medium height (sensitivity analysis)
ap3.original.medium = read.csv('../model-ap3/medium_2014.csv', header = F)

## Original AP3 emission (without flaring emissions)
ap3.original.new = read.csv('../model-ap3/area_sources_2014.csv', header = F)
ap3.original.new[4] = ap3.original.new[4] + gas.flares.county.ap3$bc_ton
write.table(ap3.original.new, '../model-ap3/area_sources_2014_new.csv', sep = ',', 
            row.names = F, col.names=F)

summary(ap3.original.new)

# !!!Test medium height (sensitivity analysis)
ap3.original.new.medium = read.csv('../model-ap3/medium_2014.csv', header = F)
ap3.original.new.medium[4] = ap3.original.new.medium[4] + gas.flares.county.ap3$bc_ton
write.table(ap3.original.new.medium, '../model-ap3/medium_2014_new.csv', sep = ',', 
            row.names = F, col.names=F)

# !!!Test low height (sensitivity analysis)
ap3.original.new.low = read.csv('../model-ap3/low_2014.csv', header = F)
ap3.original.new.low[4] = ap3.original.new.low[4] + gas.flares.county.ap3$bc_ton
write.table(ap3.original.new.low, '../model-ap3/low_2014_new.csv', sep = ',', 
            row.names = F, col.names=F)

### ONLY include FLARING in TX
ap3.original.tx = read.csv('../model-ap3/area_sources_2014.csv', header = F)
ap3.original.tx[4] = ap3.original.tx[4] + gas.flares.county.ap3.tx$bc_ton
write.table(ap3.original.tx, '../model-ap3/area_sources_2014_tx.csv', sep = ',', 
            row.names = F, col.names=F)

summary(ap3.original.tx)

## <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
# EOF ----
## <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
