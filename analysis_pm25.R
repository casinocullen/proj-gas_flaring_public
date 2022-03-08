### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ---- 
###   NAME: AP3 PM2.5
### AUTHOR: Chen Chen
### OUTPUT: Maps
# Start ----
### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ---- 
library(dplyr)
library(stringr)
library(data.table)
library(janitor)
library(ggplot2)
library(ggrepel)
library(sf)

source('../base/write_carto.R')

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Prepare data ----
x.county.geo = st_read('../base/layer/us_county_geo.gpkg')
x.ap3.county = readRDS('../base/ap3_county_fips.RDS')

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Read AP3 results ----

# No flaring
x.pm.original = fread('../model-ap3/ori_pm_25_ugm3.csv', col.names = 'pm_25_conc')
x.pm.original.county = x.pm.original %>% 
  cbind(x.ap3.county)

# Add flaring BC
x.pm.new = fread('../model-ap3/new_pm_25_ugm3.csv', col.names = 'pm_25_conc')
x.pm.new.county = x.pm.new %>% 
  cbind(x.ap3.county)

# Compare
x.pm.compare = x.pm.new.county %>% 
  full_join(x.pm.original.county, by = 'ap3_county_fips', suffix = c('_new', '_ori')) %>% 
  select(ap3_county_fips, pm_25_conc_ori, pm_25_conc_new) 


### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Read AP3 results (Texas flarings only) ----
x.pm.tx = fread('../model-ap3/tx_pm_25_ugm3.csv', col.names = 'pm_25_conc_tx')
x.pm.tx.county = x.pm.tx %>%
  cbind(x.ap3.county)

x.pm.compare = x.pm.compare %>% 
  full_join(x.pm.tx.county, by = 'ap3_county_fips') %>% 
  select(ap3_county_fips, pm_25_conc_ori, pm_25_conc_new, pm_25_conc_tx)

x.pm.compare.geo = x.county.geo %>% 
  right_join(x.pm.compare, by = c('geoid'='ap3_county_fips'))

# Make a shapefile for mapping ----
x.pm.compare.geo = x.pm.compare.geo %>% 
  select(names(x.county.geo), starts_with("pm_25")) %>% 
  mutate(diff_ori_new = pm_25_conc_new - pm_25_conc_ori, 
         diff_ori_tx = pm_25_conc_tx - pm_25_conc_ori
         )

st_write(x.pm.compare.geo, './output/pm25_increase.gpkg', delete_layer = T)

# Plot data
ggplot(data = x.pm.original.county.geo) + 
  geom_sf(aes(fill = 'pm_25_conc'))

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
# Result: Summary ----
### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
x.pm.compare.ap3 = x.pm.compare.geo %>% 
  st_drop_geometry() %>% 
  mutate(pm_inc = pm_25_conc_new - pm_25_conc_ori, 
         pm_pct_chg = (pm_25_conc_new - pm_25_conc_ori)/pm_25_conc_new) %>% 
  arrange(-pm_inc) %>% 
  top_n(4)

summary(x.pm.compare.ap3)

## <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
# EOF ----
## <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----