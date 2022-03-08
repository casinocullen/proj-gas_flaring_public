### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
###   NAME: BENMAP Mortality
### AUTHOR: Chen Chen
### OUTPUT: CSV
# Start ----
### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
library(dplyr)
library(stringr)
library(data.table)
library(janitor)
library(ggplot2)
library(sf)
library(scales)
library(googlesheets4)
`%notin%` <- Negate(`%in%`)

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Prepare data for running BenMAP-CE ----
x.pm.ap3 = st_read('./output/pm25_increase.gpkg') %>% 
  st_drop_geometry() %>% 
  mutate(Column = as.numeric(str_sub(geoid, 0, 2)), 
         Row = as.numeric(str_sub(geoid, 3, 5)), 
         Metric = 'D24HourMean',
         `Seasonal Metric` = 'QuarterlyMean', 
         `Annual Metric` = 'Mean')

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
## AP3 data ----
# Load AP3 air pollution surfaces
x.pm.ap3.ori = x.pm.ap3 %>% 
  mutate(Values = pm_25_conc_ori) %>% 
  select(Column, Row, Metric, `Seasonal Metric`, `Annual Metric`, Values)

x.pm.ap3.new = x.pm.ap3 %>% 
  mutate(Values = pm_25_conc_new) %>% 
  select(Column, Row, Metric, `Seasonal Metric`, `Annual Metric`, Values)
# Write
write.csv(x.pm.ap3.ori, './output/benmap_air_surface_baseline.csv', row.names = F)
write.csv(x.pm.ap3.new, './output/benmap_air_surface_add_gas_flaring.csv', row.names = F)

## InMAP data ----
x.inmap.raw = st_read('../inmap/nei2005/2005nei_output_gas_flaring_12km.shp') %>% 
  mutate(inmap_id = as.character(row_number())) %>% 
  filter(Mort_InMAP<100)

sum(x.inmap.raw$Mort_InMAP)
sum(x.inmap.raw$Mort_Kr)

# make grid and number cells
x.grid <- st_make_grid(x.inmap.raw, square = T, cellsize = c(12000, 12000)) %>% 
  st_sf %>% 
  mutate(grid_id = 1:nrow(.))

st_crs(x.grid.index)

# make bounding box
reqGridBbox <- st_bbox(x.grid)

# calculate number of rows/columns
nCols <- (reqGridBbox[3]-reqGridBbox[1])/12000
nRows <- (reqGridBbox[4]-reqGridBbox[2])/12000

# label by row / column number and combine labels
x.grid.index <- x.grid %>% 
  st_transform(crs = 4326) %>%
  mutate(X = rep(1:nCols, nRows),
         Y = unlist(lapply(1:nRows, rep, nCols))) %>% 
  st_join(x.inmap.raw %>% st_transform(crs = 4326) ) %>% 
  filter(grid_id %in% unlist(st_intersects(x.inmap.raw %>% st_transform(crs = 4326), .))) %>% # filter by intersection 
  st_join(x.state %>% st_transform(crs = 4326))

x.grid.index.state.xwalk = x.grid.index %>% 
  st_drop_geometry() %>% 
  select(Column = X, Row = Y, st_abbr) %>% 
  filter(!is.na(st_abbr)) %>% 
  unique()

ggplot(x.inmap.raw) +
  geom_sf(size=0.1) +
  geom_sf(data = x.grid.index, alpha = 0, size = 0.1, color = 'red')

x.inmap.12km = x.grid.index %>%  
  st_drop_geometry() %>% 
  group_by(grid_id, X, Y) %>% 
  summarise(BasePM25 = mean(BasePM25), 
            AfterPM25 = mean(BasePM25 + TotalPM25)) %>% 
  ungroup()

x.inmap.12km.base = x.inmap.12km %>% 
  mutate(`Row` = Y,
         `Column` = X,
         `Metric`	= 'D24HourMean',
         `Seasonal Metric` = 'QuarterlyMean', 
         `Annual Metric` = 'Mean', 
         `Values` = AfterPM25) %>% 
  select(`Row`, `Column`, `Metric`, `Seasonal Metric`, `Annual Metric`, `Values`)

x.inmap.12km.control = x.inmap.12km %>% 
  mutate(`Row` = Y,
         `Column` = X,
         `Metric`	= 'D24HourMean',
         `Seasonal Metric` = 'QuarterlyMean', 
         `Annual Metric` = 'Mean', 
         `Values` = BasePM25) %>% 
  select(`Row`, `Column`, `Metric`, `Seasonal Metric`, `Annual Metric`, `Values`)

# Write
write.csv(x.inmap.12km.base, './benmap/input/inmap_air_surface_base.csv', row.names = F)
write.csv(x.inmap.12km.control, './benmap/input/inmap_air_surface_control.csv', row.names = F)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
## Analysis output from BenMAP ----
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
## AP3-BenMAP ----
x.benmap.raw = fread('./benmap/output/ap3_morbidity_county.CSV')
names(x.benmap.raw)

x.benmap.raw.grp = x.benmap.raw %>% 
  group_by(Endpoint, Author, Year) %>% 
  summarise(total_pop = sum(Population), 
            total_cases = sum(`Point Estimate`),
            total_cases_2.5 = sum(`Percentile 2.5`),
            total_cases_97.5 = sum(`Percentile 97.5`),
            num_county = n(),
            num_age_groups = n_distinct(`Start Age`)) %>% 
  ungroup()

sheet_write(x.benmap.raw.grp, ss = 'https://docs.google.com/spreadsheets/d/1xrooYcbcPW_JRDDAo3hGwacuibPyIGcuegLxT-IjbWM/edit#gid=0', sheet = 'AP3 Morbidity')

## InMAP-BenMAP ----
x.benmap.inmap.raw = fread('./benmap/output/inmap_morbidity_12km.CSV')
x.benmap.inmap.grp = x.benmap.inmap.raw %>% 
  group_by(Endpoint, Author, Year) %>% 
  summarise(total_pop = sum(Population), 
            total_cases = round(sum(`Point Estimate`), 2),
            total_cases_2.5 = round(sum(`Percentile 2.5`), 2),
            total_cases_97.5 = round(sum(`Percentile 97.5`), 2),
            num_county = n(),
            num_age_groups = n_distinct(`Start Age`)) %>% 
  ungroup()


sheet_write(x.benmap.inmap.grp, ss = 'https://docs.google.com/spreadsheets/d/1xrooYcbcPW_JRDDAo3hGwacuibPyIGcuegLxT-IjbWM/edit#gid=0', sheet = 'InMAP Morbidity')

## Check state results ----
x.state.xwalk = x.state %>% 
  st_drop_geometry() %>% 
  select(geoid_st, st_abbr)

x.benmap.ap3.grp.state = x.benmap.raw %>% 
  mutate(geoid_st = str_pad(Column, width = 2, side = 'left', pad = '0')) %>% 
  filter(Endpoint %in% c("ER visits  respiratory", "Work Loss Days", "Incidence  Asthma"), 
         Author %notin% c('McConnell et al.', 'Nishimura et al.')) %>% 
  left_join(x.state.xwalk) %>% 
  group_by(st_abbr, Endpoint, Author, Year) %>% 
  summarise(total_pop = sum(Population), 
            total_cases = sum(`Point Estimate`),
            total_cases_2.5 = round(sum(`Percentile 2.5`), 2),
            total_cases_97.5 = round(sum(`Percentile 97.5`), 2),
            num_county = n(),
            num_age_groups = n_distinct(`Start Age`)) %>% 
  ungroup() %>% 
  mutate(model = 'AP3', 
         total_cases = case_when(Endpoint == 'ER visits  respiratory' ~ total_cases/4, 
                                 Endpoint == 'ER visits  respiratory' ~ total_cases/4, 
                                 TRUE ~ total_cases), 
         total_cases_2.5 = ifelse(Endpoint == 'ER visits  respiratory', total_cases_2.5/4, total_cases_2.5), 
         total_cases = ifelse(Endpoint == 'ER visits  respiratory', total_cases/4, total_cases), 
         
         ) %>% 
  select(st_abbr, Endpoint, model, total_cases)

x.benmap.inmap.grp.state = x.benmap.inmap.raw %>% 
  left_join(x.grid.index.state.xwalk) %>% 
  group_by(st_abbr, Endpoint, Author, Year) %>% 
  summarise(total_pop = sum(Population), 
            total_cases = sum(`Point Estimate`),
            total_cases_2.5 = sum(`Percentile 2.5`),
            total_cases_97.5 = sum(`Percentile 97.5`),
            num_county = n(),
            num_age_groups = n_distinct(`Start Age`)) %>% 
  ungroup() %>% 
  filter(Endpoint %in% c("ER visits  respiratory", "Work Loss Days", "Incidence  Asthma"), 
         Author %notin% c('McConnell et al.', 'Nishimura et al.')) %>% 
  mutate(model = 'InMAP')


x.benmap.grp.state = x.benmap.inmap.grp.state %>% 
  rbind(x.benmap.ap3.grp.state) %>% 
  filter(!is.na(st_abbr))

## Check US results ----
## AP3
x.ap3.state.raw = read.csv('./benmap/output/ap3_state_agg.CSV') 
x.ap3.state = x.ap3.state.raw %>% 
  mutate(geoid_st = str_pad(Column, width = 2, side = 'left', pad = '0')) %>% 
  left_join(x.state.xwalk) %>% 
  group_by(st_abbr, Endpoint, Author) %>% 
  summarise(total_pop = sum(Population), 
            total_cases = sum(`Point.Estimate`),
            total_cases_2.5 = sum(`Percentile.2.5`),
            total_cases_97.5 = sum(`Percentile.97.5`),
            num_studies = n(), 
            num_age_groups = n_distinct(`Start.Age`)) %>% 
  ungroup() %>% 
  mutate(total_cases = total_cases/num_studies*num_age_groups, 
         total_cases_2.5 = total_cases_2.5/num_studies*num_age_groups, 
         total_cases_97.5 = total_cases_97.5/num_studies*num_age_groups, 
         model = 'AP3') %>% 
  select(st_abbr, Endpoint, model, total_cases, total_cases_2.5, total_cases_97.5) 


x.ap3.us = x.ap3.state %>% 
  group_by(Endpoint, model) %>% 
  summarise(total_cases = sum(total_cases),
            total_cases_2.5 = sum(total_cases_2.5),
            total_cases_97.5 = sum(total_cases_97.5)) %>% 
  ungroup()

## InMAP
x.inmap.state.raw = read.csv('./benmap/output/inmap_state_agg.CSV') 
x.inmap.state = x.inmap.state.raw %>% 
  mutate(geoid_st = str_pad(Col, width = 2, side = 'left', pad = '0')) %>% 
  left_join(x.state.xwalk) %>% 
  group_by(st_abbr, Endpoint, Author) %>% 
  summarise(total_pop = sum(Population), 
            total_cases = sum(`Point.Estimate`),
            total_cases_2.5 = sum(`Percentile.2.5`),
            total_cases_97.5 = sum(`Percentile.97.5`),
            num_studies = n(), 
            num_age_groups = n_distinct(`Start.Age`)) %>% 
  ungroup() %>% 
  mutate(total_cases = total_cases/num_studies*num_age_groups, 
         total_cases_2.5 = total_cases_2.5/num_studies*num_age_groups, 
         total_cases_97.5 = total_cases_97.5/num_studies*num_age_groups, 
         model = 'InMAP') %>% 
  select(st_abbr, Endpoint, model, total_cases, total_cases_2.5, total_cases_97.5)


x.inmap.us = x.inmap.state %>% 
  group_by(Endpoint, model) %>% 
  summarise(total_cases = sum(total_cases),
            total_cases_2.5 = sum(total_cases_2.5),
            total_cases_97.5 = sum(total_cases_97.5)) %>% 
  ungroup()

sheet_write(x.inmap.us, ss = 'https://docs.google.com/spreadsheets/d/1xrooYcbcPW_JRDDAo3hGwacuibPyIGcuegLxT-IjbWM/edit#gid=0', sheet = 'New InMAP')

# Bind AP3 and InMAP
x.benmap.grp.state = x.ap3.state %>% 
  rbind(x.inmap.state)

## <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
# EOF ----
## <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
