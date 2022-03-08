## <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
### NAME: Prepare gas flaring emissions
### AUTHOR: Chen Chen
### OUTPUT: CSV
# Start ----
## <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(janitor)
library(readxl)
library(ggplot2)
library(ggrepel)
library(sf)
library(tidyverse)
library(ggsflabel)
library(scales)
`%notin%` <- Negate(`%in%`)

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Data analysis ----
## Gas flaring point ----
gas.flares = read_xlsx('./source/2019flares_bcm_g black carbon.xlsx', sheet = 'flareswstatesandcounties') %>% 
  clean_names() %>% 
  mutate(fips = paste0(str_pad(statefp, 2, 'left', '0'), str_pad(countyfp, 3, 'left', '0'))) %>% 
  select(fips, county_state, stusps, id, longitude, latitude, starts_with('basin'), starts_with('ef'), hhv, bcm2019, black_carbon_billion_g_thosand_metric_tonnes, bc_tons)

## Check high-level stats ----
sum(gas.flares$black_carbon_billion_g_thosand_metric_tonnes)
mean(gas.flares$black_carbon_billion_g_thosand_metric_tonnes)

## Group flares by state ----
gas.flares.st = gas.flares %>% 
  group_by(stusps) %>% 
  summarise(count = n(), 
            tons = sum(bc_tons), 
            volume = sum(bcm2019)) %>%
  ungroup()

## Group flares by basin ----
gas.flares.basin = gas.flares %>% 
  group_by(basin_name) %>% 
  summarise(count = n(), 
            tons = sum(bc_tons), 
            volume = sum(bcm2019)) %>%
  ungroup()

## Create shp file (point) for mapping ----
gas.flares.geo = gas.flares %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4269) %>% 
  mutate(PM2_5 = black_carbon_billion_g_thosand_metric_tonnes * 1000, 
         SOx = 0, 
         VOC = 0, 
         NOx = 0, 
         NH3 = 0, 
         height = 0, 
         diam = 0, 
         temp = 0, 
         velocity = 0
  ) %>% 
  select(PM2_5, SOx, VOC, NOx, NH3, height, diam, temp, velocity)

st_write(gas.flares.geo, './output/bc_gas_flaring.shp', delete_layer = T)

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Generate AP3 emission files - area source ----
ap3.original = read.csv('../model-ap3/area_sources_2014.csv', header = F)

summary(ap3.original)

## Original AP3 emission (without flaring emissions)
ap3.original = read.csv('../model-ap3/area_sources_2014.csv', header = F)[,c(1,2,4,5,6,7)]
colnames(ap3.original) <- c('NH3', 'NOx', 'PM2_5', 'SOx', 'VOC1', 'VOC2')
ap3.original = ap3.original %>% 
  mutate(VOC = VOC1 + VOC2) %>% 
  select(-VOC1, -VOC2) %>% 
  cbind(ap3.county)

ap3.original.geo = x.county.geo %>% 
  right_join(ap3.original, by = c('GEOID'='ap3_county_fips')) %>% 
  select('NH3', 'NOx', 'PM2_5', 'SOx', 'VOC')

st_write(ap3.original.geo, './output/area_source_original.shp', delete_layer = T)


## National Flaring
ap3.original.new = read.csv('../model-ap3/area_sources_2014.csv', header = F)
ap3.original.new[4] = ap3.original.new[4] + gas.flares.county.ap3$bc_ton
write.table(ap3.original.new, '../model-ap3/area_sources_2014_new.csv', sep = ',', 
            row.names = F, col.names=F)

summary(ap3.original.new)

### ONLY include FLARING in TX
ap3.original.tx = read.csv('../model-ap3/area_sources_2014.csv', header = F)
ap3.original.tx[4] = ap3.original.tx[4] + gas.flares.county.ap3.tx$bc_ton
write.table(ap3.original.tx, '../model-ap3/area_sources_2014_tx.csv', sep = ',', 
            row.names = F, col.names=F)

summary(ap3.original.tx)

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Make Plots ----
## P1: EF vs HHV [scatter] ----
ggplot(data = gas.flares, aes(x = hhv, y = ef_g_m3)) + 
  geom_point(color = '#3E5C76', fill = 'red', alpha = 0.4, size = 2) + 
  theme_bw() + 
  xlab('HHV (MJ/m3)') + 
  ylab('EF (g/m3)')

# Write
path = paste0("./vis/ppt_viz_p1.png")
ggsave(path, width = 5, height = 4, dpi = 120, units = "in")

## P2: BC emissions vs flared BCM [scatter] ----
ggplot(data = gas.flares, aes(x = bcm2019, y = bc_tons)) + 
  geom_point(aes(color = hhv), shape = 20, alpha = 0.8, size = 3) + 
  scale_color_gradient(low = "#A6D49F", high = "#C73E1D") + 
  theme_bw() + 
  theme(legend.position="bottom") + 
  xlab('BCM') + 
  ylab('BC (tons)') + 
  labs(color = 'HHV (MJ/m3)')

# Write
path = paste0("./vis/ppt_viz_p2.png")
ggsave(path, width = 5, height = 4, dpi = 120, units = "in")

## P3: BC/BCM grouped by state [bar] ----
ggplot(data = gas.flares.grp, aes(x = stusps, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', 
           position = "dodge") + 
  # geom_bar(aes(x = stusps, y = `BC emissions`/1000), 
  #          position = "dodge", 
  #          stat = 'identity', 
  #          fill = '#B0413E') + 
  scale_y_continuous("Gas Volume (bcm)", sec.axis = sec_axis(~. * 1000, name = "BC emissions (tons)")) + 
  scale_fill_manual(values = c('#FCAA67', '#B0413E')) +
  theme_minimal() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom', 
        legend.text = element_text(size = 13, face = 'bold'),
        plot.background = element_rect(fill = 'white', color = 'white'),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size= 13, 
                                    face = 'bold'),
        axis.text.x = element_text(size= 11, 
                                   face = 'bold'),
        axis.text.y = element_text(size= 11, 
                                   face = 'bold')
  )

# Write
path = paste0("./vis/fig2.png")
ggsave(path, width = 8, height = 6, dpi = 200, units = "in")


## P3: BC/BCM grouped by basin [map + pie] ----
sf::sf_use_s2(FALSE)
basin.geo = st_read('./source/ShalePlays_US_EIA_Sep2019.shp', crs = 4269) %>%
  group_by(Basin) %>% 
  summarise() %>%
  ungroup()

basin.gas.volume = basin.geo %>% 
  st_join(gas.flares %>% 
            st_as_sf(coords = c("longitude", "latitude"), crs = 4269)) %>% 
  st_drop_geometry() %>% 
  group_by(Basin) %>% 
  summarise(gas_volume = sum(bcm2019)) %>% 
  ungroup()

basin.gas.volume.geo = basin.geo %>% 
  inner_join(basin.gas.volume) %>% 
  mutate(gas_volume = ifelse(is.na(gas_volume), 0, gas_volume), 
         basin_label = ifelse(gas_volume>1, Basin, NA))

gas.flares.grp.basin = gas.flares %>% 
  group_by(basin_name) %>% 
  summarise(`Gas volume` = sum(bcm2019),
            `BC emissions` = sum(black_carbon_billion_g_thosand_metric_tonnes),
            # ef = mean(ef_g_m3),
            # hhv = mean(hhv)
  ) %>% 
  ungroup() %>% 
  separate(basin_name, sep = ' - ', into = c('basin_num', 'basin_name')) %>% 
  arrange(-`Gas volume`) %>%
  mutate(basin_name = ifelse(basin_name == 'Gulf Coast Basin (LA, TX)', "Western Gulf", basin_name), 
         basin_name = factor(basin_name, level = unique(basin_name))) %>% 
  mutate(prop = `Gas volume` / sum(`Gas volume`) *100, 
         pos = cumsum(prop)- 0.5*prop, 
         label = ifelse(`Gas volume` > 1, paste0(basin_name, '\n', round(prop), "%"), NA))

# MAP
x.state = st_read('../base/layer/us_state_geo.gpkg') %>% 
  filter(st_abbr %notin% c('AK', 'HI')) %>% 
  filter(st_abbr %in% c(state.abb, 'DC'))
basin_map = ggplot(data = basin.gas.volume.geo) + 
  geom_sf(data = x.state, size = 0.3, fill = NA, color = 'grey30') + 
  geom_sf(aes(fill = gas_volume), size = 0.5) + 
  scale_fill_viridis_c(alpha = 0.9, 
                       labels = c(expression(bold(0-2)),  
                                  expression(2-4), 
                                  expression(4-6), 
                                  expression("> 6")),
                       guide = guide_legend(direction = "horizontal", 
                                            title.position = "top"
                       )) + 
  geom_sf_label_repel(aes(label = basin_label),
                      force = 100, nudge_x = -2, seed = 10) + 
  theme_void() + 
  labs(fill = "Gas Volume (bcm)") + 
  theme(legend.position = 'bottom', 
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 10), 
        legend.box="horizontal", 
        legend.key.size = unit(80, "pt"),
        legend.key.width = unit(5, 'pt'), 
        legend.key.height = unit(20, 'pt'), 
        plot.background = element_rect(fill = 'white', color = 'white')
  ) 

# PIE CHART
basin_pie = ggplot(data = gas.flares.grp.basin, aes(x = "", y = prop, fill = basin_name)) +
  geom_bar(stat="identity", size=0.2, color="white") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("#00798c","#d1495b","#edae49","#66a182","#2e4057", "#8d96a3", "#8d96a3", "#8d96a3", "#8d96a3", "#8d96a3", "#8d96a3", "#8d96a3", "#8d96a3", "#8d96a3", "#8d96a3", "#8d96a3", "#8d96a3", "#8d96a3", "#8d96a3", "#8d96a3", "#8d96a3", "#8d96a3", "#8d96a3", "#8d96a3", "#8d96a3", "#8d96a3", "#8d96a3", "#8d96a3")) + 
  geom_label_repel(aes(y = 100-pos, label = label), 
                   size=3, force_pull = 3, 
                   fontface = 'bold') + 
  theme_void() + 
  theme(legend.position = 'none')


# COMBINE MAP + PIE
ggplot() +
  coord_equal(xlim = c(0, 5), ylim = c(0, 2), expand = FALSE) + 
  annotation_custom(ggplotGrob(basin_map), xmin = 0, xmax = 3, ymin = 0, 
                    ymax = 2) + 
  annotation_custom(ggplotGrob(basin_pie), xmin = 3, xmax = 5, ymin = 0, 
                    ymax = 2) + 
  theme_void() + 
  theme(plot.background = element_rect(fill = 'white', color = 'white'))


# Write
path = paste0("./vis/fig_a1.png")
ggsave(path, width = 6, height = 4, dpi = 200, units = "in")

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
### New Plot (From David) ----
### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
## P4: EF histogram weighted by BCM [bar + scatter] ----
p1 = ggplot(data = gas.flares, aes(x = hhv, y = ef_g_m3)) + 
  geom_point(color = '#3E5C76', fill = 'red', alpha = 0.4, size = 2) + 
  scale_y_continuous(expand = c(0,0.1)) + 
  theme_bw() + 
  xlab(expression(bold(paste("HHV (MJ/", m^3, ")", sep="")))) + 
  ylab(expression(bold(paste("EF (g/", m^3, ")", sep="")))) + 
  theme(legend.title = element_blank(), 
        legend.text = element_text(size = 8, face = 'bold'), 
        legend.key.size = unit(10, "pt"),
        panel.grid.major.y = element_line(colour = "grey"), 
        plot.background = element_rect(fill = 'white', color = 'white'),
        plot.title = element_text(size= 12, 
                                  face = 'bold',
                                  hjust = 0.5),
        axis.title.x = element_text(size= 10, 
                                    face = 'bold'),
        axis.title.y = element_text(size= 10, 
                                    face = 'bold'),
        axis.text.x = element_text(size= 10, 
                                   face = 'bold'),
        axis.text.y = element_text(size= 10, 
                                   face = 'bold')
  )

p2 = ggplot(data = gas.flares) + 
  geom_histogram(aes(x = ef_g_m3, weight = bcm2019), bins = 20,
                 fill = '#77966D',
                 color = 'white', 
                 alpha=0.8, size=0.1) + 
  scale_y_continuous(breaks = pretty_breaks(5), expand = c(0,0.1)) + 
  xlab(expression(bold(paste("EF (g/", m^3, ")", sep="")))) + 
  ylab("Flared gas volume (bcm)") + 
  # labs(title = 'Weighted EF Frequency by Volume') + 
  theme_classic() + 
  theme(legend.title = element_blank(), 
        legend.text = element_text(size = 8, face = 'bold'), 
        legend.key.size = unit(10, "pt"),
        panel.grid.major.y = element_line(colour = "grey"), 
        plot.background = element_rect(fill = 'white', color = 'white'),
        plot.title = element_text(size= 12, 
                                  face = 'bold',
                                  hjust = 0.5),
        axis.title.x = element_text(size= 10, 
                                    face = 'bold'),
        axis.title.y = element_text(size= 10, 
                                    face = 'bold'),
        axis.text.x = element_text(size= 10, 
                                   face = 'bold'),
        axis.text.y = element_text(size= 10, 
                                   face = 'bold')
  )

# Combine
ggplot() + 
  coord_equal(xlim = c(0, 6), ylim = c(0, 3), expand = F) + 
  annotation_custom(ggplotGrob(p1), xmin = 0, xmax = 3, ymin = 0, 
                    ymax = 3) + 
  annotation_custom(ggplotGrob(p2), xmin = 3, xmax = 6, ymin = 0, 
                    ymax = 3)

# Write
path = paste0("./vis/fig3.png")
ggsave(path, width = 8, height = 4, dpi = 500, units = "in")

## <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
# EOF ----
## <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
