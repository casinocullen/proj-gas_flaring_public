### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
###   NAME: Analysis Mortality Sensibility
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
library(scales)
library(readxl)
library(tidyr)
library(matlabr)
library(googlesheets4)

source('./function_compare_ef.R')

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Alternate emission factors ----
efs = c(0.13, 0.28, 0.51, 0.57, 0.85, 0.9, 1.6, 1.83, 2.5632, 4.2, 6.4)

gas.flares.vol = read_xlsx('/Users/chenchen/Desktop/Research/Gas_Flaring/2019flares_bcm_g black carbon_CC.xlsx', sheet = 'flareswstatesandcounties') %>% 
  clean_names() %>% 
  mutate(fips = paste0(str_pad(statefp, 2, 'left', '0'), str_pad(countyfp, 3, 'left', '0'))) %>% 
  select(fips, county_state, stusps, id, longitude, latitude, starts_with('basin'), hhv, bcm2019)

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
## EASIUR ----
easiur.md = read.csv('easiur_EASIURIMPORT.csv') %>% 
  clean_names() %>% 
  select(starts_with('pm')) %>% 
  select(ends_with('ground'))

easiur.deaths = NULL
for (ef in efs) {
  gas.flares.easiur = gas.flares.vol %>% 
    cbind(easiur.md) %>% 
    mutate(bc_damage_easiur = pm25_annual_ground*ef*bcm2019*1000, 
           mort_easiur = bc_damage_easiur/8.8E6) %>% 
    filter(!is.na(pm25_annual_ground))
  
  total_sum = sum(gas.flares.easiur$mort_easiur)
  print(paste0('When EF = ', ef, ', mortality = ', total_sum))
  easiur.deaths = append(easiur.deaths, total_sum)
}

## AP3 ----
ap3.deaths = NULL
for (ef in efs) {
  total_sum = ap3_compare_ef(ef)
  ap3.deaths = append(ap3.deaths, total_sum)
  # print(paste0('When EF = ', ef, ', mortality = ', total_sum))
}

# InMAP ----
# Create Shapefiles for sensitivity from other EFs
for (ef in efs) {
  ef_str = sub("\\.", "", ef)

  gas.flares.shp = gas.flares.vol %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4269) %>%
    mutate(PM2_5 = bcm2019 * ef * 1000,
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
  st_write(gas.flares.shp, paste0('./output/bc_gas_flaring_', ef_str, '.shp'), delete_layer = T)
}

# After running InMAP...
inmap.deaths = NULL
for (ef in efs) {
  ef_str = sub("\\.", "", ef)
  x.inmap.one = st_read(paste0('../inmap/gas_flaring_output/inmap_bc_gas_flaring_', ef_str, '.shp')) %>% 
    filter(Mort_InMAP<100)
  
  total_sum = sum(x.inmap.one$Mort_InMAP)
  inmap.deaths = append(inmap.deaths, total_sum)
  # print(paste0('When EF = ', ef, ', mortality = ', total_sum))
}


### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
# Combine results ----
x.total = data.frame(ef = efs, EASIUR = easiur.deaths, AP3 = ap3.deaths, InMAP = inmap.deaths)

# Analysis
lm_ea = lm(data = x.total, formula = EASIUR ~ AP3)
lm_ei = lm(data = x.total, formula = EASIUR ~ InMAP)

summary(lm_ea)
summary(lm_ei)

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
# Make plot----
easiur_death = 48.21661
ap3_death = 52.77926
inmap_death = 25.78014

line_easiur = lm(EASIUR ~ `EF (g/m3)`, data = x.total)
line_ap3 = lm(AP3 ~ `EF (g/m3)`, data = x.total)
line_inmap = lm(InMAP ~ `EF (g/m3)`, data = x.total)

bottcher_ef_easiur = (easiur_death - coefficients(line_easiur)[1])/coefficients(line_easiur)[2]
bottcher_ef_ap3 = (ap3_death - coefficients(line_ap3)[1])/coefficients(line_ap3)[2]
bottcher_ef_inmap = (inmap_death - coefficients(line_inmap)[1])/coefficients(line_inmap)[2]

bottcher_df = data.frame('ef' = c(bottcher_ef_easiur, bottcher_ef_ap3, bottcher_ef_inmap), 
                         'death' = c(easiur_death,ap3_death, inmap_death), 
                         'key' = c("EASIUR", "AP3", "InMAP"))

ggplot(data = x.total.plot, aes(x = ef, 
                                y = value, 
                                color = key, 
                                fill = key, 
                                shape = key, 
                                alpha = key)) + 
  geom_point(position=position_jitter(h=0.01, w=0.01), 
             size = 1, show.legend = F) + 
  geom_smooth(show.legend = T) + 
  geom_segment(aes(y = 52.77926, yend = 52.77926, x = 0.194, xend = 4.781691), 
               color = '#A3C4BC', size = 1, alpha = 0.1,linetype=2,
               show.legend = T) + 
  geom_segment(aes(y = 48.21661, yend = 48.21661, x = 0.194, xend = 4.781691), 
               color = '#413C58', size = 1, alpha = 0.1,linetype=2,
               show.legend = T) + 
  geom_segment(aes(y = 25.78014, yend = 25.78014, x = 0.194, xend = 4.781691), 
               color = '#C97C5D', size = 1, alpha = 0.1, linetype=2,
               show.legend = T) + 
  geom_segment(x = 0.194, xend = 0.194, 
               y = 42.77926, yend = 62.77926,
               colour = "#A3C4BC", size = 1, show.legend = F) + 
  geom_segment(x = 4.781691, xend = 4.781691, 
               y = 42.77926, yend = 62.77926,
               colour = "#A3C4BC", size = 1, show.legend = F) + 
  geom_segment(x = 4.781691, xend = 4.781691, 
               y = 38.21661, yend = 58.21661,
               colour = "#413C58", size = 1, show.legend = F) + 
  geom_segment(x = 0.194, xend = 0.194, 
               y = 38.21661, yend = 58.21661,
               colour = "#413C58", size = 1, show.legend = F) + 
  geom_segment(x = 0.194, xend = 0.194, 
               y = 15.78014, yend = 35.78014,
               colour = "#C97C5D", size = 1, show.legend = F) + 
  geom_segment(x = 4.781691, xend = 4.781691, 
               y = 15.78014, yend = 35.78014,
               colour = "#C97C5D", size = 1, show.legend = F) + 
  ylab("Predicted mortality (deaths)") +
  xlab(expression(bold(paste("Emission factor (g/", m^3, ")", sep="")))) + 
  scale_x_continuous(breaks = seq(0, 7, by = 0.5)) + 
  scale_y_continuous(breaks = seq(0, 400, by = 50)) + 
  scale_color_manual(values = c('#A3C4BC','#413C58','#C97C5D')) +
  scale_fill_manual(values = c( '#A3C4BC','#413C58','#C97C5D')) + 
  scale_shape_manual(values= c(23, 24, 22)) + 
  scale_alpha_manual(values= c(0.4, 0.8, 0.8)) + 
  # guides(fill=guide_legend(title='Mortality based on \nBöttcher\'s EF range'),
  #        shape=guide_legend(title='Mortality based on \nBöttcher\'s EF range'),
  #        color=guide_legend(title='Mortality based on \nBöttcher\'s EF range'),
  #        alpha=guide_legend(title='Mortality based on \nBöttcher\'s EF range')) + 
  # geom_point(data = bottcher_df, aes(x = ef, y = death, 
  #                                    colour = key, 
  #                                    fill = key, 
  #                                    shape = key), 
  #            size = 4, 
  #            alpha = 0.8) + 
  theme_bw() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom', 
        legend.text = element_text(size = 13, face = 'bold'),
        plot.background = element_rect(fill = 'white', color = 'white'),
        axis.title.x = element_text(size= 13, 
                                    face = 'bold'),
        axis.title.y = element_text(size= 13, 
                                    face = 'bold'),
        axis.text.x = element_text(size= 11, 
                                   face = 'bold'),
        axis.text.y = element_text(size= 11, 
                                   face = 'bold')
  )

# Write
path = paste0("./vis/paper_viz_p8.png")
ggsave(path, width = 7, height = 6, dpi = 500, units = "in")

## <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
# EOF ----
## <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----