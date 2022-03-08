### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
###   NAME: Analysis AP3 Mortality
### AUTHOR: Chen Chen
### OUTPUT: Maps
# Start ----
### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(janitor)
library(ggplot2)
library(ggrepel)
library(sf)
library(scales)
library(readxl)
library(grid)
`%notin%` <- Negate(`%in%`)
source('../base/write_carto.R')

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Prepare data ----
x.ap3.county = readRDS('../base/ap3_county_fips.RDS')
x.county.geo = st_read('../base/layer/us_county_geo.gpkg')

WTP_Mort = 9186210
DoseResponseAdult= 0.005826891; # Krewski
DoseResponseInfant=0.006765865; # Woodruff 2006

# Load AP3 result ----
x.pm.ori = fread('../model-ap3/ori_pm_25_ugm3.csv', col.names = 'pm_25_conc') %>% 
  pull()
x.pm.new = fread('../model-ap3/new_pm_25_ugm3.csv', col.names = 'pm_25_conc') %>% 
  pull()
x.pm.tx = fread('../model-ap3/tx_pm_25_ugm3.csv', col.names = 'pm_25_conc') %>% 
  pull()

# Load Population ----
x.pop = read.table('../model-ap3/pop_2014.csv', sep = ',') %>% 
  data.matrix()

## Infant
x.pop.infant = x.pop
x.pop.infant[,2:19] = 0

## Adult
x.pop.30 = x.pop
x.pop.30[,1:7] = 0

# Load AP3 mortality ----
x.mort = read.table('../model-ap3/mort_2014.csv', sep = ',') %>% 
  data.matrix()

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
# Deaths w/o flaring ----
# Adult Deaths from baseline
x.death.ori = x.mort * (1-(1/exp(DoseResponseAdult * x.pm.ori))) * x.pop.30
sum(x.death.ori)
x.death.ori.county = rowSums(x.death.ori)

# Infant deaths from baseline
x.infant.death.ori = x.mort * (1-(1/exp(DoseResponseInfant * x.pm.ori))) * x.pop.infant
sum(x.infant.death.ori)
x.infant.death.ori.county = rowSums(x.infant.death.ori)

x.death.total.ori = rowSums(x.death.ori) + rowSums(x.infant.death.ori)
# saveRDS(x.death.total.ori, './output/x.death.total.ori.RDS')

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Deaths with flaring ----
# Adult Deaths from new
x.death.new = x.mort * (1-(1/exp(DoseResponseAdult * x.pm.new))) * x.pop.30
sum(x.death.new)
x.death.new.county = rowSums(x.death.new)

# Infant deaths from new
x.infant.death.new = x.mort * (1-(1/exp(DoseResponseInfant * x.pm.new))) * x.pop.infant
sum(x.infant.death.new)
x.infant.death.new.county = rowSums(x.infant.death.new)

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Deaths with flaring from tx only ----
# Adult Deaths from tx
x.death.tx = x.mort * (1-(1/exp(DoseResponseAdult * x.pm.tx))) * x.pop.30
sum(x.death.tx)
x.death.tx.county = rowSums(x.death.tx)

# Infant deaths from tx
x.infant.death.tx = x.mort * (1-(1/exp(DoseResponseInfant * x.pm.tx))) * x.pop.infant
sum(x.infant.death.tx)
x.infant.death.tx.county = rowSums(x.infant.death.tx)

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Combine all death results to a df ----
x.health.outcomes.county = data.frame(ap3_county_fips = x.ap3.county,
                                      pm_25_conc_ori = x.pm.ori, 
                                      pm_25_conc_new = x.pm.new, 
                                      pm_25_conc_tx = x.pm.tx, 
                                      mort_pop_30_ori = x.death.ori.county, 
                                      mort_pop_30_new = x.death.new.county, 
                                      mort_pop_30_tx = x.death.tx.county, 
                                      mort_infant_ori = x.infant.death.ori.county, 
                                      mort_infant_new = x.infant.death.new.county,
                                      mort_infant_tx = x.infant.death.tx.county
                                      ) %>% 
  mutate(ap3_county_fips = case_when(ap3_county_fips == '12025' ~ '12086', 
                                     ap3_county_fips == '51560' ~ '51005', 
                                     ap3_county_fips == '46113' ~ '46102', 
                                     ap3_county_fips == '51515' ~ '51019', 
                                     TRUE ~ ap3_county_fips
  )) %>% 
  mutate(diff_ori_new = pm_25_conc_new - pm_25_conc_ori, 
         diff_ori_tx = x.pm.tx - pm_25_conc_ori, 
         mort_ori = mort_pop_30_ori + mort_infant_ori, 
         mort_new = mort_pop_30_new + mort_infant_new, 
         mort_tx = mort_pop_30_tx + mort_infant_tx, 
         mort_inc = mort_new - mort_ori, 
         mort_inc_tx = mort_tx - mort_ori, 
         monetized_value = mort_inc * WTP_Mort, 
         monetized_value_tx = mort_inc_tx * WTP_Mort, 
         monetized_value_label = paste(format(round(monetized_value / 1e6, 2), trim = TRUE), "M"))

sum(x.health.outcomes.county$mort_inc)


# Join county info ---
x.health.outcomes.county.geo = x.county.geo %>% 
  right_join(x.health.outcomes.county, by = c('geoid' = 'ap3_county_fips')) 

# Check TX 
x.health.outcomes.county.geo.tx = x.health.outcomes.county.geo %>% 
  filter(st_abbr == 'TX')

# National mortality increase
sum(x.health.outcomes.county.geo$mort_inc)
sum(x.health.outcomes.county.geo.tx$mort_inc)

# Texas mortality increase
sum(x.health.outcomes.county.geo$mort_inc_tx)
sum(x.health.outcomes.county.geo.tx$mort_inc_tx)

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
# ANALYSIS ----
### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----

### <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
## EASIUR ----
easiur.md = read.csv('easiur_EASIURIMPORT.csv') %>% 
  clean_names() %>% 
  select(starts_with('pm')) %>% 
  select(ends_with('ground'))

# Bind flare df with EASIUR damage
gas.flares.easiur = gas.flares %>% 
  cbind(easiur.md) %>% 
  mutate(bc_damage_easiur = pm25_annual_ground*black_carbon_billion_g_thosand_metric_tonnes*1000, 
         mort_easiur = bc_damage_easiur/8.8E6) %>% 
  filter(!is.na(pm25_annual_ground))

# Group by state 
gas.flares.easiur.st = gas.flares.easiur %>% 
  group_by(stusps) %>% 
  summarise(mort_easiur = sum(mort_easiur)) %>% 
  ungroup()

# Group by county 
gas.flares.easiur.county = gas.flares.easiur %>% 
  group_by(fips) %>% 
  summarise(mort_easiur = sum(mort_easiur)) %>% 
  ungroup()

# County geo
gas.flares.easiur.county.geo = x.county.geo %>% 
  right_join(gas.flares.easiur.county, by = c('geoid' = 'fips'))

sum(gas.flares.easiur$bc_damage_easiur)
sum(gas.flares.easiur$mort_easiur)
sum(gas.flares.easiur.150m$mort_easiur)

## <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
## AP3 ---- 
x.health.outcomes.county.grp = x.health.outcomes.county.geo %>% 
  st_drop_geometry() %>% 
  group_by(st_abbr) %>% 
  summarise(mort_inc = sum(mort_inc)) %>% 
  ungroup()

sum(x.health.outcomes.county.grp$mort_inc)

## <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
## InMAP ----
x.inmap.raw = st_read('../inmap/nei2005/2005nei_output_gas_flaring_12km.shp') %>% 
  mutate(inmap_id = as.character(row_number())) %>% 
  filter(Mort_InMAP<100)

sum(x.inmap.raw$Mort_InMAP)
sum(x.inmap.raw$Mort_Kr)

# Group to state/county level
x.state = st_read('../base/layer/us_state_geo.gpkg') %>% 
  filter(st_abbr %notin% c('AK', 'HI')) %>% 
  filter(st_abbr %in% c(state.abb, 'DC'))
x.county = st_read('../base/layer/us_county_geo.gpkg')

x.inmap.county = x.inmap.raw %>% 
  st_transform(crs = 4269) %>% 
  st_join(x.county) %>% 
  add_count(inmap_id, inmap_id) %>% 
  mutate(Mort_InMAP = Mort_InMAP/n, 
         AfterPM25 = BasePM25 + TotalPM25, 
         Mort_Kr = Mort_Kr/n, 
         NonWhitD = NonWhitD/n, 
         TotalPop = TotalPop/n, 
         WhitNoLatD = WhitNoLatD/n)

x.inmap.county.large = x.inmap.county %>% 
  filter(TotalPM25 >= 0.1)

x.inmap.state = x.inmap.raw %>% 
  st_transform(crs = 4269) %>% 
  st_join(x.state) %>% 
  add_count(inmap_id, inmap_id) %>% 
  mutate(Mort_InMAP = Mort_InMAP/n, 
         Mort_Kr = Mort_Kr/n, 
         NonWhitD = NonWhitD/n, 
         TotalPop = TotalPop/n, 
         WhitNoLatD = WhitNoLatD/n) %>% 
  st_drop_geometry()

x.inmap.state.grp = x.inmap.state %>% 
  group_by(st_abbr) %>% 
  summarise(Mort_InMAP = sum(Mort_InMAP), 
            Mort_Kr = sum(Mort_Kr), 
            WhitNoLatD = sum(WhitNoLatD), 
            NonWhitD = sum(NonWhitD)) %>% 
  ungroup()

x.inmap.county.grp = x.inmap.county %>% 
  st_drop_geometry() %>% 
  group_by(geoid, name, st_abbr, st_name) %>% 
  summarise(Mort_InMAP = sum(Mort_InMAP), 
            Mort_Kr = sum(Mort_Kr), 
            WhitNoLatD = sum(WhitNoLatD), 
            NonWhitD = sum(NonWhitD),
            BasePM25 = mean(BasePM25), 
            AfterPM25 = mean(AfterPM25), 
            PM25_INC = AfterPM25 - BasePM25, 
            PM25_PCT = (AfterPM25 - BasePM25)/AfterPM25) %>% 
  ungroup()

x.inmap.county.grp.geo = x.county.geo %>% 
  right_join(x.inmap.county.grp)

sum(x.inmap.state.grp$Mort_InMAP)
sum(x.inmap.state.grp$Mort_Kr)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
# Make Plot ----
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
## Load plotting need data only (previously saved) 
x.health.outcomes.county.grp = readRDS('./output/x.health.outcomes.county.grp.RDS')
gas.flares.easiur.st = readRDS('./output/gas.flares.easiur.st.RDS')
x.inmap.state.grp = readRDS('./output/x.inmap.state.grp.RDS')
x.health.outcomes.county.geo = readRDS('./output/x.health.outcomes.county.geo.RDS')
x.inmap.county.grp = readRDS('./output/x.inmap.county.grp.RDS')

x.ap3.mort.county = x.health.outcomes.county.geo %>% st_drop_geometry() %>% 
  select(geoid, name, st_abbr, st_name, mort_inc_ap3 = mort_inc)

x.inmap.mort.county = x.inmap.county.grp %>% 
  select(geoid, name, st_abbr, st_name, mort_inc_inmap = Mort_InMAP)

x.ap3.inmap.mort.county = x.ap3.mort.county %>% 
  inner_join(x.inmap.mort.county)

ap3_inmap_lm = lm(formula = mort_inc_ap3 ~ mort_inc_inmap, data = x.ap3.inmap.mort.county)
summary(ap3_inmap_lm)


## P1: Mortality from three models (ND, TX, others) [bar] ----
x.ap3.p1 = x.health.outcomes.county.grp %>% 
  mutate(state_grp = ifelse(st_abbr != 'TX'&st_abbr != 'ND', 'Other', st_abbr)) %>% 
  filter(!is.na(st_abbr)) %>% 
  group_by(state_grp) %>% 
  summarise(mort = sum(mort_inc), 
            model = 'AP3') %>% 
  ungroup()

x.easiur.p1 = gas.flares.easiur.st %>% 
  mutate(state_grp = ifelse(stusps != 'TX'&stusps != 'ND', 'Other', stusps)) %>% 
  filter(!is.na(stusps)) %>% 
  group_by(state_grp) %>% 
  summarise(mort = sum(mort_easiur), 
            model = 'EASIUR') %>% 
  ungroup()

x.inmap.p1 = x.inmap.state.grp %>% 
  mutate(state_grp = ifelse(st_abbr != 'TX'&st_abbr != 'ND', 'Other', st_abbr)) %>% 
  filter(!is.na(st_abbr)) %>% 
  group_by(state_grp) %>% 
  summarise(mort = sum(Mort_InMAP), 
            model = 'InMAP') %>% 
  ungroup()

sum(x.ap3.p1$mort)
sum(x.easiur.p1$mort)
sum(x.inmap.p1$mort)


x.p1 = x.ap3.p1 %>% 
  rbind(x.easiur.p1) %>% 
  rbind(x.inmap.p1) %>% 
  arrange(match(state_grp, c("TX", "ND", 'Other'))) %>% 
  mutate(state_grp = factor(state_grp, level = unique(state_grp)), 
         money = mort * 8500000)

ggplot(data = x.p1, aes(x=model, y=mort, fill=state_grp)) + 
  geom_bar(stat="identity", position="dodge", width = 0.8, size = 0.3) + 
  geom_text(aes(label = paste0(round(mort, 1))), 
            fontface = 'bold',
            size = 3.5, 
            position = position_dodge(width = 0.8), 
            vjust = -0.3) + 
  scale_y_continuous("Predicted mortality (deaths)", 
                     breaks = breaks_pretty(6),
                     # sec.axis = sec_axis(~. * 8500000, 
                     #                     breaks = breaks_pretty(6),
                     #                     labels = label_dollar(scale = 1e-6, suffix = "M"), 
                     #                     name = "Monetized impact ($)"),
                     expand = expansion(mult=c(0,0.14)))+
  scale_fill_manual(values = c('#D72638', '#3F88C5', '#F49D37')) +
  theme_minimal() +
  labs(fill='State') + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.background = element_rect(fill = 'white', color = 'white'),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size= 13, 
                                    face = 'bold'),
        axis.text.x = element_text(size= 10, 
                                   face = 'bold'),
        axis.text.y = element_text(size= 10, 
                                   face = 'bold')
        )

# Write
path = paste0("./vis/paper_viz_p1.png")
ggsave(path, width = 6, height = 6, dpi = 200, units = "in")

## P2: County scatter map [point] ----
# Text
coef <- round(coef(ap3_inmap_lm), 2)
r2 <- round(summary(ap3_inmap_lm)$r.squared,2)

plot_county_point = ggplot(data = x.ap3.inmap.mort.county, aes(x = mort_inc_inmap, y = mort_inc_ap3)) + 
  geom_point(shape=1, alpha = 1, color = '#995D81') + 
  geom_smooth(method=lm,   # Add linear regression line
              se=F, 
              color = '#EB8258', 
              alpha = 0.6) + 
  scale_x_continuous(expand = c(0, 0.), 
                     limits = c(0, 0.83),
                     "Mortality per county (InMAP)") + 
  scale_y_continuous("Mortality per county (AP3)", 
                     expand = c(0, 0),
                     limits = c(0, 2.7)) +
  geom_label_repel(data = filter(x.ap3.inmap.mort.county, mort_inc_inmap > 0.23 | mort_inc_ap3 > 0.5),
                   aes(label = paste0(name, ', ', st_abbr)), size = 2.5,
                   min.segment.length = 30,
                   fontface = 2, box.padding = 0.1,
                   max.overlaps = 14) +
  annotate(geom="text",  
           label = paste0("Y = ", coef[1]," + ", coef[2], "x"), 
           x = 0.6, y = 2.3, 
           fontface = 2) + 
  annotate(geom="text",  
           label = bquote(R^2 == 0.62), 
           x = 0.6, y = 2.1,
           fontface = 2) + 
  theme_classic() +
  theme(panel.grid.major = element_line(colour = "grey"), 
        plot.background = element_rect(fill = 'white', color = 'white'),
        axis.title.x = element_text(size= 10, 
                                    face = 'bold'),
        axis.title.y = element_text(size= 10, 
                                    face = 'bold'),
        axis.text.x = element_text(size= 10, 
                                   face = 'bold'),
        axis.text.y = element_text(size= 10, 
                                   face = 'bold')
  )

## P3: state bar chart [bar] ----
# Group by state 
x.ap3.inmap.mort.state = x.ap3.inmap.mort.county %>% 
  group_by(st_abbr, st_name) %>% 
  summarise(`InMAP` = sum(mort_inc_inmap, na.rm = T), 
            `AP3` = sum(mort_inc_ap3, na.rm = T)
  ) %>% 
  ungroup() %>% 
  filter(`InMAP` >= 0.2 & `AP3` >= 0.2) %>% 
  gather(key = key, value = value, -c(st_abbr, st_name)) %>% 
  arrange(- value, key) %>% 
  mutate(st_abbr = factor(st_abbr, level = unique(st_abbr)))

plot_state_bar = ggplot(data = x.ap3.inmap.mort.state, aes(x = st_abbr, y= value, fill = key)) + 
  geom_bar(stat = 'identity', 
           position = 'dodge') + 
  scale_y_continuous(breaks = pretty_breaks(10), 
                     expand = c(0, 0), 
                     "Mortality per state") + 
  scale_fill_manual(values = c('#A3C4BC', '#C97C5D')) + 
  theme_classic() + 
  theme(legend.position = 'bottom', 
        legend.title = element_blank(), 
        legend.text = element_text(size = 8, face = 'bold'), 
        legend.key.size = unit(10, "pt"),
        panel.grid.major.y = element_line(colour = "grey"), 
        plot.background = element_rect(fill = 'white', color = 'white'),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size= 10, 
                                    face = 'bold'),
        axis.text.x = element_text(size= 10, 
                                   face = 'bold'),
        axis.text.y = element_text(size= 10, 
                                   face = 'bold')
  )
  

Text_a = textGrob('(a)', gp=gpar(fontface="bold", fontsize = 10), vjust = 0.1)
Text_b = textGrob('(b)', gp=gpar(fontface="bold", fontsize = 10), vjust = -0.3)

ggplot() + 
  coord_equal(xlim = c(0, 6), ylim = c(0, 6), expand = FALSE) + 
  annotation_custom(Text_a, xmin = 0, xmax = 0.2, ymin = 2.6, ymax = 6) + 
  annotation_custom(Text_b, xmin = 0, xmax = 0.2, ymin = 0.5, ymax = 2.6) + 
  annotation_custom(ggplotGrob(plot_state_bar), xmin = 0.2, xmax = 6, ymin = 0, 
                    ymax = 2.6) + 
  annotation_custom(ggplotGrob(plot_county_point), xmin = 0.2, xmax = 6, ymin = 2.6, 
                    ymax = 6) + 
  theme_void() + 
  theme(plot.background = element_rect(fill = 'white', color = 'white'))
  
  
# Write
path = paste0("./vis/fig5.png")
ggsave(path, width = 7, height = 7, dpi = 500, units = "in")


## P4: Morality maps by county [map] ----
gas.flares.geo = st_read('./output/bc_gas_flaring.shp') %>% 
  st_filter(x.state)

my_breaks <- c(0, 0.001, 0.01, 0.1, 1, 10)
my_labels <- c("0", '0.001', '0.01', '0.1', '1',">10")


ap3_map = ggplot() + 
  geom_sf(data = x.health.outcomes.county.geo, aes(fill = mort_inc), size = 0.1) + 
  geom_sf(data = x.state, size = 0.7, fill = NA, color = 'black') + 
  # geom_sf(data = gas.flares.geo, size = 0.2, color = '#84DCCF', alpha = 0.2) + 
  scale_fill_viridis_c(option = "inferno",
                       direction = -1,
                       limits=c(0,10),
                       breaks = my_breaks,
                       trans = pseudo_log_trans(sigma = 0.0001, 10),
                       alpha = 0.9,
                       labels = my_labels) +
  theme_void() + 
  labs(fill = 'Deaths') + 
  theme(legend.position = 'right', 
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 10, face = 'bold'), 
        legend.key.size = unit(80, "pt"),
        legend.key.width = unit(5, 'pt'), 
        legend.key.height = unit(20, 'pt'), 
        plot.background = element_rect(fill = 'white', color = 'white')
  )


gas.flares.easiur.county.geo.plot = gas.flares.easiur.county.geo %>% 
  mutate(mort_easiur = ifelse(mort_easiur > 10, 10, mort_easiur))

easiur_map = ggplot() + 
  geom_sf(data = gas.flares.easiur.county.geo.plot, aes(fill = mort_easiur), size = 0.1) + 
  geom_sf(data = x.state, size = 0.7, fill = NA, color = 'black') + 
  # geom_sf(data = gas.flares.geo, size = 0.1, color = '#84DCCF', alpha = 0.1) + 
  scale_fill_viridis_c(option = "inferno",
                       direction = -1,
                       limits=c(0,10),
                       breaks = my_breaks,
                       trans = pseudo_log_trans(sigma = 0.0001, 10),
                       alpha = 0.9,
                       labels = my_labels) +
  theme_void() + 
  labs(fill = 'Deaths') + 
  theme(legend.position = 'right', 
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 10, face = 'bold'), 
        legend.key.size = unit(80, "pt"),
        legend.key.width = unit(5, 'pt'), 
        legend.key.height = unit(20, 'pt'), 
        plot.background = element_rect(fill = 'white', color = 'white')
  )



inmap_map = ggplot() + 
  geom_sf(data = x.inmap.county.grp.geo, aes(fill = Mort_InMAP), size = 0.1) + 
  geom_sf(data = x.state, size = 0.7, fill = NA, color = 'black') + 
  # geom_sf(data = gas.flares.geo, size = 0.1, color = '#84DCCF', alpha = 0.1) + 
  scale_fill_viridis_c(option = "inferno", 
                       direction = -1,
                       limits=c(0,10),
                       breaks = my_breaks,
                       trans = pseudo_log_trans(sigma = 0.0001, 10),
                       alpha = 0.9,
                       labels = my_labels) +
  theme_void() + 
  labs(fill = 'Deaths') + 
  theme(legend.position = 'right', 
        legend.title = element_text(size = 13, face = 'bold'),
        legend.text = element_text(size = 10, face = 'bold'), 
        legend.key.size = unit(80, "pt"),
        legend.key.width = unit(5, 'pt'), 
        legend.key.height = unit(20, 'pt'), 
        plot.background = element_rect(fill = 'white', color = 'white')
  )

Text1 = textGrob('EASIUR', rot = 90, gp=gpar(fontface="bold", fontsize = 15), hjust = -0.1)
Text2 = textGrob('AP3', rot = 90, gp=gpar(fontface="bold", fontsize = 15), hjust = -0.3)
Text3 = textGrob('InMAP', rot = 90, gp=gpar(fontface="bold", fontsize = 15), hjust = -0.2)

ggplot() +
  coord_equal(xlim = c(0, 5), ylim = c(0, 4), expand = FALSE) + 
  # coord_equal(xlim = c(0, 5), ylim = c(0, 6), expand = FALSE) + 
  annotation_custom(ggplotGrob(inmap_map), xmin = 0.3, xmax = 5, ymin = 0, 
                    ymax = 2) + 
  annotation_custom(ggplotGrob(ap3_map), xmin = 0.3, xmax = 5, ymin = 2, 
                    ymax = 4) + 
  # annotation_custom(ggplotGrob(easiur_map), xmin = 0.3, xmax = 5, ymin = 4, 
  #                   ymax = 6) + 
  annotation_custom(Text3, xmin = 0, xmax = 0.3, ymin = 0, ymax = 2) + 
  annotation_custom(Text2, xmin = 0, xmax = 0.3, ymin = 2, ymax = 4) + 
  # annotation_custom(Text1, xmin = 0, xmax = 0.3, ymin = 4, ymax = 6) + 
  theme_void() + 
  theme(plot.background = element_rect(fill = 'white', color = 'white'))



# Write
path = paste0("./vis/paper_viz_p6.png")
ggsave(path, width = 6, height = 4, dpi = 500, units = "in")


# P5: EASIUR moving to appendix ----
ggplot() +
  coord_equal(xlim = c(0, 5), ylim = c(0, 2), expand = FALSE) + 
  annotation_custom(ggplotGrob(easiur_map), xmin = 0.3, xmax = 5, ymin = 0,
                    ymax = 2) +
  annotation_custom(Text1, xmin = 0, xmax = 0.3, ymin = 0, ymax = 2) +
  theme_void() + 
  theme(plot.background = element_rect(fill = 'white', color = 'white'))

# Write
path = paste0("./vis/paper_viz_pa1.png")
ggsave(path, width = 6, height = 3, dpi = 500, units = "in")

## <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----
# EOF ----
## <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> ----