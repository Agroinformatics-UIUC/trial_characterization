######################################
#HordNeef-F17 2018 Trial Design 05Apr18_01#
######################################
# Written by David Bullock

#Objective:  Design 2018 HordNeef-F17 soy population trial.

#===================================
# Preparation
#===================================
#--- packages ---#
library(data.table)
library(tidyverse)
library(parallel)
library(sp)
library(rgdal)
library(ggplot2)
library(raster)
library(maptools)
library(rgeos)
library(broom)
library(geosphere)
library(tmap)
library(sf)
library(tidyverse)
library(viridis)
library(rvest)
library(RColorBrewer)

#--- source functions ---#
source('~/Box Sync/DIFM_trial_design/Functions/functions.R')

# ###################----------------------------#####################
# Email from Aaron Gingerich:

# David,

# Attached is new corn field for the 2018 crop that I would like to enroll in the DIFM project.

# It is named Gingerich2. I have attached a boundary, soil map, GF planting RX, guidance line, and a fall Dap map
# If you need a static rate instead of RX for what we would plant in the field average is 34,700 seeds per acre
# There will need to be 120’ border around the entire field.
# The field is farmed east and west.
# I prefer the plot start on the south side 120’ from the boundary.
# We will be using 60’ equipment again this year.
# If the guidance line does upload for you it marks the edge of the field and the desired heading of the plot. 
# The center of the first pass of the plot will be 150’ north of this line. Heading # is 271.14923096

# FYI, my corn planter will have the smart firmer from Precision Planting installed on it. Don’t know if that replaces your veris data but wanted you to know.
# The attached fall Dap map will show 100 & 200 # spread.  I plan to treat this field with a flat rate of 52.5 units of N at planting, and an average of 147 units of N at sidedress  V5-V7.  
# All N will be 32% and applied as gallons per acre on the monitor. 

# Let me know if you have any questions or need any other info.
# Please forward to others on the team that may need this info.

# Thanks Aaron Gingerich
 
#--------------------------
# Bullock’s interpretation and suggestions:
#--------------------------
# In the shapefiles falldapspread_poly.shp and 2018GFPlantRX_poly.shp you will see that the field is partioned into 19 “management zones” (denoted by Obj_Id). 

# For Obj_id = 1, 2, 12, 14, and 18, the base nitrogen application rate is 52.5 + 100x0.18 = 70.5
# On the other fourteen Obj_id, the base nitrogen rate is 52.5 + 200x0.18 = 88.5.

# After that base rate is applied, later he wants to average 147 pounds of N, which he will apply in the form UAN32.  
# A gallon of UAN 32% weighs 11.06 pounds, and 32% of the weight in nitrogen.
 
# GAL32_1 <- 97/(11.06*0.32)
# GAL32_2 <- 122/11.06*0.32)
# GAL32_3 <- 147/(11.06*0.32)
# GAL32_4 <- 172/(11.06*0.32)
# GAL32_5 <- 197/(11.06*0.32)

# Those are the numbers that the machine will read when it puts down variable rates.
# So, on Obj_ID = 1, 2, 12, 14, 18, total amounts of N will be 70.5 + 97, 70.5 + 122, 70.5 + 147, 70.5 +172, and 70.5 + 197.
# In the other fourteen ObJ_Id, total amounts of N will be 88.5 + 97, 88.5 + 122, 88.5 + 147, 88.5 +172, and 88.5 + 197.

# You should make N target rate maps that show gallons of UAN32 applied, and also total N applied.
# Three seed rate zones.

# Zone32 is made up of the Zones with 32K target seed rates: zone 8
# Zone34 is made up of the Zones with 34K target seed rates: zones 3, 4, 9, 11, and 13-17 
# Zone36 is made up of the Zones with 36K target seed rates:  zones 1, 2, 6, 7, 10, 12, 18, 19

# Treatments:

# In Zone32 there will be 20 treatments, which are the combinations of the five GAL32 values and seed rates 24, 28, 32, 36.  (Zone32 is small, so you may not fit 20 plots in that.  If not, make sure that at least a couple have the status quo treatment (GAL32_3, 32K).
# In Zone34 there will be 20 treatments, which are the combinations of the five GAL32 values and seed rates 26, 30, 34, 38.
# In Zone36 there will be 20 treatments, which are the combinations of the five GAL32 values and seed rates 28, 32, 36, 40.

# Put a 120-foot buffer around the whole field.
# Plots are 60 feet wide.
# There is an AB-line shapefile (SouthsideGuidanceline_ln.shp). 
# The the first pass that is not in the buffer zone will be along a line parallel to the AB-line, 
# except 150 feet to north of it.  (That is, 120 feet for the buffer, then 30 more feet because the equipment is 60 feet wide, 
# so its middle should go on a line 30 feet from the buffer.)  The heading of the AB-line is 271.14923096.

# Please make two N target maps, one showing the GAL_32 applied variably, and the other showing total N (in pounds).    

#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
# Dave's answer to Taro's request to describe exactly what the designs are
#
# The plots need to be 40 feet wide.  Let’s make a buffer with width twice that.

# The Rx files show that this field has four kinds of zones:

# Zones_1, on which the recommended rates are pounds of NH3 = 144 and seed rate = 32K
# Zones_2, on which the recommended rates are pounds of NH3 = 134 and seed rate = 34K
# Zones_3, on which the recommended rates are pounds of NH3 = 124 and seed rate = 36K
# Zones_4, on which the recommended rates are pounds of NH3 = 114 and seed rate = 38K.

# The first thing I notice is that Zones_1 and Zones_4 are small.  I think it will make sense to therefore just make the design with two zones:
# Zones_12 is the union of Zones_1 and Zones_2.
# Zones_12 is the union of Zones_3 and Zones_4.

# First, determine, for every plot, what zone it in.  You said you just wanted to use the zone that has the largest share in the plot.  That is fine with me.   

# Zones_12:  Use NH3 = 84, 114, 134, 154
# Zones_12:  Use seed = 28, 32, 34, 38


# Zones_34:  Use NH3 = 74, 104, 124, 144
# Zones_34:  Use seed = 30, 34, 36, 40

# I know that it would be easier just to use the same 16 treatments in both Zones_A and Zones_B.  But Dave Neef asked me to design it this way, so let’s do it.
 
#===================================
# Import data  
#===================================
setwd('~/Box Sync/DIFM_trial_design/HortonNeef_2018/Data')

#--------------------------
# Boundary 
#--------------------------
boundary <- st_read(dsn = ".", "HordNeef-F17 boundary") %>% 
  st_transform(.,26917)

#From past application maps, I see they start in the southwest corner of this field:

#--------------------------
# ab line
#--------------------------
#Make an ab-line, which I'm getting from two points on the western boundary, from zooming in with QGIS.
#Later, I will move this east by 40 feet.
# HordNeef_F17_Long1 <- -82.9830885
# HordNeef_F17_Long2 <- -82.9832275
# HordNeef_F17_Lat1 <- 40.7402288
# HordNeef_F17_Lat2 <- 40.7463182

# ab_line <- rbind(c(HordNeef_F17_Long1, HordNeef_F17_Lat1),c(HordNeef_F17_Long2, HordNeef_F17_Lat2)) %>% 
#   st_linestring() %>% 
#   st_sfc() %>% 
#   st_sf %>% 
#   st_set_crs(4326) 

# st_write(ab_line,'HordNeef-F17 ab_line.shp',delete_layer=TRUE)

ab_line <- st_read(dsn='.','HordNeef-F17 ab_line') %>% 
  st_transform(26917)

#===================================
# Preparation 
#===================================
####Some initial parameters that have to be stuck in:

#--------------------------
# Set parameters
#--------------------------
#--- plot width ---#
plot_width_ft <- 40 
feetinameter <- 3.28082
plot_width_meter <- plot_width_ft/feetinameter

#--- buffer width ---#
buffer_width_ft <- 80 
buffer_width_meter <- buffer_width_ft/feetinameter

#--------------------------
# Inner field
#--------------------------
infield <- st_buffer(boundary,-buffer_width_meter) %>% 
  dplyr::select('geometry') %>%  
  dplyr::mutate(type='in')

#--------------------------
# Buffer
#--------------------------
outfield <- st_difference(boundary,infield) %>% 
  dplyr::select('geometry') %>% 
  dplyr::mutate(type = 'out')

#--------------------------
# Whole field
#--------------------------
wholefield <- rbind(infield, outfield) 

#--------------------------
# Check if things are okay
#--------------------------
# tm_shape(wholefield) + 
#   tm_borders(col='green') + 
#   tm_compass() + 
#   tm_grid() +
# tm_shape(ab_line) + 
#   tm_lines(col='red', scale = 3)


#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#

#===================================
# Make plots
#===================================
#--------------------------
# Set parameters 
#--------------------------
#--- set directions ---#
# pick one of c('SN','NS','WE','EW')
long_in <- 'NS' # the direction in which the applicator moves first

# pick one of c('SN','NS','WE','EW')
# SN means the applicator goes from south to north 
short_in <- 'EW' 

#--------------------------
# Find starting points
#--------------------------
temp <- st_geometry(infield)[[1]][[1]] %>% 
  apply(1, function(x) list(c(x[1],x[2]))) %>% 
  lapply("[[", 1) %>% 
  lapply(function(x) st_point(x)) %>% 
  st_sfc() %>% 
  st_sf() %>% 
  mutate(id=1:nrow(.))

#--- manually inspect to identify the left-upper corner of the boundary ---#
tm_shape(temp) +
  tm_text('id')

#--- it's 5 th observation ---#
starting_point <- temp[15,] %>% 
  st_coordinates 

#--------------------------
# Identify plot length
#--------------------------
bbox_field <- st_bbox(infield)
field_length_meter <- bbox_field['ymax']-bbox_field['ymin']
plot_length_meter <- field_length_meter/10-0.5

#--------------------------
# Make plots
#--------------------------
design_grids <- make_grids(
  ab_line,long_in,short_in,
  plot_length_meter,plot_width_meter,starting_point
  ) %>% 
  st_set_crs(.,st_crs(wholefield))

ggplot() +
  geom_sf(data=design_grids) +
  geom_sf(data=infield)

#--- clip by the field boundary  ---#
trial_grids <- st_intersection(design_grids,infield) %>% 
  mutate(max_area=as.numeric(max(st_area(.)))) %>% 
  mutate(area=as.numeric(st_area(.))) %>%
  filter(area>=(max_area-200)) 

# tm_shape(wholefield) + tm_borders(col='blue') + 
#   tm_shape(ab_line) + tm_lines(col='red', scale = 4) +
#   tm_shape(trial_grids) + tm_polygons() + tm_text('plotid', size = 0.5)

#===================================
# Assigning rates
#===================================
#--------------------------
# Seed recommendation map
#--------------------------
RxSeedMap <- st_read(dsn = ".", "HordNeef-F17 2018 Seed Rx") %>% 
  setnames(names(.),tolower(names(.))) %>% 
  mutate(seed_grid_id = 1:nrow(.)) %>% 
  st_transform(.,26917) %>% 
  .[-which(is.na(st_is_valid(.))),] # get rid of invalid polygon

#--- intersects with the plots ---#
RxAndPlotGrid_utm <- st_intersection(trial_grids,RxSeedMap) %>% 
  dplyr::arrange(plotid) %>% 
  mutate(area:=as.numeric(st_area(.))) %>% 
  data.table() %>% 
  #--- calculate area by plotid and target rate ---#
  .[,.(area=sum(area)),by=.(target_rat,plotid)] %>% 
  #--- pick the dominant one ---#
  .[,.SD[which.max(area),],by=plotid] %>% 
  #--- assign new zone ids based on the Dave's plan (found at the beginning of the file) ---#
  .[,zone:=ifelse(target_rat %in% c(32000,34000),'zone_1','zone_2')]  

#--------------------------
# Zone 1
#--------------------------
seed_treat_ls <- list()
seed_treat_ls$zone_1 <- c(28, 32, 34, 38)*1000
seed_treat_ls$zone_2 <- c(30, 34, 36, 40)*1000

NH3_treat_ls <- list()
NH3_treat_ls$zone_1 <- c(84, 114, 134, 154)
NH3_treat_ls$zone_2 <- c(74, 104, 124, 144)

#--------------------------
# Zone 2
#--------------------------

get_design <- function(i){

  zone_name <- paste('zone_',i,sep='')

  #--- working data sets ---#
  temp_plots <- RxAndPlotGrid_utm[zone==zone_name,]
  temp_num_plots <- nrow(temp_plots)

  #--- define treatments ---#
  treat_data <- expand.grid(
      seed_treat_ls[[zone_name]],
      NH3_treat_ls[[zone_name]]
      ) %>% 
    data.table() %>% 
    setnames(names(.),c('seed_rate','NH3_rate')) %>% 
    .[,treat_id:=1:nrow(.)] %>% 
    setkey(treat_id)

  num_trt <- nrow(treat_data)

  quotient <- temp_num_plots %/% num_trt
  remainder <- temp_num_plots %% num_trt

  treat_ls_temp <- c(replicate(quotient,sample(1:num_trt,num_trt)) %>% as.vector(),sample(1:num_trt,remainder)) 

  w_design <- temp_plots %>% 
    .[,treat_id:=treat_ls_temp] %>% 
    setkey(treat_id) %>% 
    treat_data[.] %>% 
    .[,.(plotid,seed_rate,NH3_rate,zone)] 

  return(w_design)
}
  
whole_design <- mclapply(1:2,get_design,mc.cores=2) %>% 
  rbindlist()

#--------------------------
# Merge it back to the sf
#--------------------------
in_design <- left_join(trial_grids,whole_design,by='plotid') %>% 
  mutate(geometry=st_sfc.col_polygons_ls.) %>% 
  st_set_geometry('geometry') %>% 
  mutate(drop=0) %>%  
  dplyr::select(-st_sfc.col_polygons_ls.) %>% 
  dplyr::select(drop,NH3_rate,seed_rate)  

out_design <- st_difference(RxSeedMap,st_union(trial_grids)) %>% 
  rename(seed_rate=target_rat) %>% 
  mutate(
    NH3_rate= case_when(
      seed_rate == 32000 ~ 144,
      seed_rate == 34000 ~ 134,
      seed_rate == 36000 ~ 124,
      seed_rate == 38000 ~ 114
      ),
    drop=1
    ) %>% 
  dplyr::select(drop,NH3_rate,seed_rate)

whole_design <- rbind(in_design,out_design)

#===================================
# Save
#===================================
ggplot(whole_design) +
  geom_sf(aes(fill = factor(drop))) +
  scale_fill_manual("Status",labels = c("in trial","out of trial"), values = c("white","grey")) +
  ggtitle("HordNeef-F17 2018 Plots \n in and out of Trial") +
  theme_bw()
ggsave('./../Design/Niese_2018_in_or_out.pdf')

ggplot(whole_design) +
  geom_sf(aes(fill = factor(NH3_rate))) 
ggsave('./../Design/Niese_2018_N_design.pdf')

ggplot(whole_design) +
  geom_sf(aes(fill = factor(seed_rate))) 
ggsave('./../Design/Niese_2018_seed_design.pdf')

whole_design <- st_transform(whole_design,4326)
st_write(whole_design,'./../Design/HordNeef_2018_Trial_Design.shp',delete_layer=TRUE)


