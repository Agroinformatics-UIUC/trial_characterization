######################################
#Bohnhoff-Adams 2018 Trial Design 10Mar18.R#
##########`############################


#===================================
# Preparation
#===================================
#--- packages ---#
library(data.table)
library(dplyr)
library(parallel)
library(sp)
library(rgdal)
library(ggplot2)
library(raster)
library(maptools)
library(rgeos)
library(sf)
library(broom)
library(geosphere)
library(tidyverse)
library(tmap)
library(RColorBrewer)
source('C:/Users/germa/Box Sync/DIFM_Data/DIFM 2018 Trial Design/trial.design.functions.gm.R')
source('D:/Box Sync/DIFM_Data/DIFM 2018 Trial Design/trial.design.functions.gm.R')

#############################------Field Management Info Below-------##############################
farmer.name <- 'Nelson'
farm.name <- 'Wirth'


#--- grid parameters in feet ---#
plot_length_ft <- 561.335/2
plot_width_ft <- 425.169/4
# planter_width_ft <- 20 #Dual hydraulic drive.  He is going to plant one hybrid on the right, another from the left.
# combine_width_ft <- 20
# sprayer_width_ft <- 20

#--- meter to feet ---#
feetinameter <- 3.28082

#--- grid parameters in meter ---#
plot_length_meter <- plot_length_ft/feetinameter
plot_width_meter <- plot_width_ft/feetinameter
# planter_width_meter <- planter_width_ft/feetinameter
# sprayer_width_meter <- sprayer_width_ft/feetinameter

####################Step 1:  Bring in Bohnhnoff-Adams 2018 Trial Design Data ###########################

setwd('C:/Users/germa/Box Sync/DIFM_Data/DIFM 2018 Trial Design/Nelson-Wirth 2018 Trial Design.german')
setwd('D:/Box Sync/DIFM_Data/DIFM 2018 Trial Design/Nelson-Wirth 2018 Trial Design.german')
boundary.sfdf <- st_read(dsn = "./Input Data", "Nelson18_Wirth_Boundary")
# st_crs(boundary.sfdf) <- '+proj=longlat +datum=WGS84 +no_defs' 
boundary.utm <-boundary.sfdf %>% st_transform(26915) #UTM.Zone = 15N http://spatialreference.org/ref/epsg/26915/
ab_line.sfdf <- st_read(dsn = './Input Data', "2018 Nelson Family Farms Wirth Guidance_ln")
ab_line.utm <-ab_line.sfdf %>% st_transform(26915) 
# st_crs(ab_line) <- "+proj=longlat +datum=WGS84 +no_defs"
# ab_line <- st_transform(ab_line,26916) %>% filter(NAME == 'North AB') #we will use only the North abline


infield <- st_buffer(boundary.utm,-plot_width_meter*2/feetinameter) %>% 
  dplyr::select('geometry') %>%  dplyr::mutate(buffer = 0)#The end rows will be 80 feet, 2 passes of the planter rather than 100 feet.
outfield <- st_difference(boundary.utm,infield) %>% 
  dplyr::select('geometry') %>% dplyr::mutate(buffer = 1)

wholefield <- rbind(infield, outfield) %>% dplyr::mutate(FarmName = farmer.name,
                                                         FieldName = farm.name)

tm_shape(wholefield) + tm_borders(col='green') + tm_compass() + tm_grid() #+
#   tm_shape(ab_line) + tm_lines(col='red', scale=3)

#--- meter to feet ---#
feetinameter <- 3.28084

#--- identify the boundary ---#
# NOTE: we want to only select the area of the field that we will design the grid. That means, exclude the buffer area
bbox_field <- st_bbox(wholefield[wholefield$buffer == 0,])#+c(-20,-35,50,25)

#--- identify the starting point ---#
#IMPORTANT: The starting point A tells were we want to start drawing. It usually will be in one of the corners of the infield area (after excluding the buffer area)
#The starting point B makes a straight line with the point A and tells the direction how we want to make the columns of the trial
#I am getting the starting point A and B by opening the file in QGIS, zooming at maximum and coping the coordinates shown in the middle lower window
st_write(wholefield[wholefield$buffer == 0,], "findstartingpoints.shp", delete_dsn=TRUE)

#starting_point_A <- c(725354.93167863,4571840.44416983+24)
#starting_point_B <- c(725354.93167863, 4572137.4011694)
ab_line.utm$geometry

starting_point_A.abline <- c(726485.4, 4571904)
starting_point_B.abline <- c(726483.9,4572021)

starting_point_A <- c(726472.695487767,4571853.299129922+18)
offset.x <- starting_point_A.abline[1] - starting_point_A[1]
offset.y <- starting_point_A.abline[2] - starting_point_A[2]

starting_point_B <- starting_point_B.abline - c(offset.x, offset.y)

ab_string <- rbind(starting_point_A,starting_point_B) %>% 
  st_linestring()  

ab_line <-st_sf(id=c('ab'), st_sfc(ab_string)) 
st_crs(ab_line) <- st_crs(wholefield)
# st_write(ab_line, "ab_line.shp", delete_dsn=TRUE)
#Plot it to check if it is well located
starting_point.sf <- st_sf(st_sfc(st_point(x = starting_point_A)))
st_crs(starting_point.sf) <- st_crs(wholefield)

tm_shape(wholefield) + tm_borders(col='green') + tm_compass() + tm_grid() +
  tm_shape(ab_line) + tm_lines(col='red', scale=2) +
  tm_shape(starting_point.sf) + tm_dots(col='blue', scale=3)

design_grids_utm <- make_grids(ab_line)
st_crs(design_grids_utm) <-st_crs(wholefield) #boundary file came with crs as utm

# st_crs(all_grids) <-st_crs(wholefield)

tm_shape(wholefield) + tm_borders(col='blue') + 
  tm_shape(ab_line) + tm_lines(col='red', scale = 3) +
  # tm_shape(all_grids) + tm_borders(col='green')
   tm_shape(design_grids_utm) + tm_borders(col='green')+ tm_text('plotid', size = 0.5) 

CentX <- lapply(st_geometry(st_centroid(design_grids_utm)),function(x) x[1]) %>% unlist()
CentY <- lapply(st_geometry(st_centroid(design_grids_utm)),function(x) x[2]) %>% unlist()
design_grids_utm <- design_grids_utm %>% 
  mutate(cent_x=CentX,cent_y=CentY)

trial <- wholefield[wholefield$buffer == 0,]
trial_grid1_utm <- st_intersection(trial, design_grids_utm)
trial_grid1_utm$Name <- NULL
rownames(trial_grid1_utm) <- 1:nrow(trial_grid1_utm)

tm_shape(wholefield) + tm_borders(col='blue') + 
  tm_shape(ab_line) + tm_lines(col='red', scale = 4) +
  tm_shape(trial_grid1_utm) + tm_borders(col='green') + tm_text('plotid', size = 0.5)

max_area <- as.numeric(max(st_area(trial_grid1_utm)))


#First subset the plots that do not have the right area
trial_grid2 <- trial_grid1_utm %>%
  mutate(area=as.numeric(st_area(.))) %>%
  mutate(drop=ifelse(area<(max_area*0.95), 1, 0))

tm_shape(wholefield) + tm_borders(col='blue') + 
  tm_shape(ab_line) + tm_lines(col='red', scale = 4) +
  tm_shape(trial_grid2) + tm_polygons('drop') + tm_text('plotid', size = 0.5)

#REMOVE ONE ISOLATED PLOT 
trial_grid2[trial_grid2$plotid == 303,'drop'] <- 1

#CREATE NEW PLOTID ONLY FOR PLOTS WITH TREATMENTS
trial_grid2$plotid <- 0
trial_grid2$plotid[trial_grid2$drop == 0] <- seq.int(nrow(trial_grid2[trial_grid2$drop == 0,]))
tm_shape(trial_grid2) + tm_polygons('plotid') + tm_text('plotid', size = 0.5)


#remove plots we will not use
trial_grid2 <- trial_grid2[trial_grid2$plotid >0,] 

#ASSIGN THE N TREATMENTS
tm_shape(trial_grid2) + tm_polygons('plotid') + tm_text('plotid', size = 0.5)

num_treat <- 16
set.seed(123)
id.plots <- unique(trial_grid2$plotid)
number.plots <- length(id.plots)
treat.options <- rep(1:num_treat, floor(number.plots/num_treat))
complete <- sample(1:num_treat, number.plots - length(treat.options), replace = FALSE)
treat.options <- c(treat.options, complete)
treat.options.random <- sample(treat.options,number.plots,replace=FALSE) 
treat_type.df <- data.frame(plotid = id.plots,
                            treat_type = treat.options.random)

trial_grid3 <- trial_grid2 %>% dplyr::left_join(treat_type.df, by = 'plotid')

tm_shape(trial_grid3) + tm_polygons('treat_type') + tm_text('treat_type', size = 0.5)


BaseAppN <- 100 #Currently I am thinking 46 units of N using 100 pounds of urea before planting. 
# n1 <- 120 + 20 #I'm adding 20 pounds to the 2017 rates, becasue Kent wrote, "Based on last year's trial results it sounds like we need 
# #to up the total pounds of N #for each trial. does that sound right to you?
# n2 <- 140 + 20
# n3 <- 150 + 20
# n4 <- 160 + 20
# n5 <- 170 + 20
# n6 <- 180 + 20 
# 
# UREA_1 <- (n1-BaseAppN)/0.46 ##The sidedress I am thinking will be urea also. 
# UREA_2 <- (n2-BaseAppN)/0.46
# UREA_3 <- (n3-BaseAppN)/0.46
# UREA_4 <- (n4-BaseAppN)/0.46
# UREA_5 <- (n5-BaseAppN)/0.46
# UREA_6 <- (n6-BaseAppN)/0.46
N <- c(120,140,160,180)
SEED <- c(28,31,34,37)

farmer.rate <- data.table(N = 160,
                          SEEDRATE = 34)

treatments <- expand.grid(N, SEED) %>% data.table() %>% 
  setnames(.,c('N','SEEDRATE')) %>% 
  rbind(farmer.rate) %>% 
  cbind(treat_type=1:(num_treat+1),.)

treatments[,UREA := round((N - BaseAppN) / 0.46,0)]


#FUNCTION THAT merge internal boundaries of the boundary file and selects the difference between the boundary and the grid (i.e. the border)

#FUNCTION THAT merge internal boundaries of the boundary file and selects the difference between the boundary and the grid (i.e. the border)
st_erase <- function(x, y){
  x.union <- st_union(st_combine(x))
  x.union.sfc <- st_buffer(x.union, 0)
  
  y.union <- st_sfc(st_combine(y))
  y.union.sfc <- st_buffer(y.union, 0)
  
  border.sfc<- st_difference(x.union.sfc, y.union.sfc)
  
}  

delete <- c('drop',  'treat_type_N', 'treat_type_S')
trial_grid3[,delete] <- NULL

border.sfc <- st_erase(wholefield, trial_grid3)
border.sfdf = st_sf(data.frame(buffer = 1,
                               FarmName = farmer.name,
                               FieldName = farm.name,
                               plotid = NA,
                               GRIDY = NA,
                               GRIDX = NA,
                               cent_x = NA,
                               cent_y = NA, 
                               area = st_area(border.sfc),
                               treat_type=(num_treat+1),
                               geom=border.sfc))


app_map <- rbind(border.sfdf, trial_grid3)


tm_shape(app_map) + tm_polygons('treat_type')

app_map2 <- dplyr::left_join(app_map, treatments, by = 'treat_type') %>%
  dplyr::mutate(N_Tgt_Total = round(UREA*(0.46) + BaseAppN,0), #transform again the column to control values
                N = NULL)
names(app_map2)
tm_shape(app_map2) + 
  tm_polygons(c('treat_type')) 

app_map2$plotid <- app_map2$drop <- app_map2$area <- app_map2$cent_x <- app_map2$cent_y <- app_map2$GRIDX <- app_map2$GRIDY <- NULL

whole <- app_map2
whole_td <- tidy(as(whole, "Spatial")) 

whole_td$new_id <- paste('ID',whole_td$id,sep='')
whole_td$id <- whole_td$new_id
whole_td$new_id <- NULL

temp_whole <- whole %>% 
  mutate(id=paste('ID',1:nrow(whole),sep='')) %>% 
  dplyr::select(id,treat_type,UREA,N_Tgt_Total,SEEDRATE)

whole_plot <- left_join(whole_td,temp_whole,by='id') %>% 
  data.table()

summary(whole_plot)
head(whole_plot)



(Nprod.tm <- tm_shape(app_map2) + tm_polygons(col = 'UREA', palette = 'Greys', style = 'cat', format = 'f', digits = '1') +
    tm_compass() +
    tm_grid(alpha = 0.2) +
    tm_legend(
    main.title = paste(farmer.name,farm.name, '2018_UREA_MAP', sep = '_'),
    main.title.position = "left",
    legend.outside=TRUE))
save_tmap(tm = Nprod.tm, filename = paste('./Trial Design Output Shapefiles from R/',farmer.name,'_',farm.name,'2018_UREA_tmap.pdf',sep = ''))


(totalN.tm <- tm_shape(app_map2) + tm_polygons(col = 'N_Tgt_Total', palette = 'Blues', style = 'cat', format = 'f', digits = '1') +
    tm_compass() +
    tm_grid(alpha = 0.2)+
    tm_legend(
      main.title = paste(farmer.name, farm.name, '2018_Total_N_map', sep = '_'),
      main.title.position = "left",
      legend.outside=TRUE))
save_tmap(tm = totalN.tm, filename = paste('./Trial Design Output Shapefiles from R/',farmer.name,'_',farm.name,'2018_Total_N_tmap.pdf',sep = ''))


(SEEDRATE.tm <- tm_shape(app_map2) + tm_polygons(col = 'SEEDRATE', palette = 'Greens', style = 'cat', format = 'f', digits = '1') +
    tm_compass() +
    tm_grid(alpha = 0.2) +
    tm_legend(
      main.title = paste(farmer.name, farm.name, '2018_SEEDRATE_map', sep = '_'),
      main.title.position = "left",
      legend.outside=TRUE))
save_tmap(tm = SEEDRATE.tm, filename = paste('./Trial Design Output Shapefiles from R/',farmer.name,'_',farm.name,'2018_SEEDRATE_tmap.pdf',sep = ''))


(treat_type.tm <- tm_shape(app_map2) + tm_polygons(col = 'treat_type', palette = 'Accent', n = num_treat, style="cont") +
    tm_compass() +
    tm_grid(alpha = 0.2) +
    tm_legend(
      main.title = paste(farmer.name, farm.name, '2018_TREATMENT_map', sep = '_'),
      main.title.position = "left",
      legend.outside=TRUE))
save_tmap(tm = treat_type.tm, filename = paste('./Trial Design Output Shapefiles from R/',farmer.name,'_',farm.name,'2018_TREATMENT_tmap.pdf',sep = ''))

ggplot(data=whole_plot, aes(x=long,y=lat,group=group)) +
  geom_polygon(aes(fill=factor(UREA))) +
  scale_fill_brewer(palette = 'Greys') +
  ggtitle(paste(farmer.name,farm.name, '2018_UREA_MAP', sep = '_'))

ggsave(paste('./Trial Design Output Shapefiles from R/',farmer.name,'_',farm.name,'2018_UREA_MAP.pdf',sep = ''))

ggplot(data=whole_plot, aes(x=long,y=lat,group=group)) +
  geom_polygon(aes(fill=factor(N_Tgt_Total))) +
  scale_fill_brewer(palette = 'Blues') +
  ggtitle(paste(farmer.name,farm.name, '2018_Total_N_MAP', sep = '_'))

ggsave(paste('./Trial Design Output Shapefiles from R/',farmer.name,'_',farm.name,'2018_Total_N_MAP.pdf',sep = ''))

ggplot(data=whole_plot, aes(x=long,y=lat,group=group)) +
  geom_polygon(aes(fill=factor(SEEDRATE))) +
  scale_fill_brewer(palette = 'Greens') +
  ggtitle(paste(farmer.name,farm.name, '2018_SEEDRATE_MAP', sep = '_'))
ggsave(paste('./Trial Design Output Shapefiles from R/',farmer.name,'_',farm.name,'2018_SEEDRATE_MAP.pdf',sep = ''))

#Expand palette to get enough colors:
colourCount <- length(unique(whole_plot$treat_type)) # number of levels
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))

ggplot(data=whole_plot, aes(x=long,y=lat,group=group)) +
  geom_polygon(aes(fill=factor(treat_type))) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(8,"Accent"))(colourCount)) +
  ggtitle(paste(farmer.name,farm.name, '2018_TREATMENT_MAP', sep = '_'))

ggsave(paste('./Trial Design Output Shapefiles from R/',farmer.name,'_',farm.name,'2018_TREATMENT_MAP.pdf',sep = ''))

colourCount <- length(unique(whole_plot$block)) # number of levels

# ggplot(data=whole_plot, aes(x=long,y=lat,group=group)) +
#   geom_polygon(aes(fill=factor(block))) +
#   scale_fill_manual(values = colorRampPalette(brewer.pal(8,"Accent"))(colourCount)) +
#   ggtitle('Nelson-Wirth_2018_BLOCK MAP')
# ggsave('./Trial Design Output Shapefiles/Nelson-Wirth_2018_BLOCK MAP.pdf')

app_map.wgs <- st_transform(app_map2,'+proj=longlat +datum=WGS84 +no_defs')

st_write(app_map.wgs,paste('./Trial Design Output Shapefiles from R/',farmer.name,'_',farm.name,'_2018_appmap.shp',sep = ''))

