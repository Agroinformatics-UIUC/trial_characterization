## ------------------------------------------------------------------------
# Import packages
library(sf)
library(DIFMR)
library(tmap)

## ------------------------------------------------------------------------
trial_params <- list(
  farm_name = "Sehn_R4",
  rseed = 123456,
  plot_width = ft_to_meters(120),
  plot_length = 72,
  subplot_length = 18,
  n_treats =5,
  type = "NR",
  expand_rates = TRUE,
  nitrogen_treat_rates = c(120, 140,150,160,180), #in kgN/ac to apply in trial (total)
  nitrogen_sq = 140, #status quo (total N kgN/ac)
  nitrogen_multip = 1/0.28/10.67, #to convert from N to product
  nitrogen_subtract = 108, #base N in kgN/ac 
  product_name = 'uan28_gal_ac' #product that the farmer will use for the trial
)

attach(trial_params)

nitrogen_treat_rates <- nitrogen_treat_rates - nitrogen_subtract
nitrogen_sq <- nitrogen_sq - nitrogen_subtract
# Keep results consistent over executions:
set.seed(rseed)

print(farm_name)
## ------------------------------------------------------------------------
# Read the data into R:
main_dir <- "./data"
farm_dir <- file.path(main_dir, farm_name)

field <- read_sf(file.path(farm_dir, "Sehn_R4_2020 boundary.shp"))
st_crs(field) <- 4326
field <- st_utm(field)

trial <- sf::st_buffer(field, -plot_width) %>% dplyr::select(geometry)
tm_shape(field) + tm_borders()+
  tm_shape(trial) + tm_polygons(col='red')

border <- sf::st_difference(field, trial)%>% dplyr::select(geometry)
trial$Type <- "Trial"
border$Type <- "Headland"
field <- rbind(trial, border)["Type"]

tm_shape(field) + tm_polygons('Type')

# write_sf(field, "./data/Sehn_R4/Sehn_R4_boundary_headland_byhand.shp") #edit in QGIS
# field <- read_sf("./data/Sehn_R4/Sehn_R4_boundary_headland_byhand.shp")

ab_line <- read_sf(file.path(farm_dir, "Sehn_R4 AB-line.shp")) #if we use an abline that is right in the border, should we save a new one offset?
ab_line <- st_utm(ab_line)

ab_line2 <- read_sf(file.path(farm_dir, "Sehn_R4_2019 ab_line_over60feet.shp"))
ab_line2 <- st_utm(ab_line2)

tm_shape(field)+tm_borders()+
  tm_shape(ab_line2)+tm_lines(col='red')

# rx <- read_sf(file.path(farm_dir, "rx_nitrogen.gpkg"))
# rx <- st_utm(rx)
# rx <- rx[!is.na(st_is_valid(rx)),]


# Expand the abline to ensure whole field coverage:
max_dist <- calc_distance(field, max = TRUE)
ab_line <- expand_lines(ab_line, max_dist)

# Creates a field with dissolved internal subdivisions:
field_ext <- st_buffer(st_buffer(st_combine(field), 1), -1)

# plot(st_geometry(field_ext))
# plot(st_geometry(ab_line), lwd = 2, col = "red", add = TRUE)

## ------------------------------------------------------------------------
# Creates the final grid for the Nitrogen and Seed rate maps:
path_lines <- parallel_lines(ab_line, plot_width, field_ext, 0.5, max_dist, F)
tm_shape(path_lines)+tm_lines()+tm_shape(ab_line)+tm_lines(col = 'red')
path_pols <- get_coverage(path_lines, plot_width, field_ext)
tm_shape(path_lines)+tm_lines(col='green')+tm_shape(path_pols)+tm_borders()+tm_shape(ab_line)+tm_lines(col = 'red')
grid_pols <- get_subpols(path_lines, path_pols, plot_width, subplot_length)
grid_pols <- grid_pols[grid_pols$Area > 10, ]

tm_shape(grid_pols)+tm_borders()+tm_shape(ab_line2)+tm_lines(col = 'red')

ov <- st_over(st_centroid(st_geometry(grid_pols)), field)
grid_pols$Type <- field$Type[ov]
grid_pols$Type[is.na(ov)] <- "Headland"
tm_shape(field)+tm_polygons('Type')+tm_shape(ab_line2)+tm_lines(col = 'red')

## ------------------------------------------------------------------------
# In case any polygon is partially over the headland, remove it from the useful area:
area_crit <- 0.85 * subplot_length * plot_width
grid_pols$Type[grid_pols$Area < area_crit] <- "Headland"
spols <- grid_pols[grid_pols$Type == "Trial", ]

## ------------------------------------------------------------------------
# Group the small polygons according to the plot length to make the experimental units:
trial_pols <- group_subpols(spols, plot_length, subplot_length, 10,
                            fixed_length = F, invert_side = T
)

# Check the assignement of row an col ids, needed for allocating the blocks:
plot(trial_pols["pcol"])
plot(trial_pols["prow"])

# In this case we reverse the side where the blocking starts:
#trial_pols$prow <- (1 + max(trial_pols$prow)) - trial_pols$prow

## ------------------------------------------------------------------------
# Apply the blocking:
trial_pols <- get_block_ids(trial_pols, n_treats)
plot(trial_pols["pcol"])
plot(trial_pols["prow"])
plot(trial_pols["Block"])

## ------------------------------------------------------------------------
# Randomly assign the rates within each block and
# get the factor levels to the experimental units:
trial_rates <- design_treat_graeco(trial_pols, n_treats, expand_rates = expand_rates)
trial_pols$NR <- nitrogen_treat_rates[as.numeric(trial_rates$F2)]

# Check the distribution of the rates:
table(trial_pols$NR)

plot(trial_pols["NR"])

## ------------------------------------------------------------------------
# Transfer the design information to the small polygons and save the files:
ov <- st_over(st_centroid(st_geometry(grid_pols)), trial_pols)
# ov_rx <- st_over(st_centroid(st_geometry(grid_pols)), rx)

grid_pols$prow <- trial_pols$prow[ov]
grid_pols$pcol <- trial_pols$pcol[ov]
grid_pols$Block <- trial_pols$Block[ov]
plot(grid_pols["Block"])


grid_pols$NR <- trial_pols$NR[ov]
sq_crit = grid_pols$NR == nitrogen_sq
grid_pols$Type[sq_crit] = 'Status Quo'
grid_pols$NR[is.na(grid_pols$NR)] <- nitrogen_sq
# sq_crit = grid_pols$NR == nitrogen_sq & !is.na(rx$Target_Rat[ov_rx])
grid_pols$NR <- grid_pols$NR * nitrogen_multip
# grid_pols$NR[sq_crit] <- rx$Target_Rat[ov_rx][sq_crit]

plot(grid_pols["NR"])
plot(grid_pols["Type"])


tm_shape(grid_pols) + tm_fill("NR", style = 'cat') + tm_layout(legend.outside = TRUE)+tm_shape(ab_line2)+tm_lines(col='red')
tmap_save(filename = file.path(farm_dir, 'Nitrogen.png'))

tm_shape(grid_pols) + tm_fill("Type") + tm_layout(legend.outside = TRUE)
tmap_save(filename = file.path(farm_dir, 'Nitrogen_Type.png'))


grid_pols_ll <- st_transform(grid_pols, 4326) %>% dplyr::select(NR)
trial_file <- file.path(farm_dir, paste0(farm_name, "_Nitrogen_Trial_design.gpkg"))
write_sf(grid_pols_ll, trial_file)

#Save the file as shp for farmer
names(grid_pols_ll)[names(grid_pols_ll) == "NR"] <- product_name
trial_file <- file.path(farm_dir, paste0(farm_name, "_Nitrogen_Rate.shp"))

write_sf(grid_pols_ll, trial_file)

## ------------------------------------------------------------------------
# Creates a list with all data and save in the RDS format:
farm_data <- list(
  field = field,
  ab_line = ab_line,
  trial_params = trial_params,
  trial_design = grid_pols_ll
)
farm_data_file <- file.path(farm_dir, paste0(farm_name, "_nitrogen_data.rds"))
saveRDS(farm_data, farm_data_file)

## ------------------------------------------------------------------------
