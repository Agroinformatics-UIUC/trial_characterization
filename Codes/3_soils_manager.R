rm(list=setdiff(ls(), c("codes_folder", "result_folder")))
if(!exists("result_folder")) {
  result_folder <- "~/example_characterization"
  setwd(result_folder) 
} 
if(!exists("codes_folder")) {codes_folder <-'~/trial_characterization'} 

# ---------------------------------------------------------------------------------------------
# Load libraries and functions 
source(paste0(codes_folder,'/Codes/R.libraries.R'))

# Load the locs
locs_sf <- readRDS('./trial_characterization_box/rds_files/locs_sf.rds')

#---------------------------------------------------------------
# Step 1 Get the soils for each field
source(paste0(codes_folder, '/APssurgo_master/R/get_soils_parallel.R'))
soils_sf <- readRDS('./trial_characterization_box/rds_files/soils_sf.rds')


#---------------------------------------------------------------  
# Step 2 get the horizons information
source(paste0(codes_folder, '/APssurgo_master/R/get_horizons_parallel.R'))

horizons_dt <- readRDS("./trial_characterization_box/rds_files/horizons_dt.rds")

# Clean the soils not available in SSURGO

all(soils_sf$mukey %in% horizons_dt$mukey)



