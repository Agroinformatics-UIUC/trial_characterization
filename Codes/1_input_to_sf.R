rm(list=ls())

result_folder <- "~/example_characterization" #set to full path to the folder that will hold the characterization results 
#(you can arbitrarily make a new one, ex: soy_trials of what will become soy_trials/trial_characterization_box)

result_folder <- "~/Documents/example_characterization"

setwd(result_folder) 
codes_folder <-"~/trial_characterization" #set to full path to the folder with the codes 
#(unless you change the name, that's trial_characterization of trial_characterization/Codes)

codes_folder <-"~/Documents/GitHub/trial_characterization"

source(paste0(codes_folder,'/Codes/R.libraries.R'))
# ---------------------------------------------------------------------------------------------
# Load the input file, with coordinates and planting dates

#trials_dt <- data.table::fread('./trial_characterization_box/Data/input.csv') 
if(!dir.exists(result_folder)){dir.create(result_folder, recursive = TRUE)}
if(!dir.exists('./trial_characterization_box/')){dir.create('./trial_characterization_box/', recursive = TRUE)}
if(length(list.files(result_folder))==0) {print(paste0("Place the characterization input .csv in ",result_folder))}
charact_dt <- list.files(result_folder) %>% .[grep(".csv",.)]
if(any(grep(charact_dt,"input.csv")==TRUE)){
  charact_dt<-"input.csv"
} else {charact_dt<-charact_dt[1]}
trials_dt <- data.table::fread(paste0(result_folder,"/",charact_dt)) 

trials_dt[,Crop := tolower(Crop)]

trials_sf = st_as_sf(trials_dt, coords = c("Longitude", "Latitude"), 
                     crs = 4326, agr = "constant")

us_states4326.sf <- st_transform(us_states, 4326) %>% dplyr::select(state = NAME, region = REGION)


(plot1 <- tm_shape(us_states4326.sf) + tm_polygons()   +
  tm_shape(trials_sf) + tm_dots(size = 0.2))

if(!file.exists('./trial_characterization_box/output')){dir.create('./trial_characterization_box/output', recursive = TRUE)}

tmap_save(plot1, 
          filename = "./trial_characterization_box/output/map.pdf", height = 8, width = 6)  

# Intersect with US map, in case there are some trials in other countries. We may not have SSURGO and DAYMET data in those
trials_sf <- st_intersection(trials_sf, us_states4326.sf)


# Add coordinates and id_trial (unique identifier for trial x year)
trials_sf <- cbind(trials_sf, st_coordinates(trials_sf)) %>% 
        mutate(id_trial = row_number())

# Find unique fields and called them location. A trial is an year x loc combination

locs_sf <- st_difference(trials_sf) %>% mutate(id_loc = row_number()) %>% dplyr::select(id_loc, X,Y)
  

# Add the id_loc to the trials
trials_sf <- st_join(trials_sf, dplyr::select(locs_sf, id_loc))

trials_sf <- trials_sf %>% dplyr::mutate(planting_date = as.Date(Planting, format = "%m/%d/%Y"),
                                         day = format(planting_date,"%d"),
                                         month = tolower(format(planting_date,"%b")),
                                         year = format(planting_date,"%Y")) 

if(!file.exists('./trial_characterization_box/rds_files')){dir.create('./trial_characterization_box/rds_files', recursive = TRUE)}


saveRDS(trials_sf, './trial_characterization_box/rds_files/trials_sf.rds')
saveRDS(locs_sf, './trial_characterization_box/rds_files/locs_sf.rds')
