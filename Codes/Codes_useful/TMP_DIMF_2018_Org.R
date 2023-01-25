#######################################################################
########################          Preamble         ####################
##                                                                   ##
## Tile: DFIM Scripts                                                ##
## Author: Rodrigo Gonçalves Trevisan        DATE: 28/08/2018        ##
## Contact: rodrigo7@illinois.edu                                    ##
##                                                                   ##
#######################################################################

#######################################################################
##########################       Observations     #####################


#######################################################################
##########################       Libraries    #########################

require(sf)


#######################################################################
##########################      Functions     #########################
coordTransform = function(field){
  long2UTM = function(long) {(floor((long + 180)/6) %% 60) + 1}
  if (is.na(st_crs(field))) {
    st_crs(field) = st_crs(4326)
  }else{
    field = st_transform(field, st_crs(4326))
  }
  utmzone = long2UTM(mean(st_bbox(field)[c(1,3)]))
  projutm <<- st_crs(paste0("+init=epsg:326", utmzone))
  field = st_transform(field, projutm)
  return(field)
}

get_values = function(obj, names_dic){
  coln = match(names_dic, names(obj))
  colname = coln[!is.na(coln)][1]
  if (sum(!is.na(coln)) > 1){
    print(paste0('Two Column names matched! Using: ', names(obj)[colname]))
  }
  if(!is.na(any(coln))){
    v = obj[[colname]]
  }else{
    print('Column not found!')
    v = rep(NA, nrow(obj))
  }
  return(v)
}

st_over = function(x,y) {
  sapply(st_intersects(x,y), function(z) if (length(z)==0) NA_integer_ else z[1])
}

#######################################################################
##########################        Config      #########################
# Obs: I would avoid using spaces in path
data_dir = "C:/Users/rodrigo7/Documents/Projects/GWR/tmp"
farms = list.dirs(data_dir, recursive = FALSE, full.names = FALSE)
farms = setdiff(farms, "Wendte_Laue")
s_dir = 'C:/Users/rodrigo7/Documents/DIFM/2018/Database'


# If the column is not found, because it has a name
# that is not included in the respective list,
# include it here and it should work:

yield_cols = c("Field3", "Dry_Yield", "Dry_Yield_", "DryYield", "Mass_Yield",
               "VRYIELDVOL", "Yield__Dry", "YieldMas", "Yld_Mass_D")
moist_cols = c("HarvestM", "Moisture", "Moisture__")
elev_cols = c("Elevatio", "Elevation", "Elevation_")
sr_cols = c("SEEDRATE",  "Seed", "PLANTS", "SEEDRAT", "seed_rate", "TgtSeed")
n_cols = c("Nrate", "NRATE", "NRATE_Gal3", "NH3_rate",
           "GAL_28", "GAL32", "GAL8020", "PoundsUrea", "TgtNTotl","N_Tgt_T")

appsr_cols = c("Rt_Apd_Ct_", 'VRSEEDRATE')
appnr_cols = c("Rt_Apd_Liq", 'Rate_Monta', 'Rt_Apd_Ms_', 'VRAPPRATEV')


f = 4
for(f in 4:length(farms)){
  farm = farms[f]
  print(farm)

  farm_name = paste0('Field', stringr::str_pad(f, 2, pad = "0"))
  farm_dir = file.path(data_dir, farm)
  shp_files = list.files(farm_dir, full.names = TRUE,
                         pattern = '.shp|.gpkg',
                         recursive = TRUE)
  #######################################################################
  ########################      Trial Layout      #######################
  print('Trial Layout')

  # This is the orignal trial layout:
  trial_shp = grep('Trial', shp_files, value = TRUE)
  trial = read_sf(trial_shp)
  trial = coordTransform(trial)
  trial = dplyr::distinct(trial, geom, .keep_all = TRUE)
  trial$ID = 1:nrow(trial)

  trial$SRPlan = get_values(trial, sr_cols)
  trial$NPlan = get_values(trial, n_cols)


  trial = trial[c('ID','SRPlan','NPlan')]
  t_file = file.path(s_dir, farm_name, 'Trial_Design.gpkg')
  suppressWarnings(dir.create(dirname(t_file)))
  write_sf(st_transform(trial, 4326), t_file)

  #######################################################################
  ########################       Yield Data       #######################
  print('Yield Data')

  #    Shapefile with yield data, collumn mapping necessary,
  # minimum filtering is included but can be largelly improved:
  yield_shp = grep('Yield', shp_files, value = TRUE)[1]
  yield = read_sf(yield_shp)
  yield = st_zm(yield)
  yield = coordTransform(yield)
  yield$ID = 1:nrow(yield)
  print(nrow(yield))

  yield$Yield = get_values(yield, yield_cols)
  yield$Moisture = get_values(yield, moist_cols)
  yield$Elev = get_values(yield, elev_cols)

  # Attempts to convert to Mg/ha-1
  avg = median(yield$Yield)
  cfct = 1
  if(avg < 5000){
    cfct = 62.77
  }
  if(avg < 20){
    cfct = 1000
  }
  yield$Yield = yield$Yield * cfct
  print(median(yield$Yield))


  if(farm == "Hord_F17"){
    yield$Yield = runmed(yield$Yield, 5)
    yield = yield[1:nrow(yield) %% 5 == 0,]
  }


  yield$PolID = st_over(yield, trial)
  yield = yield[c('ID','PolID','Yield', 'Moisture', 'Elev')]
  yield = yield[!is.na(yield$PolID),]
  t_file = file.path(s_dir, farm_name, 'Trial_Yield.gpkg')
  write_sf(st_transform(yield, 4326), t_file)

  #######################################################################
  ########################      As Applied      #######################
  print('As Planted')

  # This is the orignal trial layout:

  as_plant_f = grep(shp_files, pattern = 'Planted', value = TRUE)

  obj = read_sf(as_plant_f)
  obj = coordTransform(obj)

  obj$SR = get_values(obj, appsr_cols)
  hist(obj$SR)

  obj$ID =1:nrow(obj)
  obj = obj[c('ID','SR')]
  t_file = file.path(s_dir, farm_name, 'As_Planted.gpkg')
  write_sf(st_transform(obj, 4326), t_file)

  print('As Applied')

  as_app_f = grep(shp_files, pattern = 'Applied', value = TRUE)

  obj = read_sf(as_app_f)
  obj = coordTransform(obj)


  obj$NR = get_values(obj, appnr_cols)
  hist(obj$NR)

  obj$ID =1:nrow(obj)
  obj = obj[c('ID','NR')]
  t_file = file.path(s_dir, farm_name, 'As_Applied.gpkg')
  write_sf(st_transform(obj, 4326), t_file)

}



##                                                                   ##
##########################        END         #########################
##                                                                   ##
