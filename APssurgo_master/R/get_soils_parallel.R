#===================================
# prepare clusters
#===================================


# no_cores <- detectCores() - 4
# cl <- makeCluster(no_cores,type='SOCK')

#===================================
# parallelized simulations 
#===================================

# loc_n=86

get_soils <- function(loc_n){
  packages_need <- c('sf', 'soilDB', 'dplyr', 'data.table')
  lapply(packages_need, require, character.only = TRUE)
  
  one_loc_sf <- locs_sf[loc_n,]
  
  possibleError <- tryCatch({
    
    ssurgo_pol <- SDA_spatialQuery(one_loc_sf)
    
    ssurgo_sf <- st_as_sf(cbind(ssurgo_pol["mukey"], one_loc_sf))
  
    if(any(st_is_valid(ssurgo_sf) == FALSE)) {ssurgo_sf<-st_make_valid(ssurgo_sf)}
    
    field_soils_tmp <- ssurgo_sf

  }, error = function(e) e)
  
  #REAL WORK:if there is no error
  if(!inherits(possibleError, "error")){
    return(field_soils_tmp)
  } else {
    return()
  }
}

# source('./vr_value/Codes/functions_vr.R')

# keep <- c('keep', 'one_tile_sf','get_soils', 'st_utm')

# clusterExport(cl, varlist = keep, envir=environment())

# results_list <- parLapply(cl, 1:nrow(one_tile_sf), function(x) get_soils(x))

# Obtain soils
results_list <- list()
for(loc_n in 1:nrow(locs_sf)){
  
  #rerun until the soil is obtained (it fails often)
  uncompleted <- T
  while(uncompleted){
    print(paste0(round(loc_n/nrow(locs_sf)*100,2),"%"))
    results_list[[loc_n]] <- get_soils(loc_n)
    uncompleted <- length(results_list) < loc_n
  }
}

results_list_clean <- results_list[vapply(results_list, Negate(is.null), NA)]

soils_sf <- do.call(what = base::rbind, args = results_list_clean)
rownames(soils_sf) <- 1:nrow(soils_sf)

saveRDS(soils_sf, './trial_characterization_box/rds_files/soils_sf.rds') 
# stopCluster(cl)
  