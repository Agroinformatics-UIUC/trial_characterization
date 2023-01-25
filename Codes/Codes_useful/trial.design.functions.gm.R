make_grids <- function(ab_line){
  
  #===================================
  # Define the long and short vectors
  #===================================
  ab_1 <- st_geometry(ab_line)[[1]][1,]
  ab_2 <- st_geometry(ab_line)[[1]][2,]
  
  diffX <- ab_1[1] - ab_2[1]
  diffY <- ab_1[2] - ab_2[2]
  startingpoint.ymax <- ab_1[2] - bbox_field['ymax']
  startingpoint.ymin <- ab_1[2] - bbox_field['ymin']
  
  if(abs(diffX) > abs(diffY)){#long_in it is EW or WE
    if(diffX < 0){
      long_in <- 'WE'
      if(startingpoint.ymin > startingpoint.ymax){
        short_in <- 'NS'
      }else{short_in <- 'SN'}
    }else{long_in <- 'EW'}
  }else{
    if(diffY < 0){
      long_in <- 'SN'
    }else{long_in <- 'NS'}
  }
  
  
  
  
  #--------------------------
  # find the origin, end point, and the rotation matrix
  #--------------------------
  if (long_in=='SN'){ # if the plot is long in SN direction
    #--- find the origin and end points ---#
    if (ab_1[2]>=ab_2[2]){
      origin <- ab_2
      end_point <- ab_1
    } else{
      origin <- ab_1
      end_point <- ab_2
    }
    #--- find rotation vector ---#
    #if (short_in=='WE'){
    #rotate_mat <- matrix(c(cos(-pi/2),sin(-pi/2),-sin(-pi/2),cos(-pi/2)),nrow=2)
    #} else{
    #rotate_mat <- matrix(c(cos(pi/2),sin(pi/2),-sin(pi/2),cos(pi/2)),nrow=2)
    #}
    if (short_in=='WE'){
      rotate_mat <- matrix(c(cos(-3.000*pi/2),sin(-3.000*pi/2),-sin(-3.000*pi/2),cos(-3.000*pi/2)),nrow=2)
    } else{
      rotate_mat <- matrix(c(cos(3.000*pi/2),sin(3.000*pi/2),-sin(3.000*pi/2),cos(3.000*pi/2)),nrow=2)
    }
  } else if (long_in=='NS') {
    #--- find the origin and end points ---#
    if (ab_1[2]>=ab_2[2]){
      origin <- ab_1
      end_point <- ab_2
    } else{
      origin <- ab_2
      end_point <- ab_1
    }
    #--- find rotation vector ---#
    if (short_in=='WE'){
      rotate_mat <- matrix(c(cos(3.000*pi/2),sin(3.000*pi/2),-sin(3.000*pi/2),cos(3.000*pi/2)),nrow=2)
    } else{
      rotate_mat <- matrix(c(cos(-3.000*pi/2),sin(-3.000*pi/2),-sin(-3.000*pi/2),cos(-3.000*pi/2)),nrow=2)
    }
  } else if (long_in=='WE') {
    #--- find the origin and end points ---#
    if (ab_1[1]>=ab_2[1]){
      origin <- ab_2
      end_point <- ab_1
    } else{
      origin <- ab_1
      end_point <- ab_2
    }
    #--- find rotation vector ---#
    if (short_in=='SN'){
      rotate_mat <- matrix(c(cos(-3.000*pi/2),sin(-3.000*pi/2),-sin(-3.000*pi/2),cos(-3.000*pi/2)),nrow=2)
    } else if (short_in=='NS'){
      rotate_mat <- matrix(c(cos(3.000*pi/2),sin(3.000*pi/2),-sin(3.000*pi/2),cos(3.000*pi/2)),nrow=2)
    }
  } else if (long_in=='EW'){
    #--- find the origin and end points ---#
    if (ab_1[1]>=ab_2[1]){
      origin <- ab_1
      end_point <- ab_2
    } else{
      origin <- ab_2
      end_point <- ab_1
    }
    
    #--- find rotation vector ---#
    if (short_in=='SN'){
      rotate_mat <- matrix(c(cos(3.000*pi/2),sin(3.000*pi/2),-sin(3.000*pi/2),cos(3.000*pi/2)),nrow=2)
    } else if (short_in=='NS'){
      rotate_mat <- matrix(c(cos(-3.000*pi/2),sin(-3.000*pi/2),-sin(-3.000*pi/2),cos(-3.000*pi/2)),nrow=2)
    }
  }
  
  #--------------------------
  # Find the long and short vectors
  #--------------------------
  #--- long vector ---#
  long_vec <- end_point-origin 
  
  #--- short vector ---#
  short_vec <- rotate_mat %*% long_vec
  
  #--------------------------
  # normalize the vectors
  #--------------------------
  vector_len <- sqrt(long_vec[1]^2+long_vec[2]^2)
  long_norm <- long_vec/vector_len*plot_length_meter 
  short_norm <- (short_vec/vector_len*plot_width_meter) %>% as.vector()
  
  #===================================
  # Create grids
  #===================================
  
  
  #--- how many rows and columns ---#
  if (long_in %in% c('SN','NS')){
    num_rows <- ceiling((bbox_field['ymax']-bbox_field['ymin'])/plot_length_meter)
    num_cols <- ceiling((bbox_field['xmax']-bbox_field['xmin'])/plot_width_meter)
  } else if (long_in %in% c('WE','EW')){
    num_rows <- ceiling((bbox_field['ymax']-bbox_field['ymin'])/plot_width_meter)
    num_cols <- ceiling((bbox_field['xmax']-bbox_field['xmin'])/plot_length_meter)
  }
  
  #--------------------------
  # Create grids
  #--------------------------
  
  all_polygons_ls <- list()
  if (long_in %in% c('SN','NS')){ # if the applicator moves NS or SN
    if (short_in %in% c('EW')){
      for (i in 1:num_cols){
        #i=1
        if(i==1){
          col_start <- starting_point_A
        } else{
          col_start <- st_geometry(all_polygons_ls[[i-1]])[[1]][[1]][4,]
        }
        
        col_polygons_ls <- list()
        for (j in 1:num_rows){
          #j=1
          if(j==1){
            point_1 <- col_start
          } else{
            point_1 <- col_polygons_ls[[j-1]][[1]][2,]
          }
          
          point_2 <- point_1+long_norm
          point_3 <- point_2-short_norm
          point_4 <- point_3-long_norm
          
          p_temp <- rbind(point_1,point_2,point_3,point_4,point_1) %>%
            list() %>%
            st_polygon()
          col_polygons_ls[[j]] <- p_temp
        }
        
        
        all_polygons_ls[[i]] <-
          st_sf(
            plotid=(1+(i-1)*num_rows):(i*num_rows),
            GRIDY = (1:num_rows),
            GRIDX = i,
            st_sfc(col_polygons_ls)
          )
      }
    }else if (short_in %in% c('WE')){
      for (i in 1:num_cols){
        #i=2
        if(i==1){
          col_start <- starting_point_A
        } else{
          col_start <- st_geometry(all_polygons_ls[[i-1]])[[1]][[1]][4,]
        }
        
        col_polygons_ls <- list()
        for (j in 1:num_rows){
          #j=1
          if(j==1){
            point_1 <- col_start
          } else{
            point_1 <- col_polygons_ls[[j-1]][[1]][2,]
          }
          
          point_2 <- point_1+long_norm
          point_3 <- point_2-short_norm
          point_4 <- point_3-long_norm
          
          p_temp <- rbind(point_1,point_2,point_3,point_4,point_1) %>%
            list() %>%
            st_polygon()
          col_polygons_ls[[j]] <- p_temp
        }
        
        
        all_polygons_ls[[i]] <-
          st_sf(
            plotid=(1+(i-1)*num_rows):(i*num_rows),
            GRIDY = (1:num_rows),
            GRIDX = i,
            st_sfc(col_polygons_ls)
          )
      }
    }}else if(long_in %in% c('WE','EW')){ # if the applicator moves WE or EW
      if (short_in %in% c('SN')){
        for (i in 1:num_rows){
          # i=2
          if(i==1){
            col_start <- starting_point_A
          } else{
            col_start <- st_geometry(all_polygons_ls[[i-1]])[[1]][[1]][2,]
          }
          
          col_polygons_ls <- list()
          for (j in 1:num_cols){
            #j = 2
            if(j==1){
              point_1 <- col_start
            } else{
              point_1 <- col_polygons_ls[[j-1]][[1]][4,]
            }
            point_2 <- point_1+short_norm
            point_3 <- point_2+long_norm
            point_4 <- point_3-short_norm
            
            # point_2 <- point_1+long_norm
            # point_3 <- point_2+short_norm
            # point_4 <- point_3-long_norm
            
            p_temp <- rbind(point_1,point_2,point_3,point_4,point_1) %>%
              list() %>%
              st_polygon()
            col_polygons_ls[[j]] <- p_temp
          }
          
          all_polygons_ls[[i]] <-
            st_sf(
              plotid=(1+(i-1)*num_cols):(i*num_cols),
              GRIDY = i,
              GRIDX = (1:num_cols),
              st_sfc(col_polygons_ls)
            )
        }
        
      }else if(short_in %in% c('NS')){ 
        for (i in 1:num_rows){
          # i=2
          if(i==1){
            col_start <- starting_point_A
          } else{
            col_start <- st_geometry(all_polygons_ls[[i-1]])[[1]][[1]][1,]
          }
          
          col_polygons_ls <- list()
          for (j in 1:num_cols){
            #j = 2
            if(j==1){
              point_2 <- col_start
            } else{
              point_2 <- col_polygons_ls[[j-1]][[1]][3,]
            }
            point_1 <- point_2+short_norm
            point_3 <- point_2+long_norm
            point_4 <- point_3+short_norm
            
            # point_2 <- point_1+long_norm
            # point_3 <- point_2+short_norm
            # point_4 <- point_3-long_norm
            
            p_temp <- rbind(point_1,point_2,point_3,point_4,point_1) %>%
              list() %>%
              st_polygon()
            col_polygons_ls[[j]] <- p_temp
          }
          
          all_polygons_ls[[i]] <-
            st_sf(
              plotid=(1+(i-1)*num_cols):(i*num_cols),
              GRIDY = i,
              GRIDX = (1:num_cols),
              st_sfc(col_polygons_ls)
            )
        }
      }
    }
  #--- combine all the grids ---#
  all_grids <- do.call(rbind,all_polygons_ls)
  
  return(all_grids)
}

displace.starting.point <- function(distance){ 
  cat <- starting_point_A - starting_point_B
  hip <- sqrt(sum(cat ^ 2))
  moveY <- distance * cat[2] / hip
  moveX <- distance * cat[1] / hip
  starting_point_A_new <- c(starting_point_A[1] - moveX, 
                            starting_point_A[2] - moveY)
return(starting_point_A_new)}
