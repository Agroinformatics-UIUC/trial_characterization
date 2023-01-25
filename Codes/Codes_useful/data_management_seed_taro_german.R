######################################
# Data management of seed rate 
######################################
# written by Taro Mieno on 12/18/2017

#===================================
# Preparation
#===================================
setwd('/Users/tmieno2/Box Sync/ResearchProjects/DIFM')
setwd('C:/Users/germa/Box Sync/DIFM_Data/DIFM Data by Variable/Taro cleaning and analysis code')

#--- packages ---#
source('C:/Users/germa/Box Sync/DIFM_Data/DIFM Data by Variable/Taro cleaning and analysis code/Codes/library.R')

library(RColorBrewer)
library(tmap)
library(cluster)

#===================================
# Data import and some transformation
#===================================
#--------------------------
# Import the original data
#--------------------------

seed <- st_read(dsn='./Data/ILS_Barton_37/37_field/ILS_37_asapplied_2017','ILS_AsApplied_37_Seed_2017')   

# plot(seed)
#--------------------------
# Reduce the dimension
#--------------------------
# Recordings of 24 nozzles occur at the same time 
# 24 recordings are summarized as they contain exactly the same information on key variables 
seed_reduced <- seed %>% 
	setnames(names(.),tolower(names(.))) %>% # variable names to lower cases
	st_transform(26914) %>% # change to UTM so feet can be used easily  
	cbind(.,st_coordinates(.)) %>% # add coordinates 
	data.table() %>% # conversion to data table
  	.[,.(distance=mean(distance),X=mean(X),Y=mean(Y),seed_rate=mean(seedra05),seed_target=mean(seedra06)),by=.(time,heading)] %>% 
	.[,time_stamp:=parse_date_time(time,'%d/%m/%y %I:%M:%S %p')] %>% # recognize time by R
	.[heading>=180,heading:=heading-360] %>% # this resolves discontinuity around 0 (and 360) 
	.[,id:=1:nrow(.)] # add ids

# ggplot() +
# 	geom_point(data=seed_reduced[seed_rate>20000,],aes(y=Y,x=X,color=seed_rate),size=0.3) +
# 	scale_color_gradient(low='red',high='green') 

#===================================
# Grouping of heading to get rid of the irrelevant records
#===================================
#--------------------------
# Difference from the previous heading
#--------------------------
#--- find the difference of heading ---#
# Notes:
# 1) first-difference of heading should be close to 0 if you are going straight
# 2) -999 is a large enough number that will be eliminated later

seed_reduced[,head_dif:=c(-999,abs(diff(heading)))]  

# ggplot(data=seed_reduced[head_dif<2,]) +
# 	geom_point(aes(x=id,y=heading)) 

#--------------------------
# Find the mode of heading (direction) and find the minimum of deviations from the modes
#--------------------------
install.packages('modeest')
library(modeest)

mode_1 <- asselin(seed_reduced[,heading])
mode_2 <- asselin(seed_reduced[heading > (mode_1+30) | heading < (mode_1-30),heading])

#--- deviations ---#
seed_reduced[,head_dev_1:=abs(heading-mode_1)]
seed_reduced[,head_dev_2:=abs(heading-mode_2)]
seed_reduced[,head_dev:=pmin(head_dev_1,head_dev_2)]

#--------------------------
# Find which to use based on heading deviation and heading FD
#--------------------------
seed_reduced[,keep:=ifelse(head_dev<2 & head_dif<2,1,0)]


#===================================
# Create plots and subplots by seed rate
#===================================
# Restrict the data to those we will use
seed_use <- seed_reduced[keep==1,]

# ggplot(data=seed_reduced[keep==1,]) +
# 	geom_point(aes(x=id,y=heading)) 

#--------------------------
# Grouping of yield sequence by and within heading  
#--------------------------
#--- grouping by heading ---#
seed_use[head_dev_1<head_dev_2,which_mode:=1]
seed_use[head_dev_2<head_dev_1,which_mode:=2]

#++++++++++++++++
# grouping within mode 1
#++++++++++++++++
# It should take time to move from one row to the next row
seed_use_1 <- seed_use[which_mode==1,] %>% 
	.[,time_dif:=c(0,diff(time))] # first difference of time

break_ls <- c(1,which(seed_use_1[,abs(time_dif)>10])) # where time breaks occur
break_len <- length(break_ls)

seed_use_1[,group:=0]
for (i in 1:break_len){
	if(i<break_len){
		seed_use_1[break_ls[i]:(break_ls[i+1]-1),group:=i]
	} else{
		seed_use_1[break_ls[i]:nrow(seed_use_1),group:=i]
	}
}

# seed_use_1[time_dif<0,]
# seed_use_1[id %in% 25700:26599,]

#++++++++++++++++
# grouping within mode 2
#++++++++++++++++
# It should take time to move from one row to the next row
seed_use_2 <- seed_use[which_mode==2,] %>% 
	.[,time_dif:=c(0,diff(time))]

break_ls <- c(1,which(seed_use_2[,abs(time_dif)>10]))
break_len <- length(break_ls)

seed_use_2[,group:=0]
for (i in 1:break_len){
	if(i<break_len){
		seed_use_2[break_ls[i]:(break_ls[i+1]-1),group:=i]
	} else{
		seed_use_2[break_ls[i]:nrow(seed_use_2),group:=i]
	}
}

#++++++++++++++++
# combine the two
#++++++++++++++++
seed_use_final <- rbind(seed_use_1,seed_use_2) %>% 
	setorder(time) %>% 
	.[,mode_group:=paste(which_mode,group,sep='_')] %>% # identifier of mode and group
	.[seed_rate>0,] # get rid of observations with seed_rate==0

# gam(dryyield~s(distance,k=10),data=seed_use_final[dryyield>1,]) %>% plot()

# ggplot(data=seed_use_final[dryyield>1,]) +
# 	geom_point(aes(y=dryyield,x=distance))

#===================================
# Making plots 
#===================================
#--------------------------
# Define the function to create plots and subplots plygons
#--------------------------
get_polygons <- function(g){
g = 5
	#--- mode_group to work on ---#
	temp_data <- seed_use_final[mode_group==mode_group_ls[g],] %>% 
		.[seed_rate<25000 | seed_rate>38000, seed_rate:=-10000]

	#--- number of points in the group ---#
	num_points <- nrow(temp_data) 

	#--------------------------
	# Cluster seed rate
	#--------------------------
	is_rate_1 <- temp_data[seed_rate>=26000 & seed_rate< 29000,] %>%
		.[c(0,diff(time_stamp))== 1,] %>% 
		nrow(.) > 15  

	is_rate_2 <- temp_data[seed_rate>=29000 & seed_rate< 31500,] %>%
		.[c(0,diff(time_stamp))== 1,] %>% 
		nrow(.) > 15  

	is_rate_3 <- temp_data[seed_rate>=31500 & seed_rate< 34500,] %>%
	 	.[c(0,diff(time_stamp))== 1,] %>% 
		nrow(.) > 15  

	is_rate_4 <- temp_data[seed_rate>=34500 & seed_rate< 37500,] %>%
		.[c(0,diff(time_stamp))== 1,] %>% 
		nrow(.) > 15  

	#--- clustering ---#
	temp_data$cluster <- kmeans(temp_data[,seed_rate],center=1+sum(is_rate_1+is_rate_2+is_rate_3+is_rate_4),nstart=25)$cluster

	# ggplot() +
	# 	geom_point(data=temp_data[25000<seed_rate & seed_rate<38000,],aes(y=seed_rate,x=id))

	#--------------------------
	# Identify ids that start clusters
	#--------------------------
	temp_data[,cluster_dif:=c(0,diff(cluster))]

	id_ls_potential <- c(temp_data[1,id],temp_data[cluster_dif!=0 & seed_rate!=-10000,id])
	id_len <- length(id_ls_potential)

	cluster <- vector()
	for (i in 1:id_len){
		cluster[i] <- temp_data[id %in% id_ls_potential[i]:max(nrow(temp_data),(id_ls_potential[i]+15)),cluster] %>% var(.)==0 
	}
	start_id_ls <- id_ls_potential[cluster]
	start_id_len <- length(start_id_ls)
	temp_data[,plot:=-999]
	for (i in 1:start_id_len){
		if(i!=start_id_len){
			stop_id <- temp_data[
				id %in% (start_id_ls[i]+1):start_id_ls[i+1] 
				& cluster_dif!=0,
				id]	%>% 
				min() 

		} else{ # the final i
		stop_id_ls <- temp_data[id > start_id_ls[i] & cluster_dif!=0,id]	
			if(length(stop_id_ls)==0){
				stop_id <- temp_data[nrow(temp_data),id]
			} else{
				stop_id <- min(stop_id_ls)
			}
		}
		temp_data[id >= start_id_ls[i] & id < stop_id,plot:=i]
	}

	#--------------------------
	# Subgroup within a plot
	#--------------------------
	temp_data[,sub_plot:=-999]
	for (i in 1:start_id_len){
		plot_data <- temp_data[plot==i,]
		points <- plot_data[,.(X,Y)] %>% as.matrix()
		plot_data$dist <- dist(points) %>% 
			as.matrix() %>% 
			.[1,]*3.28084

		sub_plot_len <- max(plot_data$dist)%/%plot_len
		for (s in 1:sub_plot_len){
			if(s==sub_plot_len){
				id_ls_temp <- plot_data[dist>=plot_len*(s-1),id]
				temp_data[id %in% id_ls_temp,sub_plot:=s]
			}else{
				id_ls_temp <- plot_data[dist>=plot_len*(s-1) & dist<plot_len*s,id]
				temp_data[id %in% id_ls_temp,sub_plot:=s]
			}
		}
	}

	#--------------------------
	# rotation angle
	#--------------------------
	temp_vec_dt <- temp_data[,.(diff(X),diff(Y))] %>% 
		.[,vec_dist:=sqrt(V1^2+V2^2)] %>% 
		.[,V1_norm:=V1*swidth_m_half/vec_dist] %>% 
		.[,V2_norm:=V2*swidth_m_half/vec_dist]  

	rotate_mat_1 <- matrix(c(
		cos(pi/2),
		sin(pi/2),
		-sin(pi/2),
		cos(pi/2)),
		nrow=2)
	vec_add_1 <- as.matrix(temp_vec_dt[,.(V1_norm,V2_norm)]) %*% rotate_mat_1 %>% 
		rbind(.[1,],.) 
	end_points_1 <- as.matrix(temp_data[,.(X,Y)]) + vec_add_1

	rotate_mat_2 <- matrix(c(
		cos(-pi/2),
		sin(-pi/2),
		-sin(-pi/2),
		cos(-pi/2)),
		nrow=2)
	vec_add_2 <- as.matrix(temp_vec_dt[,.(V1_norm,V2_norm)]) %*% rotate_mat_2 %>% 
		rbind(.[1,],.) 
	end_points_2 <- as.matrix(temp_data[,.(X,Y)]) + vec_add_2

	#--------------------------
	# Create layers of polygons
	#--------------------------
	polygons_ls <- list()
	for (i in 1:(num_points-1)){
		polygons_ls[[i]] <- rbind(
			end_points_1[i,],
			end_points_1[i+1,],
			end_points_2[i+1,],
			end_points_2[i,],
			end_points_1[i,]
			) %>% 
		list() %>% 
		st_polygon()
	}
	
	# Combined with the original data
	# Note: i+1 data is assigned to polygon created using i and i+1 location data 	
	temp_data_sf <- temp_data[-1,] %>% 
		.[,geometry:=st_sfc(polygons_ls)] %>% 
		st_as_sf()

	return(temp_data_sf)
}

#--------------------------
# Create plots and subplots polygons
#--------------------------
# 1 meter = 3.28084 feet
# pi = 180
swidth <- 60 # (24*2.5): 24 nozzles each spanning 2.5 feet
swidth_m_half <- swidth/3.28084/2
plot_len <- 60 # length of each plot

#--- mode_group to loop over ---#
mode_group_ls <- unique(seed_use_final$mode_group) 
group_len <- length(mode_group_ls) 

#--- create plots and subplots polygons ---#
seed_sf <- mclapply(1:group_len,get_polygons,mc.cores=6) %>% 
	do.call(rbind,.) %>% 
	filter(sub_plot!=-999) %>% 
	mutate(mgps=paste(which_mode,group,plot,sub_plot,sep='_'))

st_crs(seed_sf) <- 26914

#--- get rid of observations with invalid polygons (self-intersections) ---#
sf_valid <- st_is_valid(seed_sf)
seed_sf <- seed_sf[sf_valid,]

# plot(seed_sf)
# tm_shape(seed_sf) +
# 	tm_polygons('mgps')

#--- save ---#
saveRDS(seed_sf,paste('./Data/ILS_Barton_37/seed_sf_',plot_len,'.rds'))

#--------------------------
# Union all the polygons by subplot
#--------------------------
mgps_ls <- seed_sf$mgps %>% unique()
mgps_len <- length(mgps_ls)

plot_unionize <- function(i){
	temp_data <- filter(seed_sf,mgps==mgps_ls[i])
	temp_plot <- filter(seed_sf,mgps==mgps_ls[i]) %>% 
		st_union()
	temp_sf <- st_sf(
		seed_rate=mean(temp_data$seed_rate),
		seed_target=mean(temp_data$seed_target),
		mgps=mgps_ls[i],
		geometry=temp_plot
	)
	return(temp_sf)
}

seed_plots_sf <- mclapply(1:mgps_len,plot_unionize,mc.cores=6) %>% 
	do.call(rbind,.)
 
saveRDS(seed_plots_sf,paste('./Data/ILS_Barton_37/seed_plots_sf_',plot_len,'.rds'))

# seed_plots_sf <- readRDS(paste('./Data/ILS_Barton_37/seed_plots_sf_',plot_len,'.rds'))

