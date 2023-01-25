# x <- info_trials_tmp[,.(Year, Farm, Planting_Date, Irrigation)]
# y <- calibration_tmp[,.(seed_m2_bin, ntotal_kgha)]


expand_table <- function(x,y){
  
  x = data.table(x)
  y = data.table(y)
  #repeat each row of x as many times as y
  z <- cbind(x[rep(1:nrow(x), each = nrow(y))], y)
  return(z)
}



# filter_dt_in_dt <- function(x_dt, filter_dt){
#   cols <- intersect(names(x_dt), names(filter_dt))
#   for(col_n in cols){
#     # col_n <- cols[4]
#     x_dt <- x_dt[get(col_n) == filter_dt[[col_n]]]
#   }
#   return(x)
# }

# x_dt <- eonr_mukey_dt3
# filter_dt <- stations_dt[,.(id_10, id_field)]

filter_dt_in_dt <- function(x_dt, filter_dt, return_table = FALSE){
  cols <- intersect(names(x_dt), names(filter_dt))
  # output_list <- list()
  vector_filter <- c()
  for(filter_row in 1:nrow(filter_dt)){
    # filter_row =1
    filter_tmp <- filter_dt[filter_row]
    # x_dt2 <- copy(x_dt)
    vector <- 1:nrow(x_dt)
    
    for(col_n in cols){
      # col_n <- cols[3]
      # x_dt2 <- x_dt2[get(col_n) == filter_tmp[[col_n]]]

      vector <- base::intersect(vector, which(x_dt[[col_n]] == filter_tmp[[col_n]]))
    }
    # output_list[[length(output_list)+1]] <- x_dt2
    vector_filter <- append(vector_filter, vector)
  }
  if(return_table){
    return(x_dt[vector_filter])
  }else{
    return(vector_filter)}
}


# data_dt1 <- rbind(data.table(field = 1, zone = 1:3, area_ha = as.numeric(c(10,20,30)), yld = as.numeric(c(10, 15, 8)), N = as.numeric(c(100, 200, 80))), 
#                  data.table(field = 2, zone = 1:3, area_ha = as.numeric(c(20,30,30)), yld = as.numeric(c(11, 15, 8)), N = as.numeric(c(120, 150, 80))))
# 
# data_dt2 <- rbind(data.table(field = 1, zone = 1:3, area_ha = as.numeric(c(50,20,30)), yld = as.numeric(c(12, 15, 8)), N = as.numeric(c(100, 150, 80))), 
#                  data.table(field = 2, zone = 1:3, area_ha = as.numeric(c(20,30,30)), yld = as.numeric(c(11, 15, 8)), N = as.numeric(c(120, 150, 180))))
# 
# data_dt <- rbind(data.table(region = 1, data_dt1), data.table(region = 2, data_dt2))
# 
# 
# variables = c('yld', 'N')
# weight = 'area_ha'
# by_c = c('field', 'region')


aggregate_by_area <- function(data_dt, variables = c('yld', 'N'), weight = 'area_ha', by_c = NULL){
  data_dt1 <- data.table(copy(data_dt)) #avoid updated in the global env 
  setnames(data_dt1,weight, 'area_ha1' ) #was the only way to avoid errors. Get and eval did not function in some situations
  
  data_dt1[, (variables) := lapply(.SD, function(x) x * area_ha1), .SDcols = variables]
  
  data_dt2 <- data_dt1[, lapply(.SD, sum, na.rm=TRUE), .SDcols = c(variables, 'area_ha1'), by=by_c]
  
  data_dt2[, (variables) := lapply(.SD, function(x) x / area_ha1), .SDcols = variables]
  
  setnames(data_dt2,'area_ha1', weight )
  
  return(data_dt2)
  }

eonr_from_rf <- function(pred_set, trial_rates, rf_model){
  # pred_set <- prediction_set
  # pred_set <- prediction_set_aggregated
  #1) Get the yld predicted by the RF
  by_c <- names(pred_set)[names(pred_set) %in% c("z", "rotation", "mukey")]
  
  pred_set2 <- pred_set[rep(1:nrow(pred_set), each = length(trial_rates))]
  pred_set2$N_fert <- rep(trial_rates, times = nrow(pred_set))
  
  pred_set2$yld_pred <- predict(rf_model, pred_set2, type = "class")
  
  
  #2) Fit a quadratic regression
  pred_set3 <- pred_set2[,list(intercept=coef(lm(yld_pred~N_fert + I(N_fert^2)))[1], coef1=coef(lm(yld_pred~N_fert + I(N_fert^2)))[2],
                                           coef2=coef(lm(yld_pred~N_fert + I(N_fert^2)))[3]),by=by_c]
  
  #3) Numerically find EONR
  eonr_rates <- seq(0,max(trial_rates), by = 1)
  rep_times <- nrow(pred_set3)
  
  pred_set3 <- pred_set3[rep(1:nrow(pred_set3), each = length(eonr_rates))]
  pred_set3$N_fert <- rep(eonr_rates, times = rep_times)
  pred_set3[,Yld_pred := intercept + N_fert * coef1 + N_fert ^ 2 * coef2]
  pred_set3[, P := Yld_pred * Pc - N_fert * Pn]
  # yc_yearly_dt[, P := Yld * Pc - N_fert * Pn - leach_n2 * Pe]
  
  
  pred_set4 <- pred_set3[, .SD[ P == max( P)], by = by_c]
  pred_set4[,eonr_pred := round(N_fert/10,0)*10]
  pred_set_upd <- merge(pred_set,  pred_set4[,c(by_c, 'eonr_pred'), with = FALSE], by = by_c)
  
  return(pred_set_upd)
}

