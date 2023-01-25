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
  # pred_set <- prediction_set_aggregated[,-'eonr_pred']
  #1) Get the yld predicted by the RF
  by_c <- names(pred_set)[names(pred_set) %in% c("z", "rotation", "mukey")]
  
  pred_set2 <- pred_set[rep(1:nrow(pred_set), each = length(trial_rates))]
  pred_set2$N_fert <- rep(trial_rates, times = nrow(pred_set))
  
  pred_set2$yld_pred <- predict(rf_model, pred_set2, type = "class")
  
  # 2) Numerically find EONR
  pred_set2[, P := yld_pred * Pc - N_fert * Pn]
  pred_set3 <- pred_set2[, .SD[ P == max( P)][1], by = by_c]
  setnames(pred_set3, 'N_fert', 'eonr_pred')
  
  # ggplot() +
  #   geom_point(data = pred_set3, aes(x = Yld, y = yld_pred, color = rotation))  +
  #   coord_fixed() + geom_abline() + 
  #   ylim(4000, 14000)+ xlim(4000, 14000) +
  #   geom_smooth()+
  #   theme(aspect.ratio=1, 
  #         axis.text=element_text(size=12),
  #         axis.title=element_text(size=14,face="bold"))+
  #   theme_bw()
  
  # 3) Update the original file
  pred_set_upd <- merge(pred_set,  pred_set3[,c(by_c, 'eonr_pred'), with = FALSE], by = by_c)
  
  return(pred_set_upd)
}
# model = model2_eonr
# data = ValidSet3_eonr2
# quant = 0.7

predictrf_quantile <- function(model, data, quant = 0.7){
  pred <- apply( predict(model, data, predict.all=TRUE)$individual, 1, function(x) quantile(x,quant))
  pred_round <- round(as.numeric(pred)/10)*10
  return(pred_round)
}

get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

theme_Publication <- function(base_size=15, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

# Rodrigo
# theme(
#   legend.text = element_text(face = "bold"),
#   legend.title = element_text(face = "bold"),
#   axis.text =element_text(face = "bold"),
#   axis.title = element_text(face = "bold"),
#   strip.text = element_text(face = "bold")
# )
theme_thesis <- function(base_size=15) {
  library(grid)
  library(ggthemes)
  (theme_bw(base_size = 15)+
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            axis.text = element_text(size = 15),
            strip.text = element_text(face="bold"),
            legend.text=element_text(size=12)))
  
}


