#' Converts units of nitrogen application
#'
#' Converts gal/ac or lbs/ac fertilizer to kg/ha N
#' @param values of class 'numeric'
#' @param n_source of class 'string', representing the N fertilizer type
#' @param orig_units of class 'string', representing the current unit of measurement.
#' @keywords Over, Intersect, Simple Features, sf
#' @export
#' @examples
#' n_total <- n_convert(c(40,40.5,42,45), 'nh3', 'gal/ac')
#'
n_convert <- function(values, n_source, orig_units){

  # values = 100
  # n_source = 'nh3'
  # orig_units = 'lbs/ac'

  n_source_options <- c('nh3', 'u28', 'u32', 'urea', 'n')
  n_source_pct <- c(0.82,0.28,0.32,0.46,1)
  n_source_density_lbs_gal <- c(5.15,10.67,11.06,NA,NA)

  if(orig_units == 'gal/ac'){
    n_pounds_ac <- values * n_source_density_lbs_gal[which(n_source_options == n_source)] * n_source_pct[which(n_source_options == n_source)]
  }else if(orig_units == 'lbs/ac'){
    n_pounds_ac <- values * n_source_pct[which(n_source_options == n_source)]
  }

  lbs_ac_to_kg_ha <- 1.12085

  n_kg_ha <- n_pounds_ac * lbs_ac_to_kg_ha

  return(n_kg_ha)
}

seed_convert <- function(values, orig_units){

  units_options <- c('count/ac', 'k_count/ac', 'k_count/ha')
  converter <- c(1/4086.86, 0.2471052, 0.1)

  count_m <- values * converter[which(units_options == orig_units)]
  return(count_m)
}

1000/10000

yield_convert <- function(values, crop, orig_units){

  if(orig_units == 'bu/ac'){
    crop_options <- c('corn', 'wheat', 'soybean')
    converter_by_crop <- c(62.77,  67.25,  67.25)
    converter <- converter_by_crop[which(crop_options == crop)]
  }else if(orig_units == 'tn/ha'){
    converter <- 1000
}else{
    stop('Non accepted orig_units')
  }

  yield_kg_ha <- values * converter
  return(yield_kg_ha)
}

#' Complements the over function in sf:
#'
#' This take in any two sets of sf geometries and returns
#'  the intersects in the same way as the sp over:
#' @param x of class 'sf' to intersect
#' @param y of class 'sf' to be intersected
#' @keywords Over, Intersect, Simple Features, sf
#' @export
#' @examples
#' yield_to_kgha(x)
#'
yield_to_kgha <- function(x) {
  avg <- median(x)
  cidx <- as.integer(cut(avg, c(0, 20, 5000, Inf)))
  cfct <- c(1000, 62.77, 1)[cidx]
  xc <- x * cfct
  return(xc)
}
