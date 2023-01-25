# devtools::use_package('sf')
# devtools::use_package('dplyr')

#' Complements the over function in sf:
#'
#' This take in any two sets of sf geometries and returns
#'  the intersects in the same way as the sp over:
#' @param x of class 'sf' to intersect
#' @param y of class 'sf' to be intersected
#' @keywords Over, Intersect, Simple Features, sf
#' @export
#' @examples
#' st_over()
st_over = function(x, y) {
  sapply(sf::st_intersects(x, y), function(z)
    if (length(z) == 0)
      NA_integer_
    else
      z[1])
}

# new_function
#Does a point in polygon operation. Return the median of the points in each polygon

if(FALSE){
  pol =  field_info 
  pnt = as_applied
  pnt_variable = 'Rt_Apd_Ms_'
  new_name = 'urea_lbs_ac'
}

st_add_median = function(pol, pnt, pnt_variable, new_name) {

  pnt <- st_transform(pnt, st_crs(pol)) 

  
  pol <- dplyr::mutate(pol, id = 1:nrow(pol))

  pnt <-  dplyr::select(pnt, pnt_variable)
  names(pnt)[names(pnt) == pnt_variable] <- 'x'

  join <- st_join(pnt, pol, join = st_intersects)

  join_sum <- join %>% group_by(id) %>% summarize(x_median = median(x))

  st_geometry(join_sum) <- NULL

  result <- dplyr::left_join(pol, join_sum, by = 'id') %>% dplyr::select(-id) %>% setnames('x_median', new_name)

  return(result)

}
