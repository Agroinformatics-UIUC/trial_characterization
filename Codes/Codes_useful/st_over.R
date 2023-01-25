devtools::use_package('sf')

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

st_erase <- function(x, y){
  x.union <- st_union(st_combine(x))
  x.union.sfc <- st_buffer(x.union, 0)
  
  y.union <- st_sfc(st_combine(y))
  y.union.sfc <- st_buffer(y.union, 0)
  
  border.sfc<- st_difference(x.union.sfc, y.union.sfc)
  
}  
  