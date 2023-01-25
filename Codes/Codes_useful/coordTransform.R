devtools::use_package("sf")

#' Tranform any projection to UTM for a sf object:
#'
#' This take in any sf geometries and returns
#'  a new sf geometry with the right UTM zone:
#' @param field  of class 'sf'
#' @keywords UTM, Projection, Simple Features, sf
#' @export
#' @examples
#' coordTransform()
coordTransform <- function(field) {
  long2UTM <- function(long) {
    (floor((long + 180) / 6) %% 60) + 1
  }
  if (is.na(sf::st_crs(field))) {
    sf::st_crs(field) <- sf::st_crs(4326)
  } else {
    field <- sf::st_transform(field, sf::st_crs(4326))
  }
  utmzone <- long2UTM(mean(sf::st_bbox(field)[c(1, 3)]))
  projutm <- sf::st_crs(paste0("+init=epsg:326", utmzone))
  field <- sf::st_transform(field, projutm)
  return(field)
}

#' Tranform any projection to UTM for a sf object:
#'
#' This take in any sf geometries and returns
#'  a new sf geometry with the right UTM zone:
#' @param sf_obj  of class 'sf'
#' @keywords UTM, Projection, Simple Features, sf
#' @export
#' @examples
#' st_utm(fields)
st_utm <- function(sf_obj) {
  # Function to get UTM Zone from mean longitude:
  long2UTM <- function(long) {
    (floor((long + 180) / 6) %% 60) + 1
  }
  
  #Check if the object class is 'sf':
  obj_c = class(sf_obj)[1]
  if(obj_c == 'sf'){
    # In case the object has no projectin assigned,
    #  assume it to geographic WGS84 :
    if (is.na(sf::st_crs(sf_obj))) {
      sf::st_crs(sf_obj) <- sf::st_crs(4326)
    }
    
    # Get the center longitude in degrees:
    bb <- sf::st_as_sfc(sf::st_bbox(sf_obj))
    bb <- sf::st_transform(bb, sf::st_crs(4326))
    
    # Get UTM Zone from mean longitude:
    utmzone <- long2UTM(mean(sf::st_bbox(bb)[c(1, 3)]))
    
    # Get the hemisphere based on the latitude:
    NS <- 100 * (6 + (mean(sf::st_bbox(bb)[c(2, 4)]) < 0))
    
    # Add all toghether to get the EPSG code:
    projutm <- sf::st_crs(32000 + NS + utmzone)
    
    # Reproject data:
    sf_obj <- sf::st_transform(sf_obj, projutm)
    return(sf_obj)
  }else{
    options(error=NULL)
    stop("Object class is not 'sf', please insert a sf object!")
  }
}
