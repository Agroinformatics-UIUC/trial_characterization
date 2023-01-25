#--- packages ---#
#install.packages(c('Hmisc', 'splm', 'geosphere', 'stringr', 'stargazer', 'sf','grDevices', 'XML', 'apsimr', 'gstat','sp', 'data.table', 'dplyr', 'snow', 'readxl', 'parallel', 'foreach', 'rgeos', 'tmap', 'raster', 'rgdal', 'maptools','gridExtra', 'MuMIn', 'maptools', 'spdep', 'lmtest', 'mgcv', 'grid', 'gtable', 'APSIM') , dependencies=TRUE)
#devtools::install_github("tidyverse/ggplot2")
#install.packages('devtools')
list.of.packages <- c('Hmisc','splm', 'geosphere', 'stringr', 'stargazer', 'sf','grDevices', 'XML', 'gstat','sp', 'data.table', 'dplyr', 'snow', 'readxl', 'parallel', 'foreach', 'rgeos', 'tmap', 'raster', 'rgdal', 'maptools','gridExtra', 'MuMIn', 'maptools', 'spdep', 'lmtest', 'mgcv', 'grid', 'gtable', 'soilDB', 'xml2') #'APSIM', 'apsimr', 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(list.of.packages, dependencies=TRUE)

#SERVER ISSUES
# sudo apt-get install -y libprotobuf-dev protobuf-compiler
# install.packages("devtools")
# library(devtools)
# install_version("rgeos", version = "0.3-28")

lapply(list.of.packages, require, character.only = TRUE)


 library(devtools)
 library(XML)
 library(stringr)
 library(MuMIn)
 library(apsimr)
 library(gstat)
 library(sp)
 library(sf)
 library(sp)
 library(readxl)
 library(data.table)
 library(dplyr)
 library(snow)
 library(parallel)
 library(foreach)
 library(rgeos)
 library(stargazer)
 library(tmap)
 library(raster)
 library(rgdal)
 library(maptools)
 library(gridExtra)
 library(grDevices)
 library(spdep)
 library(lmtest)
  library(splm)
 library(mgcv)
