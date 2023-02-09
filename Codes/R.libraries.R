#--- packages ---#
#install the outdated packages
if(!(file.exists(file.path(.libPaths(), "reshape2", 'DESCRIPTION'))[1])) {install.packages("reshape2")}
if(!(file.exists(file.path(.libPaths(), "apsimr", 'DESCRIPTION'))[1])){
  install.packages(paste0(codes_folder, "/R_packages_not_in_CRAN/apsimr_1.2.tar.gz"), repos = NULL, type = "source")
} else if (packageVersion("apsimr") != '1.2') {
  unload("apsimr")
  remove.packages("apsimr")
  install.packages(paste0(codes_folder, "/R_packages_not_in_CRAN/apsimr_1.2.tar.gz"), repos = NULL, type = "source")
}
  
  
if(!(file.exists(file.path(.libPaths(), "lmeInfo", 'DESCRIPTION'))[1])){
  install.packages(paste0(codes_folder, "/R_packages_not_in_CRAN/lmeInfo_0.2.1.tar.gz"), repos = NULL, type = "source")
} else if (packageVersion("lmeInfo") != '0.2.1') {
  unload("lmeInfo")
  remove.packages("lmeInfo")
  install.packages(paste0(codes_folder, "/R_packages_not_in_CRAN/lmeInfo_0.2.1.tar.gz"), repos = NULL, type = "source")
}


if(!(file.exists(file.path(.libPaths(), "soilDB", 'DESCRIPTION'))[1])){
  install.packages(paste0(codes_folder, "/R_packages_not_in_CRAN/soilDB_2.6.14.tar.gz"), repos = NULL, type = "source")
} else if (packageVersion("soilDB") != '2.6.14') {
  unload("soilDB")
  remove.packages("soilDB")
  install.packages(paste0(codes_folder, "/R_packages_not_in_CRAN/soilDB_2.6.14.tar.gz"), repos = NULL, type = "source")
}

#what packages will we need?
#list.of.packages <- c('Hmisc','splm', 'geosphere', 'stringr', 'stargazer', 'sf','grDevices', 'XML', 'gstat','sp',
#                      'data.table', 'dplyr', 'snow', 'readxl', 'parallel', 'foreach', 'rgeos', 'tmap', 'raster', 
#                      'rgdal', 'maptools','gridExtra', 'MuMIn', 'maptools', 'spdep', 'lmtest', 'mgcv', 'grid',
#                      'gtable', 'soilDB', 'xml2', 'daymetr', 'soilDB', 'apsimr', 'lmeInfo') 

list.of.packages <- c("devtools","XML","stringr","MuMIn","gstat","sp","sf","readxl",
  "data.table","dplyr","snow","parallel","foreach","rgeos","stargazer","tmap",
  "raster","rgdal","maptools","gridExtra","grDevices","spdep","lmtest",
  "splm","mgcv","daymetr","soilDB","apsimr","lmeInfo","maps","sirad","zoo")

#of those, install missing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) != 0) {lapply(new.packages, function(x) {install.packages(x, dependencies=TRUE)})}

#list.of.packages[!sapply(list.of.packages, require, character.only = TRUE)]

invisible(lapply(list.of.packages, require, character.only = TRUE))
