#detach packages (run twice)
#invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))

#--- packages ---#
#install the outdated packages
if(!(file.exists(file.path(.libPaths(), "reshape2", 'DESCRIPTION'))[1])) {install.packages("reshape2")}
if(!(file.exists(file.path(.libPaths(), "devtools", 'DESCRIPTION'))[1])) {install.packages("devtools")}
library(devtools)
if(!(file.exists(file.path(.libPaths(), "urbnmapr", 'DESCRIPTION'))[1])) {devtools::install_github("UrbanInstitute/urbnmapr")}


if(!(file.exists(file.path(.libPaths(), "apsimr", 'DESCRIPTION'))[1])){
  install.packages(paste0(codes_folder, "/R_packages_not_in_CRAN/apsimr_1.2.tar.gz"), repos = NULL, type = "source")
} else if (packageVersion("apsimr") != '1.2') {
  unload("apsimr")
  remove.packages("apsimr")
  install.packages(paste0(codes_folder, "/R_packages_not_in_CRAN/apsimr_1.2.tar.gz"), repos = NULL, type = "source")
}

#what packages will we need?

# list.of.packages <- c("devtools","XML","stringr","MuMIn","gstat","sp","sf","readxl",
#   "data.table","devtools","dplyr","snow","parallel","foreach","stargazer","tmap",
#   "raster","gridExtra","grDevices","spdep","lmtest","urbnmapr",
#   "splm","mgcv","daymetr","soilDB","apsimr","lmeInfo","maps","sirad","zoo")

#definitely needed
#dplyr, devtools, sf, sp, spData, tmap, daymetr, data.table, iterators
#soilDB, snow, parallel, stringr, urbnmapr, XML, xml2, tools, zoo

list.of.packages <- c("dplyr", "devtools", "sf", "sp", "spData", "tmap", "daymetr", "data.table", "reshape2",
  "soilDB", "parallel", "snow", "stringr", "urbnmapr", "XML", "xml2", "tools", "zoo", "foreach", "iterators")

#of those, install missing packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) != 0) {lapply(new.packages, function(x) {install.packages(x, dependencies=TRUE)})}

#list.of.packages[!sapply(list.of.packages, require, character.only = TRUE)]

invisible(lapply(list.of.packages, require, character.only = TRUE))
