# install.packages("readr") # you only need to do this one time on your system
setwd('~')
library(readr)

files_look <- list.files('C:/Users/germanm2/Documents/apsim_illinois_git', recursive = T, full.names = T)
files_look <- list.files('C:/Users/germanm2/Box Sync/My_Documents/vr_value_v2/Codes', recursive = T, full.names = T)
files_look <- list.files('./regions_tester/Codes', recursive = T, full.names = T)
files_look <- list.files('./n_management_git/Codes', recursive = F, full.names = T, pattern = '*.R')
files_look <- list.files('./n_management_box/Data/APssurgo_master', recursive = T, full.names = T, pattern = '*.R')
files_look <- list.files('./n_policy_git/Codes', recursive = F, full.names = T, pattern = '*.R')

files_look <- list.files('./n_policy_box/Data/APssurgo_master', recursive = T, full.names = T, pattern = '*.R')
files_look <- list.files('C:/Users/germanm2/Documents/n_dynamic_git/Codes', recursive = F, full.names = T, pattern = '*.R')
files_look <- list.files('./grid_data_git/Codes', recursive = F, full.names = T, pattern = '*.R')

dirs_list <- list.dirs(recursive = F)
dirs_list <- dirs_list[grep(pattern = '_git', x = dirs_list)]
files_look <- list.files(dirs_list,recursive = T, full.names = T, pattern = '*.R$')

setwd('~')#CPSC
# Search until it finds the pattern
pos <- FALSE
i=1

while(isFALSE(pos)){
  print(file_n)
  file_n <- files_look[i]
  mystring <- read_file(file_n)
  
  pos = grepl('st_join', mystring)
  i = i+1
  
}

#Search in all the files and print the ones that have it
for(file_n in files_look){
  # print(file_n)
  # file_n <- files_look[i]
  mystring <- read_file(file_n)
  
  pos = grepl('grid10_soils_dt', mystring)
  if(pos){print(file_n)}
  
}

#Replace strings
for(file_n in files_look){
  # print(file_n)
  # file_n <- files_look[3]
  mystring <- read_file(file_n)
  mystring <- gsub(pattern = "Project.Grid/Grid/Codes", replacement = "grid_data_git/Codes", mystring)
  write_file(mystring, path = file_n, append = FALSE)
  
}

