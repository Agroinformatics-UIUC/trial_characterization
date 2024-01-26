rm(list=setdiff(ls(), c("codes_folder", "result_folder")))

library(stringr)
library(data.table)

#Get the computer where this is running
server <- ifelse(Sys.info()["nodename"] == "campodonico", TRUE, FALSE)
cpsc <-ifelse(Sys.info()["nodename"] == "CPSC-P10E53323", TRUE, FALSE)
cluster <- str_detect(string = Sys.info()["nodename"], pattern = 'campuscluster')
print(Sys.info()["nodename"])

if(!exists("result_folder")) {
  result_folder <- "~/example_characterization"
  setwd(result_folder) 
} 
if(!exists("codes_folder")) {codes_folder <-'~/trial_characterization'} 

# ---------------------------------------------------------------------------------------------
# Load needed files
trials_dt <- readRDS("./trial_characterization_box/rds_files/trials_sf.rds") %>% 
  data.table() %>% .[,-'geometry']


# CREATE ALL FILES
start1 <- Sys.time()
"./trial_characterization_git/Codes/5_simB_setup.R"
source(paste0(codes_folder, '/Codes/5_simB_setup.R'))
instructions_rows <- nrow(trials_dt)

#RUN ALL APSIM FILES
start2 <- Sys.time()
"./trial_characterization_git/Codes/8_simF_run_files.R"
source(paste0(codes_folder, '/Codes/8_simF_run_files.R'))

#MERGE ALL THE OUTPUT
start3 <- Sys.time()
"./trial_characterization_git/Codes/9_simG_merge_results.R"
source(paste0(codes_folder, '/Codes/9_simG_merge_results.R'))

start4 <- Sys.time()

#MAKE YEARLY SUMMARY
'./trial_characterization_git/Codes/10_simH_daily_to_yearly.R'
source(paste0(codes_folder, '/Codes/10_simH_daily_to_yearly.R'))
  
unlink(directory, recursive = TRUE)
  
start5 <- Sys.time()

time_track_tmp <- data.table(trials = nrow(trials_dt),
                             time = start1,
                             create = as.numeric(difftime(start2, start1, units = "mins")),
                             run = as.numeric(difftime(start3, start2, units = "mins")),
                             merge_save = as.numeric(difftime(start4, start3, units = "mins")),
                             yearly_summary = as.numeric(difftime(start5, start4, units = "mins")),
                             all = as.numeric(difftime(start5, start1, units = "mins")))
print(time_track_tmp)

saveRDS(time_track_tmp, './trial_characterization_box/rds_files/time_track.rds')

