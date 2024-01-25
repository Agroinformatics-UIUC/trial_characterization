# Trial characterization using APSIM (Envirotyping)

# Objective

This repository takes a csv with information from trials (crop, planting date, latitude, longitude). The csv should always have the same columns as the sample. It downloads soil and weather information, and it runs APSIM for each trial. In the output will add columns to the original csv with soil, weather, and APSIM information.

# Use

The characterization variables can be used to characterize the environment of the trials, find clusters of trials with similar conditions, use the variables as regressors to explain the trial results, extrapolate results to areas where trials were not implemented.

# Input Variables

Create an input .csv file formatted as following, where each row is one trial to be simulated. 

| **Variable** | **Description** |
| --- | --- |
| Site | User site identifier |
| Planting | Planting date (month / day / year) |
| Latitude | Latitude of the trial point |
| Longitude | Longitude of the trial point |
| Crop | soybean or maize |
| Genetics | For soybean: maturity group (0 to 6, by 1) For maize: RM (80 to 130, by 5) |

# How to Run

If you don't have the last version of Classic APSIM (APSIM710-r4220), install it with all of the default settings. 

Create a results folder and place the input .csv inside.

Open 1_input_to_sf.R, which is found in trial_characterization/Codes/1_input_to_sf.R.

Edit 1_input_to_sf.R line 3 to the full path to the results folder which has the input .csv. 

Edit 1_input_to_sf.R line 7 to full path to where you have the trial_characterization folder. 

Run files 1_input_to_sf.R, 2_weather_downloader.R, 3_soils_manager.R, and 4_simA_manager.R, which are found in trial_characterization/Codes. 

Your outputs will be in the results folder in trial_characterization_box/output. 

# Output variables

| **Variable** | **Description** |
| --- | --- |
| id\_trial | Trial identifier, based on the input.csv |
| Site | User site identifier |
| Planting | Planting date |
| Crop | soybean or maize |
| state | US state |
| region | US region |
| X | latitude |
| Y | longiture |
| yield\_sim | Yield obtained from APSIM. If the simulated yield is very different than the trial yield, the simulation should be discarded |
| whc | water-holding capacity of the soil at the location of the exact coordinates (SSURGO) |
| sand | sand % (0-20cm) of the soil at the location of the exact coordinates (SSURGO) |
| clay | clay % (0-20cm) of the soil at the location of the exact coordinates (SSURGO) |
| om | Organic Matter % (0-20cm) of the soil at the location of the exact coordinates (SSURGO) |
| ph | Ph (0-20cm) of the soil at the location of the exact coordinates (SSURGO) |
| rain\_# | Rain (mm) in the indicated period |
| radn\_# | Mean radiation (MJ/m2/day) in the indicated period |
| MaxT\_# | Max temperature (Celsius) in the indicated period |
| MinT\_# | Min temperature (Celsius) in the indicated period |
| swdef\_expan\_# | APSIM water stress index (0 means stress, 1 means no stress) |
| period\_start\_doy\_# | Doy of the year that the period started |

# Crop periods

The variables are divided into periods (example: rain\_7 means rain during period 7). The periods are the following:

| **Period** | **Description** |
| --- | --- |
| 0 | fallow\_initial |
| 1 | veg\_early |
| 2 | veg\_late |
| 3 | flowering |
| 4 | grainf\_early |
| 5 | grainf\_late |
| 6 | fallow\_end |

# Structure of the Tool 

The climate characterization tool consists of the following files, which are found in the trial_characterization/Codes folder. The files run in order and call on the files indented below them. If all goes smoothly, the user should only need to run the top-level files: 1, 2, 3, and file 4 up to line 65.  

* **1_input_to_sf\.R** (Sets working directory in the format result_folder <- "\~/example_characterization" ; setwd(wd), sets code directory in the format codes_folder <-'~/trial_characterization', creates trials_sf.rds from input file which should be in the results folder. Make sure that the input .csv is formatted properly for the tool: check that dates are formatted correctly, check that maturities are coded -1 through 6.)  
    * **R.libraries.R** (Downloads the R libraries necessary. Uses older packages found in R_packages_not_in_CRAN. The packages are apsimr_1.2, lmeInfo_0.2.1, and soilDB_2.6.14, as tar.gz files.)   
* **2_weather_downloader\.R** (Downloads DAYMET weather for each location, generates weather_dt.rds.)   
    * **R.libraries.R** (Same as before.)   
* **3_soils_manager.R** (Downloads soil data from each location using APssurgo tools, generates soils_sf and horizons_dt.)  
    * **/APssurgo_master/R/get_soils_parallel.R** (Queries https://sdmdataaccess.nrcs.usda.gov/ for soil types (mukeys) of each location, generates soils_sf.rds. Despite the name, it is not actually parallel.)  
    * **/APssurgo_master/R/get_horizons_parallel.R** (Gets horizons information for each of the soil types, generates horizons_dt.rds.)  
* **4_simA_manager.R** (Runs the analysis once the .rds files are generated.)   
    * **5_simB_setup.R** (Constructs APSIM files.)  
        * **6_simC_make_met_files.R** (Makes .met files, which are the weather data for the APSIM files. Should create files in trial_characterization_box/apsim_files/met_files in the format loc_#.met)   
        * **/APssurgo_master/R/calc_apsim_variables_onesoil.R** (Calculates APSIM soil variables from horizons information.)  
            * **/APssurgo_master/R/SaxtonRawls.R** (Calculates soil hydraulic parameters.)  
        * **/APssurgo_master/R/make_apsoils_toolbox.R** (Makes the trial_characterization.soils file at the bottom of apsim_files.)  
        * **7_simD_create_apsim_files.R** (Makes APSIM files. They appear as many folders in the apsim_files folder, named in the format trial_#_crop. Each should contain a .apsim file with the same name.)  
    * **8_simF_run_files.R** (Runs APSIM files. Creates trial\_#\_crop.out and trial\_#\_crop.sum files in the apsim_files folders. This function changes depending on the computer you are running it on (ex: the lab server), because it is looking for copies of APSIM in different places. Please make this generic to any user and wherever they keep their copy of APSIM. Once 8 runs successfully, it’s all downhill from here.) 
    * **9_simG_merge_results.R** (Merge the output.)   
    * **10_simH_daily_to_yearly.R** (Make yearly summaries. The final files should be in trial_characterization_box/output. They are apsim_output_daily.csv, characterization.csv, periods_code.csv, and map.pdf.)  

# Known Issues:
- only runs on Windows
- tool references APSIM in specific locations on whichever computer it is running on, should be generic to wherever it is running
- tool should probably switch to next gen APSIM and use the apsimx package instead of the outdated apsimr package. that change would also allow it to run on systems other than Windows. 
- weather data is missing for many locations 
- tool requires older versions of some packages (which can be found in R_packages_not_in_CRAN)
- R.libraries.R loads way more packages than necessary
- still some extraneous code left over from building the tool. the commented-out debugging options should stay, but not the code that isn't connected to the code that's running.
- why are there two harvest rules in the model? 

# Contact

Questions about the code and methodology: German Mandrini, Dpt of Crop Sciences, University of Illinois at Urbana-Champaign, [germanmandrini@gmail.com](mailto:germanmandrini@gmail.com)

Questions about collaborations: Nicolas F Martin, Dpt of Crop Sciences, University of Illinois at Urbana-Champaign, [nfmartin@illinois.edu](mailto:nfmartin@illinois.edu)

Questions about changes made in this fork: Catherine Gilbert, Dpt of Crop Sciences, University of Illinois at Urbana-Champaign, [cmg3@illinois.edu](mailto:cmg3@illinois.edu)

