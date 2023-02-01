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
| Genetics | For soybean: maturity group (0 to 6, by 1) For corn: RM (80 to 130, by 5) |

# How to Run

Create a results folder and place the input .csv inside. 

Open 1_input_to_sf.R, which is found in trial_characterization/Codes/1_input_to_sf.R.

Edit 1_input_to_sf.R line 3 to the full path to the results folder which has the input .csv. 

Edit 1_input_to_sf.R line 6 to full path to where you have the trial_characterization folder. 

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

# Contact

Questions about the code and methodology: German Mandrini, Dpt of Crop Sciences, University of Illinois at Urbana-Champaign, [germanmandrini@gmail.com](mailto:germanmandrini@gmail.com)

Questions about collaborations: Nicolas F Martin, Dpt of Crop Sciences, University of Illinois at Urbana-Champaign, [nfmartin@illinois.edu](mailto:nfmartin@illinois.edu)
