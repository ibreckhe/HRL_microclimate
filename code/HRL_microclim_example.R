####Script to demonstrate HRL microclimate data processing functions.
####Author: Ian Breckheimer
####Updated: 8 February 2018

####For additional details about these functions and how to use them, 
####please read the accompanying documentation "HRL_microclim_example.html"

####Set up workspace####
project_dir <- "~/code/HRL_microclimate/" ##must have trailing slash
setwd(project_dir)
source("./code/HRL_microclim_functions.R")

require(data.table)
require(xts)
require(psych)

####Soil temperature data processing####

##Paths of folders to process (all must be absolute paths)
input_paths_soil <- paste(project_dir,c("data/test_data/soil_2014/","data/test_data/soil_2015/"),
                          sep="")

##Compiles data into a common format with consistent names.
soil_formatted <- format_micro_csv(input_paths=input_paths_soil,
                                   output_path=paste(project_dir,"temp/soil",sep=""),
                                   file_prefixes=c("Ian_soil","Ian_soil"),
                                   output_metadata_filename="metadata_soil.txt",overwrite=TRUE)

##Cleans soil temperature data, removing spikes and unphysical values.
clean_soil_temps(input_path=paste(project_dir,"temp/soil",sep=""),
                input_metadata_filename="metadata_soil.txt",
                output_path=paste(project_dir,"output/soil",sep=""),
                output_metadata_filename="metadata_flags_soil.txt",
                figure_path=paste(project_dir,"figs/soil",sep=""),
                guess_tz="Etc/GMT-7",temp_spike_thresh=20,
                min_temp_thresh=-20,max_temp_thresh=60,max_temp_hr=17,
                overwrite=TRUE)

##Extracts summary snow variables. Takes files formatted by clean_soil_temps.
snow_summaries <- extract_snow_summaries(input_path=paste(project_dir,"output/soil/clean_unflagged",sep=""),
                        input_metadata_filename=paste(project_dir,"output/soil/metadata_flags_soil.txt",sep=""),
                        output_path=paste(project_dir,"output/soil",sep=""),
                        figure_path=paste(project_dir,"figs/soil",sep=""),
                        output_metadata_filename="metadata_flags_snow.txt",
                        range_threshold=1,max_threshold=2,overwrite=TRUE)

##Interpolates time-series to hourly intervals and merges files from the same location. Takes files
##formatted by format_micro_csv.
merge_micro_csv(input_path=paste(project_dir,"output/soil/clean_unflagged",sep=""),
               file_metadata_path=paste(project_dir,"output/soil/metadata_flags_soil.txt",sep=""),
               file_metadata_join_column="filestem",
               sensor_metadata_path=paste(project_dir,"data/test_data/sensor_locations.csv",sep=""),
               sensor_metadata_join_column="combined_name",
               output_path = paste(project_dir,"output/soil/merged_hourly/",sep=""),
               output_metadata_path=paste(project_dir,"output/soil/merged_hourly/metadata.txt",sep=""),
               figure_path=paste(project_dir,"figs/soil/merged_hourly",sep=""),
               hour_begin=as.POSIXct("2014-01-01 00:00 PDT"),
               hour_end=as.POSIXct("2016-08-01 00:00 PDT"),
               tzone="Etc/GMT-7",
               interp_gap_max=8,
               overwrite=TRUE)

##Computes daily summary statistics for each location, including snow cover. Takes files formatted by
##merge_micro_csv.
summarise_soiltemp_daily(input_path=paste(project_dir,"output/soil/merged_hourly",sep=""),
                     output_path=paste(project_dir,"output/soil/summarised_daily",sep=""),
                     snow_range_thresh=1,
                     snow_maxt_thresh=2,
                     overwrite=TRUE)

##Merges all daily measurements into a single file. Takes files formatted by summarise_soil_daily.
soiltemp_data <- compile_soiltemp_daily(input_path=paste(project_dir,"output/soil/summarised_daily",sep=""),
                     output_file_snow=paste(project_dir,"output/soil/merged_daily_2014_2015_snow.csv",sep=""),
                     output_file_tmin=paste(project_dir,"output/soil/merged_daily_2014_2015_smin.csv",sep=""),
                     output_file_tmax=paste(project_dir,"output/soil/merged_daily_2014_2015_smax.csv",sep=""),
                     start_date=as.Date("2014-09-01"),
                     end_date=as.Date("2015-09-15"),
                     add_summer_zero=TRUE,
                     overwrite=TRUE,
                     return_data=TRUE)

##Plots time-series to check alignment.
alignment_plot(data_df=soiltemp_data$tmin,
               year_seq=2014:2015,
               min_month="01-01",
               max_month="12-31",
               min_y=-2,max_y=14,
               col_subset="all",
               ID_text=FALSE)

####Air temperature data processing####

##Paths of folders to process (all must be absolute paths)
input_folders_air <- paste(project_dir,c("data/test_data/air_2014/","data/test_data/air_2015/"),
                           sep="")

##Gets files in a common format.
format_micro_csv(input_paths=input_folders_air,
                   output_path=paste(project_dir,"temp/air",sep=""),
                   file_prefixes=c("Ian_air","Ian_air"),
                   output_metadata_filename="metadata_air.txt",overwrite=TRUE)

##Remove spikes and unphysical values.
clean_air_temps(input_path=paste(project_dir,"temp/air",sep=""),
               input_metadata_filename="metadata_air.txt",
               output_path=paste(project_dir,"output/air",sep=""),
               output_metadata_filename="metadata_flags_air.txt",
               figure_path=paste(project_dir,"figs/air",sep=""),
               guess_tz="Etc/GMT-7",temp_spike_thresh=10,
               min_temp_thresh=-20,max_temp_thresh=50,max_temp_hr=17,
               overwrite=TRUE)

##Merges time-series for known sensor locations.
merge_micro_csv(input_path=paste(project_dir,"output/air/clean_unflagged",sep=""),
          file_metadata_path=paste(project_dir,"output/air/metadata_flags_air.txt",sep=""),
          file_metadata_join_column="filestem",
          sensor_metadata_path=paste(project_dir,"data/test_data/sensor_locations.csv",sep=""),
          sensor_metadata_join_column="combined_name",
          output_path = paste(project_dir,"output/air/merged_hourly/",sep=""),
          output_metadata_path=paste(project_dir,"output/air/merged_hourly/metadata.txt",sep=""),
          figure_path=paste(project_dir,"figs/air/merged_hourly",sep=""),
          hour_begin=as.POSIXct("2013-01-01 00:00 PDT"),
          hour_end=as.POSIXct("2015-08-01 00:00 PDT"),
          tzone="Etc/GMT-7",
          interp_gap_max=6,
          overwrite=TRUE)

summarise_airtemp_daily(input_path=paste(project_dir,"output/air/merged_hourly",sep=""),
                         output_path=paste(project_dir,"output/air/summarised_daily",sep=""),
                         overwrite=FALSE)

airtemp_data <- compile_airtemp_daily(input_path=paste(project_dir,"output/air/summarised_daily",sep=""),
                     output_file_tavg=paste(project_dir,"output/air/merged_daily_2014_2015_tavg.csv",sep=""),
                     output_file_tmin=paste(project_dir,"output/air/merged_daily_2014_2015_tmin.csv",sep=""),
                     output_file_tmax=paste(project_dir,"output/air/merged_daily_2014_2015_tmax.csv",sep=""),
                     start_date=as.Date("2014-09-01"),
                     end_date=as.Date("2015-09-15"),
                     overwrite=TRUE,
                     return_data=TRUE)

##Plots time-series to check alignment.
alignment_plot(data_df=airtemp_data$tmin,
               year_seq=2014:2015,
               min_month="01-01",
               max_month="12-31",
               min_y=-10,max_y=20,
               col_subset="all",
               ID_text=FALSE)

