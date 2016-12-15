####Script to test HRL microclimate functions.
####Author: Ian Breckheimer
####Date: 15 December 2016

require(data.table)
setwd("~/code/HRL_microclimate/")

####Soil temperature data processing####

##Paths of folders to process (all must be absolute paths)
input_folders_soil <- c("~/code/HRL_microclimate/data/test_data/soil_2014/",
                   "~/code/HRL_microclimate/data/test_data/soil_2015/")

batch_format_micro_csv(input_paths=input_folders_soil,
                       output_path="~/code/HRL_microclimate/temp/soil",
                       file_prefixes=c("Ian_soil","Ian_soil"),
                       output_metadata_filename="metadata_soil.txt",overwrite=FALSE)

batch_extract_snow_vars(input_path="~/code/HRL_microclimate/temp/soil",
                        input_meta_filename="metadata.txt",
                        output_path="~/code/HRL_microclimate/output/soil",
                        figure_path="~/code/HRL_microclimate/figs/soil",
                        output_meta_filename="metadata_snow.txt",
                        range_threshold=1,max_threshold=2,overwrite=FALSE)

####Air temperature data processing####

##Paths of folders to process (all must be absolute paths)
input_folders_air <- c("~/code/HRL_microclimate/data/test_data/air_2014/",
                        "~/code/HRL_microclimate/data/test_data/air_2015/")

batch_format_micro_csv(input_paths=input_folders_air,
                       output_path="~/code/HRL_microclimate/temp/air",
                       file_prefixes=c("Ian_air","Ian_air"),
                       output_metadata_filename="metadata_air.txt",overwrite=FALSE)

batch_clean_air_temps(input_path="~/code/HRL_microclimate/temp/air",
                     input_metadata_name="metadata_air.txt",
                     output_path="~/code/HRL_microclimate/output/air",
                     output_metadata_name="metadata_flags_air.txt",
                     figure_path="~/code/HRL_microclimate/figs/air",
                     guess_tz="Etc/GMT-7",temp_spike_thresh=10,
                     min_temp_thresh=-20,max_temp_thresh=50,max_temp_hr=17,
                     overwrite=TRUE)
