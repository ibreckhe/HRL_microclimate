##Functions for formatting and processing microclimate data.
##Author: Ian Breckheimer
##Created: 16 February 2018

####Function to format an individual HOBO or ibutton file and extract metadata.####

##USAGE: None, internal function typically called by merge_micro_csv.
##ARGS: csv_name - The path to the file that should be processed.

micro_csv <- function(csv_name) {

  ## Checks to make sure that the file exists in the current working directory.
  stopifnot(file.exists(csv_name))
  print(paste("Now processing file ",csv_name))
  
  ## Reads in the beginning of the file to check the formatting.
  header <- scan(csv_name,nlines=30,what='raw',quiet=TRUE) 
  
  is.formatted <- tolower(header[1]) %in% c("year,month,day,hour,temp,light",
                                            "year,month,day,hour,temperature",
                                            "year,month,day,hour,temp",
                                            "year,month,day,hour,temp,",
                                            "year,month,day,hour,temp,,",
                                            "year,month,day,hour,temp,,,",
                                            "year,month,day,hour,temp,,,,",
                                            "year,month,day ,hour,temp",
                                            "year,month,day,hour,value",
                                            "year,month,day,hour,temp,light,,",
                                            "year,month,day,hour,temp,light,,,",
                                            "year,month,day,hour,temp,light,,,,",
                                            "year,month,day,hour,temp,light,,,,,,,",
                                            "year,month,day,hour,temp,intensity",
                                            "year,month,day,hour,temp,temp")
  
  is.datetime <- unlist(strsplit(header[1],split=","))[1] == "Date/Time"
  is.ibutton <- any(grepl("iButton", header))  # determine whether the sensor is an ibutton or not
  is.hobo <- any(grepl("Plot", header)) && any(grepl("Time", header)) && any(grepl("Title", header))
  if (is.formatted == TRUE){
    file <- read.table(csv_name,skip=1,sep=",")
    DateTime <- paste(file[,2],"/",file[,3],"/",file[,1]," ",file[,4],":00",sep="")
    file <- cbind(DateTime,file[-c(1:4)],NA)
  }
  else if (is.datetime) {
    table <- read.table(csv_name,skip=1,header=FALSE,sep=",")  # remove rows from the top of the data frame
    DateTime <- strptime(table[,1],format="%m/%d/%y %H:%M")
    temp <- as.vector(table[,2])
    file <- data.frame(DateTime,temp,light=NA)
  }
  else if (is.ibutton == TRUE) {   
    # do this if the sensor is an ibutton do this if the ibutton is model DS1921G
    if (any(grepl("DS1921G", header)) == TRUE) {
      file <- read.table(csv_name,skip=15,header=FALSE,sep=",")  # remove rows from the top of the data frame
    }
    if (any(grepl("DS1922L",header)) == TRUE) {
      # do this if the ibutton is model DS1922L
      file <- read.table(csv_name,skip=22,header=FALSE,sep=",")  # remove rows from the top of the data frame
    }
    if (any(grepl("DS2422",header)) == TRUE) {
      # do this if the ibutton is model DS1922
      file <- read.table(csv_name,skip=20,header=FALSE,sep=",")  # remove rows from the top of the data frame
    }
    if (any(grepl("DS1923",header)) == TRUE) {
      # do this if the ibutton is model DS1923
      file <- read.table(csv_name,skip=20,header=FALSE,sep=",")  # remove rows from the top of the data frame
    }
    # do this if there is no header.
    if (class(file) == "factor") {
      if (file[1] == "Date/Time") {
        file = file[-c(1:3)]
      }
      
      if (file[1] == "Unit") {
        file = file[-c(1:2)]
      }
      
      if (file[1] == "Value") {
        file = file[-1]
      }
      
      file = as.character(file)
      tmp.file <- matrix(data = file, ncol = 3, nrow = length(file)/3, byrow = TRUE)  # take data (which is currently in vector format) and convert to matrix format
      file <- as.data.frame(tmp.file)  # convert to data frame
      file <- file[, -2]  # remove the second column (unit)
    }
    if (dim(file)[2] == 1) {
      file = as.character(file)
      tmp.file <- matrix(data = file, ncol = 3, nrow = length(file)/3, byrow = TRUE)  # take data (which is currently in vector format) and convert to matrix format
      file <- as.data.frame(tmp.file)  # convert to data frame
      file <- file[, -2]  # remove the second column (unit)
    }
    if (dim(file)[2] == 2) {
      file = file
    }
    if (dim(file)[2] >= 3) {
      file <- file[, -2]  # remove the second column (unit), and any other columns
    }
    
    light.fill <- array("", dim = c(dim(file)[1], 1))  # make a vector of NAs to fill the light column (ibuttons don't record light)
    file[, 3] = light.fill  # add the vector to the data frame
    
  }  # end ibutton-specific procedure
  else if (is.hobo==TRUE) {
    # do this if the sensor is NOT an ibutton, and is a HOBO
    sample <- header[c((length(header)-10):length(header))]
    sample_split <- unlist(strsplit(sample,split=","))
    is.comma.del <- length(sample)!=length(sample_split)
    if(is.comma.del) {
      file <- read.table(csv_name, header = F, sep = ",",skip=2,fill=TRUE)  # read in the raw data .csv file (as comma delimited) as a dataframe
    }
    else{
      file <- read.table(csv_name,header = F, sep="\t",skip=2,fill=TRUE)    # read in the raw data .csv file (as tab delimited) as a dataframe
    }
    if(ncol(file)<4){
      file[,4] <- NA
    }
    file <- file[,2:4]  # remove all columns except 2 and 3 (date/time and temperature)
  }  # end HOBO-specific procedure
  else{
    print(paste("Could not recognize the formatting of ",csv_name))
  }
  
  names(file) <- c("DateTime", "Temperature", "Light")  # name the columns
  
  # Attempts to extract the time-zone from the header.
  if(any(grepl("GMT-07:00", header,fixed=T)) | any(grepl("PDT", header,fixed=T)) | any(grepl("PST", header,fixed=T))){
    tz <- "US/Pacific-New"
  }
  else if(any(grepl("GMT-00:00", header,fixed=T))){
    tz <- "GMT"
  }
  else{
    tz <- "UNK"
  }
  
  # Tests for a valid 4-digit year
  valid_years <- 2007:2050
  datestring <- strsplit(as.character(file$DateTime[1]),split=" ")[[1]][1]
  yeartest <- function(x){any(grepl(as.character(x),datestring))}
  valid.year <- any(sapply(valid_years,FUN=yeartest))
  am.pm <- any(grepl("PM",file$DateTime[1:20]))
  
  # Converts date vector to separate columns for year,month,day,and hour.
  if(class(file$DateTime)[1]=="POSIXct"){
    dateTime <- file$DateTime
  }else if(am.pm && valid.year) {
    dateTime <- strptime(file$DateTime, "%m/%d/%Y %r")
  }else if(am.pm==FALSE && valid.year==TRUE) {
    dateTime <- strptime(file$DateTime, "%m/%d/%Y %H:%M")
  }else if(am.pm==TRUE && valid.year==FALSE) {
    dateTime <- strptime(file$DateTime, "%m/%d/%y %r")
  }else{
    dateTime <- strptime(file$DateTime, "%m/%d/%y %H:%M")
  }
  
  YEAR <- as.numeric(strftime(dateTime,format="%Y"))
  MONTH <- as.numeric(strftime(dateTime,format="%m"))
  DAY <- as.numeric(strftime(dateTime,format="%d"))
  HOUR <- as.numeric(strftime(dateTime,format="%H"))
  MIN <- as.numeric(strftime(dateTime,format="%M"))
  TEMP <- as.numeric(file$Temperature)
  file <- cbind(YEAR,MONTH,DAY,HOUR,MIN,TEMP)
  
  # Extracts the range of dates in the file.
  date_min <- min(dateTime,na.rm=T)
  date_max <- max(dateTime,na.rm=T)
  date_lab <- paste(strftime(date_min,format="%Y-%m"),strftime(date_max,format="%Y-%m"),sep="-")
  
  # Measures the logging interval
  log_int <- dateTime[2] - dateTime[1]
  
  # Extracts the minimum and maximum temperature.
  temp_max <- max(TEMP,na.rm=T)
  temp_min <- min(TEMP,na.rm=T)
  
  # Extracts the number of measurements.
  n_measurements <- length(na.omit(TEMP))
  
  file_attrib <- list(data=file,
                      filestem=strsplit(csv_name,split=".csv")[[1]],
                      filepath=paste(getwd(),csv_name,sep="/"),
                      n_measurements=n_measurements,
                      log_interval=log_int,
                      date_min=date_min,
                      date_max=date_max,
                      date_lab=date_lab,
                      temp_min=temp_min,
                      temp_max=temp_max,
                      hobo=is.hobo,
                      ibutton=is.ibutton,
                      formatted=is.formatted,
                      datetime=is.datetime,
                      tz=tz)
  
  return(file_attrib)
}


####Function to batch-format mixed ibutton/HOBO microclimate files in a bunch of directories.####

##USAGE: This function batch-formats a collection of (possibly ill-formatted)
##comma-delimited text files representing microclimate measurements. This
##function is primarily used on files that were exported from Onset HOBO
##and OneWire viewer software.

##ARGS: input_paths - a vector of (absolute) paths to directories where 
##                      files are located.
##       output_path - path to a directory where output files should be located.
##       file_prefixes - a vector of names to append to the files from each input path,
##                       useful for keeping track of files from different studies.
##       output_metadata_filename - the name of the file to hold metadata for each
##                       processed file.
##       overwrite - whether or not to overwrite output files if they exist.

format_micro_csv <- function(input_paths = getwd(),
                             output_path = NULL,
                             file_prefixes = NULL,
                             output_metadata_filename="metadata.txt",
                             overwrite=FALSE){
  
  ##Checks inputs
  stopifnot(length(input_paths)==length(file_prefixes))
  stopifnot(length(output_metadata_filename)==1)
  stopifnot(all(dir.exists(input_paths)))
  stopifnot(dir.exists(output_path))
  stopifnot(is.logical(overwrite) & length(overwrite)==1)
  
  ##Deletes the existing metadata file if it exists
  setwd(output_path)
  if(file.exists(output_metadata_filename)){
    file.remove(output_metadata_filename)
  }
  
  ##Checks to see how many input files there are.
  csv_all <- c()
  for (i in 1:length(input_paths)){
    csv_files <- list.files(input_paths[i],pattern=".csv$")
    csv_all <- c(csv_all,csv_files)
  }
  nfiles <- length(csv_all)
  print(paste("Now processing ",nfiles," microclimate files."))
  
  ##Sets up a file counter.
  file_n <- 0
  
  for (i in 1:length(input_paths)){
    setwd(input_paths[i])
    flush.console()
    print(paste("Now processing files in folder ",input_paths[i]))
    files <- list.files(".",pattern=".csv$") # lists all the .csv files in the working directory 
    dfs <- lapply(X = files, FUN = micro_csv)  # process all .csv files in the working directory into a list of lists.
    
    ## write the modified files as .csv files to an output folder
    
    # set the working directory to the folder that will collect the output files
    setwd(output_path)
    outfiles <- length(list.files())
    if(outfiles>0){
      print(paste("Output folder already contains ", outfiles,"files."))
    }
    
    for (j in 1:length(dfs)) {
      datavals <- dfs[[j]]$data
      in_name <- dfs[[j]]$filestem
      out_name <- paste(paste(file_prefixes[i],in_name,dfs[[j]]$date_lab,sep="_"),".csv",sep="")
      meta <- data.frame(out_filename=out_name,dfs[[j]][-1])
      if(overwrite==TRUE & file.exists(out_name)){
        print(paste("Overwriting file", out_name))
        write.csv(datavals, file = out_name, row.names = FALSE)
      }else if(overwrite==FALSE & file.exists(out_name)){
        print(paste("Skipping existing file", out_name))
      }else{
        print(paste("Writing file",out_name))
        write.csv(datavals, file = out_name, row.names = FALSE)
      }
      
      # Writes the metadata, appending rows to an existing file if it already exists.
      if (length(list.files(".",pattern=output_metadata_filename)) == 0){
        write.table(meta,file=output_metadata_filename, sep=",",row.names = FALSE, append = FALSE) 
      }
      else{
        write.table(meta,file=output_metadata_filename, sep=",",row.names = FALSE, 
                    col.names = FALSE, append = TRUE)
      }
      file_n <- file_n+1
      print(paste("Completed processing file ",file_n," of ",nfiles))
    }
    
  }
  
  ##Returns output metadata
  setwd(output_path)
  meta_output <- read.csv(output_metadata_filename)
  return(meta_output)
}


####Function to merge cleaned microclimate files together if they are from the same location####

##USAGE: This function merges or concantates a bunch of individual cleaned time-series
##of microclimate files into a single time-series, in the process, interpolating
##each time-series to have a single measurement at the beginning of each hour.
## The function knows which files to concantate because each input file must have
##a record in the file metadata input that specifies which sensor the file is associated with.
##this unique code must also be represented in the sensor_metadata, which also has
##additional metadata for each sensor (site location, for example). Warnings are
##generated if some input files can't be matched to a known sensor site.

##ARGS: input_path - absolute path to directory containing input files,

##      file_metadata_path - absolute path to a .csv-formatted file describing each input file,
##                             must have a column named "out_filename" which contains the names
##                            of the input files.

##      file_metadata_join_column - the column header in the file metadata that designates which
##                                which site the file is associated with.

##      sensor_metadata_path - absolute path to a file with metadata for each sensor location.

##      sensor_metadata_join_column - the column header in the sensor metadata that matches
##                                    the name of the sites in the file metadata.

##      output_path - absolute path to a directory to hold output files

##      output_metadata_path - absolute path to a file that holds information about the output
##                            joined time-series.

##      figure_path - absolute path to a directory to hold plots of merged data

##      hour_begin - POSIXct-formatted date and time to start each time-series. This should be before 
##                   the beginning of the first time-series unless you want the output to
##                   be trimmed.

##      hour_end - POSIXct-formatted date and time to end each time-series. This should be after
##                   the end of the last time-series unless you want the output to
##                   be trimmed.

##      tzone - time-zone of the output time-series. Should match a named timzone returned
##              by the OlsenNames() function.

##      interp_gap_max - the maximum number of hours to fill missing data, gaps are filled
##                       by cubic spline interpolation (xts function NA.spline)

##      merge_fun - if time-series have simultaneous measurements, the function used to
##                    combine them. Should be a string "first","min",or "max"

##      return_metadata - if TRUE, the function returns metadata to the R environment,

##      overwrite - if TRUE, overwrite existing files.

merge_micro_csv <- function(input_path=NULL,
                            file_metadata_path=NULL,
                            file_metadata_join_column="plot",
                            sensor_metadata_path=NULL,
                            sensor_metadata_join_column="combined_name",
                            output_path=NULL,
                            output_metadata_path=NULL,
                            figure_path=NULL,
                            hour_begin=as.POSIXct("2001-01-01 00:00 PDT"),
                            hour_end=as.POSIXct("2020-08-01 00:00 PDT"),
                            tzone="Etc/GMT-7",
                            interp_gap_max=8,
                            merge_fun="min",
                            return_metadata=TRUE,
                            overwrite=TRUE){
  ##Required package.
  require(xts)
  
  ##Checks inputs.
  stopifnot("POSIXct" %in% class(hour_begin))
  stopifnot("POSIXct" %in% class(hour_end))
  stopifnot(tzone %in% OlsonNames())
  stopifnot(class(interp_gap_max)=="numeric",
            length(interp_gap_max)==1,
            interp_gap_max > 0,
            interp_gap_max==round(interp_gap_max,digits=0))
  stopifnot(file.exists(file_metadata_path))
  stopifnot(dir.exists(input_path))
  stopifnot(dir.exists(figure_path))
  stopifnot(dir.exists(output_path))
  stopifnot(merge_fun %in% c("first","min","max"))
  
  ##Checks format of input metadata.
  metadata_complete <- read.csv(file_metadata_path)
  stopifnot(file_metadata_join_column %in% colnames(metadata_complete))
  stopifnot("out_filename" %in% colnames(metadata_complete))
  stopifnot(all(!is.na(metadata_complete[,c(file_metadata_join_column)])))
  stopifnot(all(!is.na(metadata_complete$out_filename)))
  input_files <- list.files(input_path,pattern=".csv$")
  stopifnot(all(input_files %in% metadata_complete$out_filename))
  
  ##Drops metadata records that don't correspond to input files.
  metadata_files <- unique(subset(metadata_complete,out_filename %in% input_files))
  
  ##Checks format of sensor metadata.
  sensors <- read.csv(sensor_metadata_path)
  stopifnot(sensor_metadata_join_column %in% colnames(sensors))
  stopifnot(all(!is.na(sensors[,c(sensor_metadata_join_column)])))
  
  ##joins file records to metadata.
  metadata_files_all <- merge(metadata_files,sensors,by.x=file_metadata_join_column,
                              by.y=sensor_metadata_join_column,all.x=TRUE)
  metadata_files_matched <- metadata_files_all[!is.na(metadata_files_all[,file_metadata_join_column]),]
  
  unmatched_files <- metadata_files_all$out_filename[is.na(metadata_files_all$site_name)]
  if(length(unmatched_files)>0){
    print(paste("Could not match",length(unmatched_files),"files.","skipping \n",unmatched_files))
  }
  
  ##Auto-matching of filenames to sensor locations, currently doesn't work very well.
  # unknown_names <- sensors[,c(sensor_metadata_join_column)]
  # known_names_upr <- toupper(known_names)
  # guessnames <- toupper(metadata_complete[,c(file_metadata_join_column)])
  # filenames <- list.files(input_path,pattern=".csv$")
  # any_name_matches <- matrix(NA,nrow=length(known_names),ncol=length(guessnames))
  # 
  # for(i in 1:length(filenames)){
  #   for(j in 1:length(known_names)){
  #     any_name_matches[j,i] <- grepl(pattern=known_names_upr[j],x=guessnames[i],
  #                                      fixed=TRUE)
  #   }
  # }
  # file_nmatches <- colSums(any_name_matches)
  # name_nmatches <- rowSums(any_name_matches)
  # matched_names <- known_names[name_nmatches>0]
  # if(all(file_nmatches==1)){
  #   print(paste("All filenames are matched to a sensor site."))
  # }
  # if(any(file_nmatches>1)){
  #   stop(paste("Some filenames ambiguous. Multiple matches for \n",filenames[which(file_nmatches > 1)],
  #               "in \n",sensor_metadata_path))
  # }
  # if(any(file_nmatches<1)){
  #   stop(paste("Some filenames not matched. No matches for \n",filenames[which(file_nmatches > 1)],
  #               "in \n",sensor_metadata_path))
  # }
  
  hour_seq <- seq(hour_begin,hour_end,by="hour")
  na_seq <- rep(NA,length(hour_seq))
  empty_xts <- xts(na_seq,order.by=hour_seq,tzone=tzone)
  
  matched_names <- as.character(unique(metadata_files_matched[,file_metadata_join_column]))
  
  ##Sets up empty vectors for metadata.
  out_meta <- data.frame(merged_sitename = matched_names,
                          input_files = rep(NA,length(matched_names)),
                          n_input_files = rep(NA,length(matched_names)),
                          n_meas = rep(NA,length(matched_names)),
                          output_filename = rep(NA,length(matched_names)),
                          start_datetime = rep(NA,length(matched_names)),
                          end_datetime = rep(NA,length(matched_names)))
  
  ##Loops through each site and merges time-series.
  for(i in 1:length(matched_names)){
    sensor_files <- metadata_files_matched[metadata_files_matched[,file_metadata_join_column]==matched_names[i],"out_filename"]
    
    print(paste("Merging",length(sensor_files),"time-series for sensor site",matched_names[i],
                "(",i,"of",length(matched_names),")"))
    series <- read.csv(paste(input_path,"/",sensor_files[1],sep=""),header=TRUE)
    series$datetime <- as.POSIXct(series$DATE,tz=as.character(series$TZ[1]),format=c("%Y-%m-%d %H:%M"))
    series_xts <- xts(series$TEMP,order.by=series$datetime,tzone=tzone)
    series_merged <- merge(empty_xts,series_xts,join="outer")[,2]
    series_filled <- na.spline(series_merged,maxgap=interp_gap_max,na.rm=FALSE)
    series_hrly <- merge(empty_xts,series_filled,join="left")[,2]
    if(length(sensor_files)>1){
      for(k in 2:length(sensor_files)){
        series_new <- read.csv(paste(input_path,"/",sensor_files[k],sep=""),header=TRUE)
        series_new$datetime <- as.POSIXct(series_new$DATE,tz=as.character(series_new$TZ[1]),format=c("%Y-%m-%d %H:%M"))
        series_new_xts <- xts(series_new$TEMP,order.by=series_new$datetime)
        series_new_filled <- na.spline(merge(empty_xts,series_new_xts,join="outer")[,2],
                                       maxgap=interp_gap_max,na.rm=FALSE)
        series_new_hrly <- merge(empty_xts,series_new_filled,join="left")[,2]
        series_hrly <- merge(series_hrly,series_new_hrly,join="outer")
        if(merge_fun=="first"){
          series_hrly[is.na(series_hrly[,1]),1] <- series_hrly[is.na(series_hrly[,1]),2]
        }else if(merge_fun=="min"){
          series_hrly[,1] <- pmin(series_hrly[,1],series_hrly[,2],na.rm=TRUE)
        }else if(merge_fun=="max"){
          series_hrly[,1] <- pmax(series_hrly[,1],series_hrly[,2],na.rm=TRUE)
        }
        series_hrly <- series_hrly[,1]
      }
    }
    series_interp <- na.spline(series_hrly,maxgap=interp_gap_max,na.rm=FALSE)
    hour_top <- as.numeric(format(index(series_interp),format="%M")) == 0
    series_interp <- series_interp[which(hour_top)]
    has_data <- which(!is.na(series_interp))
    start_index <- min(has_data)
    start_datetime <- index(series_interp)[start_index]
    if(hour_begin > start_datetime){
      start_datetime <- hour_begin
      start_index <- which(index(series_interp)==start_datetime)
    }
    end_index <- max(has_data)
    end_datetime <- index(series_interp)[end_index]
    if(hour_end < end_datetime){
      end_datetime <- hour_end
      end_index <- which(index(series_interp)==end_datetime)
    }
    series_out <- series_interp[start_index:end_index]
    df_out <- data.frame(DATETIME=index(series_out),TZ=tzone(series_out),TEMP=as.numeric(series_out))
    fig_name <- paste(figure_path,"/",matched_names[i],"_",
                      gsub("-","_",as.character(as.Date(start_datetime))),"_",
                      gsub("-","_",as.character(as.Date(end_datetime))),
                      "_hourly.pdf",sep="")
    if(file.exists(fig_name) & overwrite==FALSE){
      print(paste("Figure path already exists, skipping..."))
    }else{
      pdf(fig_name,width=20,height=5)
      plot(df_out$TEMP~df_out$DATETIME,main=matched_names[i],ylim=c(-20,50),
           ylab="Temp (C)",xlab="Date")
      abline(0,0,lty=2)
      dev.off()
    }
    
    outfile <- paste(output_path,"/",matched_names[i],"_",
                     gsub("-","_",as.character(as.Date(start_datetime))),"_",
                     gsub("-","_",as.character(as.Date(end_datetime))),
                     "_hourly.csv",sep="")
    
    ##Prepares metadata.
    out_meta$input_files[i] <- paste(sensor_files,collapse=" ")
    out_meta$n_input_files[i] <- length(sensor_files)
    out_meta$n_meas[i] <- nrow(df_out)
    out_meta$output_filename[i] <- outfile
    out_meta$start_datetime[i] <- as.character(min(index(series_out)))
    out_meta$end_datetime[i] <- as.character(max(index(series_out)))

    if(file.exists(outfile) & overwrite==FALSE){
      print(paste("Output file already exists, skipping..."))
    }else{
      write.csv(df_out,file=outfile,row.names=FALSE)
    }
  }
  out_meta <- merge(out_meta,sensors,by.x="merged_sitename",
                    by.y=sensor_metadata_join_column,
                    all.x=TRUE)
  write.csv(out_meta, file=output_metadata_path,
            row.names=FALSE)
  if(return_metadata==TRUE){
    return(out_meta)
  }
}


####Function to estimate snow cover duration from a series of .csv microclimate files.####

##USAGE: This function extracts snow-cover and soil temperature information from a collection
##of cleaned soil temperature time-series.

##ARGS: input_path - absolute path to directory containing input files

##      input_metadata_filename - path to a file describing input files.

##      figure_path - absolute path to directory to hold plots of data,

##      output_path - absolute path to directory to hold cleaned output files,

##      output_metadata_filename - path to a file describing output files,

##      range_threshold - the maximum diurnal temperature range considered snow-covered

##      max_threshold - the highest maximum daily temperature of snow-covered days,

##      overwrite - if TRUE, overwrite input files.

extract_snow_summaries <- function(input_path = NULL,
                                  input_metadata_filename="metadata.txt",
                                  figure_path = NULL,
                                  output_path = NULL,
                                  output_metadata_filename = NULL,
                                  range_threshold=1,
                                  max_threshold=2,
                                  overwrite=FALSE){
  ##Loads required packages
  require(data.table)
  
  ##Checks inputs
  stopifnot(dir.exists(input_path))
  stopifnot(dir.exists(figure_path))
  stopifnot(dir.exists(output_path))
  
  setwd(input_path)
  stopifnot(file.exists(input_metadata_filename))
  stopifnot(!file.exists(output_metadata_filename)|overwrite==TRUE)
  
  ##Reads in list of input files and metadata.
  files <- list.files(".",pattern=".csv$")  # the data files for each temperature sensor to be analyzed
  meta <- read.table(input_metadata_filename,sep=",",header=TRUE)
  
  ##Sets up empty vectors to hold output.
  calibration <- c()
  calibration.type <- c()
  stand <- c()
  plot <- c()
  year <- c()
  snow_appearance_date <- c()
  snow_disappearance_date <- c()
  snow_cover_duration <- c()  # in days
  minimum_soil_temp <- c()
  
  # start the clock to record how long the code takes to run
  start_t <- Sys.time()
  
  nfiles <- length(files)
  for (k in 1:nfiles) {
    
    ##Makes sure we are in the right directory
    setwd(input_path)
    
    ##Prints progress.
    flush.console()
    print(paste("Now Processing file: ",files[k],"(",k," of",nfiles,")"))
    
    ############################################## READING IN DATA FROM ONE FILE
    
    d <- read.csv(files[k])
    d <- d[complete.cases(d[, 1:3]), ]
    
    # Name the columns
    names(d) <- c("DATE","TZ","TEMP")
    
    d$Date <- as.Date(as.POSIXct(d$DATE))
    d$DOY <- as.numeric(format(d$Date, format = "%j"))  # find the unique days
    d$YEAR <- as.numeric(format(d$Date, format = "%Y"))
    d <- data.table(d,key="Date")
    
    ######################################## 
    
    # EXTRACT STAND INFO FROM FILENAME
    
    stand[k] <- strsplit(files[k], "_")[[1]][2]
    plot[k] <- strsplit(files[k], "_")[[1]][3]
    year[k] <- max(d$YEAR)
    
    ############################################### 
    
    # EVALUATE SNOW COVER CRITERIA
    
    # Create an empty data table to store values
    days <- unique(d$Date)
    ndays <- length(days)
    
    daily <- data.table(date=unique(d$Date),
                        range=rep(NA,ndays),
                        mean=rep(NA,ndays),
                        rangethresh=rep(NA,ndays),
                        maxthresh=rep(NA,ndays),
                        snow=rep(NA,ndays))
    setkey(daily,"date")
    
    # Calculate the mean daily temperature and temperature range for each day:
    daily[[2]] <- d[,diff(range(TEMP)),by=Date][[2]]
    daily[[3]] <- d[,mean(TEMP,na.rm=T),by=Date][[2]]
    daily[[4]] <- d[,diff(range(TEMP)) < range_threshold,by=Date][[2]]
    daily[[5]] <- d[,max(TEMP) < max_threshold,by=Date][[2]]
    
    ############################################## 
    
    # DETERMINE CALIBRATION TEMPERATURE
    calibration.temp <- mean(daily$mean[daily$rangethresh == TRUE])  # calculate the mean temp for all the days that the temp didn't exceed range_threshold
    calibration[k] <- ifelse(!is.na(calibration.temp), yes = calibration.temp, no = 0)  # If NA, set calibration = 0
    d$TEMP.calib <- d$TEMP - calibration[k]  # recalibrate data 
    
    ################################################ 
    
    ## EVALUATE WHETHER OR NOT SNOW COVERED THE SENSOR ON EACH DATE BASED ON THE ABOVE TWO CRITERIA
    
    daily[[6]] <- daily[[4]] & daily[[5]]  # will store the algorithm's evaluation of snow cover for each date (1=snow present, 0=snow absent)
    d.snow <- subset(daily,snow==TRUE)
    ################################################ 
    
    ## SUMMARIZING
    snow_appearance_date[k] <- as.character(min(d.snow$date))  # first day when snow covered sensor
    snow_disappearance_date[k] <- as.character(max(d.snow$date))  # last day when snow covered sensor
    snow_cover_duration[k] <- sum(d.snow$snow)  #'snow cover duration' the total number of days with snow cover
    minimum_soil_temp[k] <- min(d$TEMP,na.rm=TRUE) #Winter minimum soil temperature
    
    
    ## PLOT SOIL TEMPERATURE AND THE SNOW COVER ALGORITHM OUTPUT TO MAKE SURE OUTPUT IS REASONABLE
    
    # Save figure as pdf
    setwd(figure_path)
    population <- strsplit(files[k], ".csv")[[1]]
    graph.file <- paste(population, ".pdf", sep = "")
    pdf(file = graph.file, width = 10, height = 7)
    
    # Left Y axis
    par(mar = c(4, 6, 3, 6))
    plot(d$Date, d$TEMP.calib, main = population, axes = F, xlab = "", ylab = "", pch = 20, col = "red")
    axis(2, col = "red", col.axis = "red", col.ticks = "red")
    leftY <- expression(paste("Temperature", degree, "C"))
    text(par("usr")[1] - 30, par("usr")[3] + ((par("usr")[3] + par("usr")[4])/2), adj = 0.5, leftY, srt = 90, xpd = TRUE, 
         col = "red")
    
    # Right Y axis
    par(new = TRUE)
    plot(daily$date, daily$snow, pch = 20, col = "blue", ylim = c(0, 1.2), axes = FALSE, xaxt = "n", yaxt = "n", 
         xlab = "", ylab = "")
    axis(4, col = "blue", col.axis = "blue", col.ticks = "blue", yaxp = c(0, 1, 1), labels = F)
    rightY <- paste("Snow cover")
    text(par("usr")[2] + 30, par("usr")[3] + ((par("usr")[3] + par("usr")[4])/2), adj = 0.5, rightY, srt = 270, xpd = TRUE, 
         col = "blue")
    text(par("usr")[2] + 15, 0, 0, srt = 270, xpd = T, col = "blue")
    text(par("usr")[2] + 15, 1, 1, srt = 270, xpd = T, col = "blue")
    
    # X axis
    axis.Date(1, d$Date, at = seq(from = min(d$Date), to = max(d$Date), by = "months"))
    
    dev.off()
    
    ################################################ 
    
  }
  
  ##Computes elapsed time.
  stop_t <- Sys.time()
  print(paste("Run took ",stop_t - start_t))
  
  # Consolidate summarized results for each sensor into one data frame
  output <- data.frame(files, 
                       year, 
                       plot, 
                       calibration, 
                       snow_appearance_date, 
                       snow_disappearance_date,
                       snow_cover_duration,
                       minimum_soil_temp)
  
  ##Match the snow cover information to the sensor metadata.
  out_merged <- unique(merge(meta,output,by.x="out_filename",by.y="files",all.x=T))
  
  ##Data quality flags:
  end_doy <- as.numeric(format(as.Date(out_merged$date_max),format="%j"))
  out_merged$flag_sensor_fail <- ((as.Date(out_merged$snow_disappearance_date) - as.Date(out_merged$date_max)) >= -1) & end_doy < 152 
  out_merged$flag_temp_high <- out_merged$temp_max > 90
  out_merged$flag_temp_low <- out_merged$temp_min < -30
  out_merged$flag_high_calib <- out_merged$calibration >= 2 | out_merged$calibration <= -2
  out_merged$flag_no_snow <- out_merged$snow_cover_duration <= 14
  out_merged$flag_short_record <- as.Date(out_merged$date_max) -  as.Date(out_merged$date_min) < 100
  out_merged$flagged <- with(out_merged, flag_sensor_fail | flag_temp_high | flag_temp_low | flag_high_calib | flag_no_snow | flag_short_record)
  
  ##Moves .csv and .pdf graphics of flagged files to new directories
  out_flagged <- out_merged[out_merged$flagged==TRUE,]
  flagged_csvs <- out_flagged$out_filename
  setwd(output_path)
  dir.create("./snow_flagged")
  file.copy(paste(input_path,"/",as.character(flagged_csvs),sep=""),"./clean_flagged")
  flagged_pdfs <- sub(".csv",".pdf",out_flagged$out_filename)
  setwd(figure_path)
  dir.create("./snow_flagged")
  file.copy(flagged_pdfs,"./snow_flagged")
  file.remove(flagged_pdfs)
  
  ##Moves .csv and .pdf graphics of unflagged files to a new directory
  out_unflagged <- out_merged[out_merged$flagged==FALSE,]
  unflagged_csvs <- out_unflagged$out_filename
  setwd(output_path)
  dir.create("./snow_unflagged")
  file.copy(paste(input_path,"/",as.character(unflagged_csvs),sep=""),"./snow_unflagged")
  unflagged_pdfs <- sub(".csv",".pdf",out_unflagged$out_filename)
  setwd(figure_path)
  dir.create("./snow_unflagged")
  file.copy(unflagged_pdfs,"./snow_unflagged")
  file.remove(unflagged_pdfs)
  
  ##subsets data for the unflagged files
  out_unflagged<- out_merged[out_merged$flagged==FALSE,]
  
  # Save output file
  setwd(output_path)
  write.table(out_unflagged, file = output_metadata_filename, sep = ",", row.names = FALSE)
  return(out_merged)
}



####Function to clean a collection of air temperature files####

##USAGE: Function to clean a collection of formatted air temperature files. Input files must have columns
##YEAR,MONTH,DAY,HOUR and TEMP. The function 1) checks for and corrects AM/PM problems, 2) checks for other
##time-zone problems by calculating the mean hour of temperature maxima, and attempts to fix them by assuming
##other common time-zones. 3) Checks for and corrects Fahrenheit / Celcius problems 4) Removes anomalous spikes
##and values outside of the physical temperature range. 5) eliminates measurements with near-constant temperatures
##near 0 (likely snow/ice-covered days), and measurements near 24C (likely days in climate-controlled lab conditions).

##ARGS: input_path - absolute path to directory containing input files

##      input_metadata_filename - path to file describing input files

##      output_path - path to a directory to hold output files

##      output_metadata_filename - path to a file describing output files

##      figure_path  - a path to hold plots of cleaned data

##      guess_tz - time-zone to guess for input data, must be a known time-zone returned by OlsonNames()

##      out_tz - time-zone for output data, must be a known time-zone returned by OlsonNames

##      tz_tolerance - maximum tolerated difference (in hours) between the hour of maximum temperature
##                    for each file and the hour specified in the max_temp_hr argument. If the difference
##                    is greater than this threshold, then the function will attempt to fix time-zone problems.

##      temp_spike_thresh - maximum tolerated measurement-to-measurement change in temperature. Changes exceeding
##                          this threshold will be removed.

##      min_temp_thresh - minimum temperature threshold. Values less than this will be removed.

##      max_temp_thresh - maximum temperature threshold. Values greater than this will be removed.

##      max_temp_hr - the expected hour of maximum temperature. Usually in the afternoon (hours 13 - 15).

##      cf_test_params - 4-element numeric vector controlling tests for data that is recorded in Fahrenheit.
##                       Measurements are likely in Fahrenheit if the maximum temperature is greater than
##                       [1], the maximum temperature is less than [2], the minimum temperature is greater than
##                       than [3] and the mean temperature is greater than [4].

##      overwrite - if TRUE, overwrite output files if they exist.

clean_air_temps <- function(input_path = NULL,
                            input_metadata_filename = NULL,
                            output_path = NULL,
                            output_metadata_filename = NULL,
                            figure_path = NULL,
                            guess_tz="Etc/GMT-7",
                            out_tz="Etc/GMT-7",
                            tz_tolerance=4,
                            temp_spike_thresh=10,
                            min_temp_thresh=-20,
                            max_temp_thresh=50,
                            max_temp_hr=17,
                            cf_test_params=c(40,100,-5,20),
                            overwrite=TRUE){
  ##Sets up workspace
  require(xts)
  require(psych)
  Sys.setenv(TZ=guess_tz)
  
  ##Checks inputs
  stopifnot(length(input_metadata_filename)==1)
  stopifnot(length(output_metadata_filename)==1)
  stopifnot(dir.exists(input_path))
  stopifnot(dir.exists(output_path))
  stopifnot(dir.exists(figure_path))
  stopifnot(is.logical(overwrite) & length(overwrite)==1)
  setwd(input_path)
  stopifnot(file.exists(input_metadata_filename))
  stopifnot(guess_tz %in% OlsonNames())
  stopifnot(out_tz %in% OlsonNames())
  stopifnot(length(cf_test_params)==4)
  stopifnot("numeric" %in% class(cf_test_params))
    
  ##Checks to see how many input files there are.
  airtemp_files <- list.files(input_path,pattern=".csv$")
  nfiles <- length(airtemp_files)
  
  ##Creates folders for output
  setwd(output_path)
  if(!dir.exists("./clean_flagged")){
    dir.create("./clean_flagged")
  }
  if(!dir.exists("./clean_unflagged")){
    dir.create("./clean_unflagged")
  }
  
  ##Creates folders for output
  setwd(figure_path)
  if(!dir.exists("./clean_flagged")){
    dir.create("./clean_flagged")
  }
  if(!dir.exists("./clean_unflagged")){
    dir.create("./clean_unflagged")
  }
  
  ##Checks to make sure we've got the same number files as we do metadata lines
  setwd(input_path)
  air_meta <- read.table(input_metadata_filename,sep=",",header=TRUE)
  #stopifnot(nrow(air_meta)==nfiles)
  
  print(paste("Now processing ",nfiles," air-temp files."))
  
  ##Sets up a file counter.
  file_n <- 0
  
  ##Creates empty data frame for metadata.
  nfiles <- length(airtemp_files)
  metadata <- data.frame(filename=rep("NA",nfiles),
                         cleaned_min=rep(NA,nfiles),
                         cleaned_max=rep(NA,nfiles),
                         mean_hr_max=rep(NA,nfiles),
                         flag_missing_data=rep(NA,nfiles),
                         flag_wrong_tz=rep(NA,nfiles))
  
  metadata$filename <- as.character(metadata$filename)
  
  for (i in 1:length(airtemp_files)){
    
    # Ensures we are looking at the right directory
    setwd(input_path)
    
    # Prints progress to console.
    flush.console()
    print(paste("Now processing ",airtemp_files[i],". File ",i," of ",
                length(airtemp_files)))
    #Reads data and converts to date and time-series format.
    data <- read.csv(airtemp_files[i],header=TRUE)
    data <- data[complete.cases(data),]
    
    ##Skips if file has no rows.
    if(nrow(data) < 2){
      print(paste(airtemp_files[i], "has less than 2 rows, skipping..."))
      next
    }
    
    ##Checks to make sure data has hours in military time. If not, attempts a correction,
    ##but trims first day observations, because they are ambiguous.
    max_hr <- max(data$HOUR,na.rm=TRUE)
    if(max_hr <= 12){
      print(paste("AM/PM problem, correcting..."))
      data$datestring <- paste(data$YEAR,data$MONTH,data$DAY,data$HOUR,data$MIN,sep="-")
      data$HOUR[duplicated(data$datestring)] <- data$HOUR[duplicated(data$datestring)] + 12
      dates <- as.Date(paste(data$YEAR,data$MONTH,data$DAY,sep="-"))
      first_day <- min(dates)
      first_day_index <- which(dates==first_day)
      data <- data[-first_day_index,]
    }
    
    ##Converts date and time info in input file to POSIXct datetime format, dropping
    ##rows without valid dates and times.
    data$datestring <- paste(data$YEAR,data$MONTH,data$DAY,data$HOUR,data$MIN,sep="-")
    data$datetime <- strptime(data$datestring,
                              format="%Y-%m-%d-%H-%M",tz=guess_tz)
    data <- data[!is.na(data$datetime),]
    
    ##Converts data to a timeseries object recognized by the xts package.
    tempts <- xts(data$TEMP,order.by=data$datetime,tz=guess_tz)
    tzone(tempts) <- out_tz
    
    ## Checks for and corrects Celcius / Fahrenheit problems
    if(max(tempts) > cf_test_params[1] & max(tempts) < cf_test_params[2] &
       min(tempts) > cf_test_params[3] & mean(tempts) > cf_test_params[2]){
      print(paste("Found possible C / F problem, correcting..."))
      tempts <- (tempts - 32) * (5/9)
    }

    ##Checks for time-zone problems by calculating the circadian mean hour of the day with the maximum temperature.
    hr_max_ts <- do.call(rbind, lapply(split(tempts,"days"), function(x) x[which.max(x)]))
    hrs <- as.numeric(format(index(hr_max_ts),format="%H"))
    mean_hr_max <- circadian.mean(hrs,hours=TRUE)
    
    ##Attempts a time-zone correction, accepting if max hour is within tolerance.
    if((mean_hr_max-max_temp_hr) > 4 | (mean_hr_max-max_temp_hr) < -4){
      print(paste("Found possible TZ problem, mean max temp at hour",mean_hr_max))
      data$datestring <- paste(data$YEAR,data$MONTH,data$DAY,sep="-")
      data$timestring <- paste(data$HOUR,data$MIN,"00",sep=":")
      data$datetime <- strptime(paste(data$datestring,data$timestring),
                                format="%Y-%m-%d %H:%M:%S",tz="Etc/GMT+9")
      tempts <- xts(data$TEMP,order.by=data$datetime)
      tzone(tempts) <- out_tz
      hr_max_ts <- do.call(rbind, lapply(split(tempts,"days"), function(x) x[which.max(x)]))
      hrs <- as.numeric(format(index(hr_max_ts),format="%H"))
      mean_hr_max <- circadian.mean(hrs,hours=TRUE)
      print(paste("Assuming GMT+9, mean max temp now at",mean_hr_max))
    }
    ##Tries time-zone correction again if the previous attempt didn't work.
    if((mean_hr_max-max_temp_hr) > tz_tolerance | (mean_hr_max-max_temp_hr) < -1*tz_tolerance){
      print(paste("Found possible TZ problem, mean max temp at hour",mean_hr_max))
      data$datestring <- paste(data$YEAR,data$MONTH,data$DAY,sep="-")
      data$timestring <- paste(data$HOUR,data$MIN,"00",sep=":")
      data$datetime <- strptime(paste(data$datestring,data$timestring),
                                format="%Y-%m-%d %H:%M:%S",tz="UTC")
      tempts <- xts(data$TEMP,order.by=data$datetime)
      tzone(tempts) <- out_tz
      hr_max_ts <- do.call(rbind, lapply(split(tempts,"days"), function(x) x[which.max(x)]))
      hrs <- as.numeric(format(index(hr_max_ts),format="%H"))
      mean_hr_max <- circadian.mean(hrs,hours=TRUE)
      print(paste("Assuming UTC, mean max temp now at",mean_hr_max))
    }
    
    ## Checks for and eliminates values outside of the physical range 
    ## and temperature spikes that could be caused by direct sun.
    tempts[which(tempts < min_temp_thresh)] <- NA
    tempts[which(tempts > max_temp_thresh)] <- NA
    tlagged <- lag.xts(tempts,k=1)
    delta <- tempts-tlagged
    tempts[which(delta > temp_spike_thresh)] <- NA
    
    ##Eliminates days with near-constant temperatures (snow or lab conditions).
    range_fun <- function(x){range(x,na.rm=TRUE)[2] - range(x,na.rm=TRUE)[1]}
    daily_range <- apply.daily(tempts,FUN=range_fun)
    daily_mean <- apply.daily(tempts,FUN=mean,na.rm=TRUE)
    
    ##Snow/ice days are periods of 3 days with near-constant temps near zero.
    snow_yn <- daily_range < 1 & daily_mean < 2
    snow_yn_lag1 <- lag(snow_yn,k=1)
    snow_yn_lag2 <- lag(snow_yn,k=-1)
    snow_days <- as.Date(index(daily_range)[snow_yn & snow_yn_lag1 & snow_yn_lag2])
    
    ##Lab days are periods of near-constant temps at lab temp +/- one day for deployment.
    lab_days <- as.Date(index(daily_range)[daily_range < 2 & daily_mean > 20 & daily_mean < 26])
    lab_days <- c(lab_days,(lab_days-1),(lab_days+1))
    
    ##Sets lab and snow/ice measurements to NA.
    if(length(snow_days) > 0){
      print(paste("Possible snow/ice covered days found, removing..."))
      day_index <- as.Date(index(tempts))
      snow_index <- which(day_index %in% snow_days)
      tempts[snow_index] <- NA
    }
    if(length(lab_days) > 0){
      print(paste("Possible lab days found, removing..."))
      day_index <- as.Date(index(tempts))
      lab_index <- which(day_index %in% lab_days)
      tempts[lab_index] <- NA
    }
    
    ##Flags time-series with remaining problems
    if((sum(is.na(tempts)) / length(tempts)) > 0.10){
      flag_missing_values <- TRUE
    }else{
      flag_missing_values <- FALSE
    }
    
    if((mean_hr_max-max_temp_hr) > tz_tolerance | (mean_hr_max-max_temp_hr) < -1 * tz_tolerance){
      flag_wrong_tz <- TRUE
    }else{
      flag_wrong_tz <- FALSE
    }
    
    # Writes output to disk. If no data quality flags, the file goes in the "clean_unflagged" folder.
    outts <- data.frame(DATE=index(tempts),TZ=out_tz,TEMP=tempts)
    setwd(output_path)
    if(flag_missing_values==FALSE & flag_wrong_tz==FALSE){
      write.csv(outts,paste("./clean_unflagged/",airtemp_files[i],sep=""),row.names=FALSE)
    }else{
      write.csv(outts,paste("./clean_flagged/",airtemp_files[i],sep=""),row.names=FALSE)
    }
    
    # Save plot of cleaned data as a pdf
    setwd(figure_path)
    plotname <- strsplit(airtemp_files[i], ".csv")[[1]]
    if(flag_missing_values==FALSE & flag_wrong_tz==FALSE){
      plotname <- paste("/clean_unflagged/",strsplit(airtemp_files[i], ".csv")[[1]],sep="")
    }else{
      plotname <- paste("/clean_flagged/",strsplit(airtemp_files[i], ".csv")[[1]],sep="")
    }
    figpath <- paste(figure_path,"/",plotname,".pdf", sep = "")
    pdf(file = figpath, width = 10, height = 7)
    
    # Left Y axis
    par(mar = c(4, 6, 3, 6))
    plot(outts$TEMP~outts$DATE, main = plotname, xlab='Date', 
         ylab=expression(paste("Temperature", degree, "C")),col='blue',
         axes = T,ylim=c(-10,35),pch = 20)  
    dev.off()
    
    ##Assembles metadata.
    metadata$filename[i] <- airtemp_files[i]
    
    ## Gets the cleaned temp range
    metadata$cleaned_min[i] <- min(tempts,na.rm=TRUE)
    metadata$cleaned_max[i] <- max(tempts,na.rm=TRUE)
    
    ## Gets the hour of mean temperature.
    metadata$mean_hr_max[i] <- mean_hr_max
    
    metadata$flag_missing_data[i] <- flag_missing_values 
    metadata$flag_wrong_tz[i] <- flag_wrong_tz
  }
  
  metadata_unflagged <- metadata[metadata$flag_missing_data==FALSE &
                                 metadata$flag_wrong_tz==FALSE,]
  
  ##Merges metadata with existing metadata file.
  meta_all <- merge(air_meta,metadata,by.x="out_filename",by.y="filename",all.y=TRUE)
  
  ##Writes updated metadata file to output.
  setwd(output_path)
  write.table(meta_all,output_metadata_filename,sep=",",row.names=FALSE)
  return(metadata)
}


####Function to compute daily air temperature summaries for a collection of cleaned and merged files####

##USAGE: Computes daily summaries of cleaned and merged air temperature time-series for a collection
##of input files. Computed summaries are daily minumum, maximum, and average temperatures.

##ARGS: input_path - absolute path to a directory containing input files.
##      output_path - absolute path to a directory containing output files.
##      tzone - time-zone to assume for output time-series, must be in the set 
##      of timezones returned by OlsonNames()
##      overwrite - if TRUE, overwrite output files if they exist.

summarise_airtemp_daily <- function(input_path,
                                   output_path,
                                   tzone="Etc/GMT-7",
                                   overwrite=FALSE){
  require(xts)
  
  ##Checks inputs
  stopifnot(dir.exists(input_path))
  stopifnot(dir.exists(output_path))
  stopifnot(tzone %in% OlsonNames())
  
  raw_files <- list.files(input_path,pattern=".csv",full.names = TRUE)
  raw_names <- list.files(input_path,pattern=".csv",full.names = FALSE)
  first_file <- read.csv(raw_files[1])
  if(!all(c("DATETIME","TZ","TEMP") %in% colnames(first_file))){
    stop("Input files must have columns named DATETIME, TZ, TEMP")
  }
  
  ##Loops through each file and computes daily summaries.
  for(i in 1:length(raw_files)){
    print(paste("Now processing file",raw_names[i],"(",i," of ",length(raw_files),")"))
    out_file <- paste(output_path,"/",raw_names[i],sep="")
    if(file.exists(out_file) & overwrite==FALSE){
      print(paste("Output file exists, skipping..."))
    }else{
      d <- read.csv(raw_files[i])
      if(!all(c("DATETIME","TZ","TEMP") %in% colnames(d))){
        stop("All input files must have columns named DATETIME, TZ, TEMP")
      }
      d <- d[complete.cases(d),]
      d$DATETIME <- as.POSIXct(d$DATETIME,tz=tzone)
      ts <- xts(d$TEMP,order.by=d$DATETIME)
      ts_min_daily <- apply.daily(ts,FUN=min)
      ts_max_daily <- apply.daily(ts,FUN=max)
      ts_avg_daily <- apply.daily(ts,FUN=mean)
      df <- data.frame(DATE=as.Date(index(ts_avg_daily)),
                       MIN_TEMP=ts_min_daily,
                       MAX_TEMP=ts_max_daily,
                       AVG_TEMP=ts_avg_daily)
      out_filename <- paste(output_path,"/",gsub("_hourly.csv","_daily.csv",raw_names[i]),sep="")
      write.csv(df,out_filename,row.names=FALSE)
    }
  }
}

####Function to merge daily air temperature time-series into a single file.####

##USAGE: Compiles air temperature daily summaries computed with
##summarise_airtemp_daily() into a single file for each measurement type, trimming each time-series to start
##and end on the same date.

##ARGS: input_path - directory containing files processed with summarise_soiltemp_daily.

##      output_file_tavg - path to output file with compiled snow cover data.

##      output_file_tmin - path to output file with compiled minimum temperature data.

##      output_file_tmax - path to output file with compiled maximum temperature data.

##      start_date - POSIX date for the first day represented in the output files.

##      end_date - POSIX date for the last day represented in the output files.

##      return_data - if TRUE, return a data frame with compiled data. Data is formatted as a list of 
##                    three data frames, one for each sensor type.

##      overwrite - if TRUE, overwrite existing output files

compile_airtemp_daily <- function(input_path,
                                 output_file_tavg,
                                 output_file_tmin,
                                 output_file_tmax,
                                 start_date=as.Date("2009-09-01"),
                                 end_date=as.Date("2017-09-30"),
                                 return_data=TRUE,
                                 overwrite=FALSE){
  ##Required package.
  require(xts)
  
  ##Checks inputs.
  stopifnot(dir.exists(input_path))
  if(file.exists(output_file_tavg) & overwrite==FALSE){
    stop(paste(output_file_tavg,"already exists, select overwrite=TRUE to overwrite."))
  }
  if(file.exists(output_file_tmin) & overwrite==FALSE){
    stop(paste(output_file_tmin,"already exists, select overwrite=TRUE to overwrite."))
  }
  if(file.exists(output_file_tmax) & overwrite==FALSE){
    stop(paste(output_file_tmax,"already exists, select overwrite=TRUE to overwrite."))
  }
  stopifnot("Date" %in% class(start_date))
  stopifnot("Date" %in% class(end_date))
  stopifnot(is.logical(overwrite))
  
  ##Gets file list to process.
  daily_files <- list.files(input_path,pattern=".csv",full.names=TRUE)
  daily_names <- list.files(input_path,pattern=".csv",full.names=FALSE)
  
  ##Processes first file.
  print(paste("Processing file",daily_names[1],"(",1,"of",length(daily_names),")"))
  first_file <- read.csv(daily_files[1])
  if(!all(c("DATE","MIN_TEMP","MAX_TEMP","AVG_TEMP") %in% colnames(first_file))){
    stop("Input files must have columns named DATE,MIN_TEMP,MAX_TEMP,AVG_TEMP")
  }
  first_avg_ts <- xts(first_file$AVG_TEMP,order.by=as.Date(first_file$DATE))
  first_min_ts <- xts(first_file$MIN_TEMP,order.by=as.Date(first_file$DATE))
  first_max_ts <- xts(first_file$MAX_TEMP,order.by=as.Date(first_file$DATE))
  
  ##Creates daily time-series in date range.
  days <- seq(start_date,end_date,by="day")
  day_ts <- xts(rep(FALSE,length(days)),order.by=days)
  sitenames <- rep(NA,length(daily_files))
  
  ##Merges with daily time-series.
  avg_ts_all <- merge(day_ts,first_avg_ts,join="left")
  avg_ts_all <- avg_ts_all[,-1]
  min_ts_all <- merge(day_ts,first_min_ts,join="left")
  min_ts_all <- min_ts_all[,-1]
  max_ts_all <- merge(day_ts,first_max_ts,join="left")
  max_ts_all <- max_ts_all[,-1]
  sitenames[1] <- strsplit(daily_names[1],split="_")[[1]][1]
  
  ##Loops through th rest of the files and merges them.
  for (i in 2:length(daily_files)){
    print(paste("Processing file",daily_names[i],"(",i,"of",length(daily_names),")"))
    fd <- read.csv(daily_files[i])
    if(!all(c("DATE","MIN_TEMP","MAX_TEMP","AVG_TEMP") %in% colnames(fd))){
      stop("Input files must have columns named DATE,MIN_TEMP,MAX_TEMP,AVG_TEMP")
    }
    sitenames[i] <- strsplit(daily_names[i],split="_")[[1]][1]
    avg_ts <- xts(fd$AVG_TEMP,order.by=as.Date(fd$DATE))
    min_ts <- xts(fd$MIN_TEMP,order.by=as.Date(fd$DATE))
    max_ts <- xts(fd$MAX_TEMP,order.by=as.Date(fd$DATE))
    avg_ts_all <- merge(avg_ts_all,avg_ts,join="left")
    min_ts_all <- merge(min_ts_all,min_ts,join="left")
    max_ts_all <- merge(max_ts_all,max_ts,join="left")
  }
  names(avg_ts_all) <- sitenames
  names(min_ts_all) <- sitenames
  names(max_ts_all) <- sitenames
  avg_df_all <- data.frame(DATE=index(avg_ts_all),avg_ts_all)
  min_df_all <- data.frame(DATE=index(min_ts_all),min_ts_all)
  max_df_all <- data.frame(DATE=index(max_ts_all),max_ts_all)
  colnames(avg_df_all) <- c("DATE",sitenames)
  colnames(min_df_all) <- c("DATE",sitenames)
  colnames(max_df_all) <- c("DATE",sitenames)
  DOY <- as.numeric(format(avg_df_all$datetime,format="%j"))
  Year <- as.numeric(format(avg_df_all$datetime,format="%Y"))
  
  ##Writes output to disk.
  write.csv(avg_df_all,output_file_tavg,row.names=FALSE)
  write.csv(min_df_all,output_file_tmin,row.names=FALSE)
  write.csv(max_df_all,output_file_tmax,row.names=FALSE)
  if(return_data==TRUE){
    return(list(tavg=avg_df_all,tmin=min_df_all,tmax=max_df_all))
  }
}

####Function to clean a collection of soil temperature files####

##USAGE: Function to clean a collection of formatted soil temperature files. Input files must have columns
##YEAR,MONTH,DAY,HOUR and TEMP. The function 1) checks for and corrects AM/PM problems, 2) checks for other
##time-zone problems by calculating the mean hour of temperature maxima, and attempts to fix them by assuming
##other common time-zones. 3) Checks for and corrects Fahrenheit / Celcius problems 4) Removes anomalous spikes
##and values outside of the physical temperature range. 5) eliminates measurements with near-constant temperatures
## near 24C (likely days in climate-controlled lab conditions). Usage and arguments are similar to clean_air_temps, 
##but has different tests and defaults.

##ARGS: input_path - absolute path to directory containing input files

##      input_metadata_filename - path to file describing input files

##      output_path - path to a directory to hold output files

##      output_metadata_filename - path to a file describing output files

##      figure_path  - a path to hold plots of cleaned data

##      guess_tz - time-zone to guess for input data, must be a known time-zone returned by OlsonNames()

##      out_tz - time-zone for output data, must be a known time-zone returned by OlsonNames

##      tz_tolerance - maximum tolerated difference (in hours) between the hour of maximum temperature
##                    for each file and the hour specified in the max_temp_hr argument. If the difference
##                    is greater than this threshold, then the function will attempt to fix time-zone problems.

##      temp_spike_thresh - maximum tolerated measurement-to-measurement change in temperature. Changes exceeding
##                          this threshold will be removed.

##      min_temp_thresh - minimum temperature threshold. Values less than this will be removed.

##      max_temp_thresh - maximum temperature threshold. Values greater than this will be removed.

##      max_temp_hr - the expected hour of maximum temperature. Usually in the afternoon (hours 13 - 15).

##      cf_test_params - 4-element numeric vector controlling tests for data that is recorded in Fahrenheit.
##                       Measurements are likely in Fahrenheit if the maximum temperature is greater than
##                       [1], the maximum temperature is less than [2], the minimum temperature is greater than
##                       than [3] and the mean temperature is greater than [4].

##      overwrite - if TRUE, overwrite output files if they exist.

clean_soil_temps <- function(input_path = NULL,
                            input_metadata_filename = NULL,
                            output_path = NULL,
                            output_metadata_filename = NULL,
                            figure_path = NULL,
                            guess_tz="Etc/GMT-7",
                            out_tz="Etc/GMT-7",
                            tz_tolerance=6,
                            temp_spike_thresh=20,
                            min_temp_thresh=-20,
                            max_temp_thresh=70,
                            max_temp_hr=17,
                            cf_test_params=c(40,160,20,30),
                            overwrite=TRUE){
  ##Sets up workspace
  require(xts)
  require(psych)
  Sys.setenv(TZ=guess_tz)
  
  ##Checks inputs
  stopifnot(length(input_metadata_filename)==1)
  stopifnot(length(output_metadata_filename)==1)
  stopifnot(dir.exists(input_path))
  stopifnot(dir.exists(output_path))
  stopifnot(dir.exists(figure_path))
  stopifnot(is.logical(overwrite) & length(overwrite)==1)
  setwd(input_path)
  stopifnot(file.exists(input_metadata_filename))
  stopifnot(guess_tz %in% OlsonNames())
  stopifnot(out_tz %in% OlsonNames())
  
  ##Checks to see how many input files there are.
  soiltemp_files <- list.files(input_path,pattern=".csv$")
  nfiles <- length(soiltemp_files)
  
  ##Creates folders for output
  setwd(output_path)
  if(!dir.exists("./clean_flagged")){
    dir.create("./clean_flagged")
  }
  if(!dir.exists("./clean_unflagged")){
    dir.create("./clean_unflagged")
  }
  
  ##Creates folders for output
  setwd(figure_path)
  if(!dir.exists("./clean_flagged")){
    dir.create("./clean_flagged")
  }
  if(!dir.exists("./clean_unflagged")){
    dir.create("./clean_unflagged")
  }
  
  ##Checks to make sure we've got the same number files as we do metadata lines
  setwd(input_path)
  meta <- read.table(input_metadata_filename,sep=",",header=TRUE)
  #stopifnot(nrow(meta)==nfiles)
  
  print(paste("Now processing ",nfiles," soil-temp files."))
  
  ##Sets up a file counter.
  file_n <- 0
  
  ##Creates empty data frame for metadata.
  nfiles <- length(soiltemp_files)
  metadata <- data.frame(filename=rep("NA",nfiles),
                         cleaned_min=rep(NA,nfiles),
                         cleaned_max=rep(NA,nfiles),
                         mean_hr_max=rep(NA,nfiles),
                         flag_missing_data=rep(NA,nfiles),
                         flag_wrong_tz=rep(NA,nfiles))
  
  metadata$filename <- as.character(metadata$filename)
  
  for (i in 1:length(soiltemp_files)){
    
    # Ensures we are looking at the right directory
    setwd(input_path)
    
    # Prints progress to console.
    flush.console()
    print(paste("Now processing ",soiltemp_files[i],". File ",i," of ",
                length(soiltemp_files)))
    #Reads data and converts to date and time-series format.
    data <- read.csv(soiltemp_files[i],header=TRUE)
    data <- data[complete.cases(data),]
    
    ##Skips if file has no rows.
    if(nrow(data) < 2){
      print(paste(soiltemp_files[i], "has less than 2 rows, skipping..."))
      next
    }
    
    ##Checks to make sure data has hours in military time.
    max_hr <- max(data$HOUR,na.rm=TRUE)
    if(max_hr <= 12){
      print(paste("AM/PM problem, correcting..."))
      data$datestring <- paste(data$YEAR,data$MONTH,data$DAY,data$HOUR,data$MIN,sep="-")
      data$HOUR[duplicated(data$datestring)] <- data$HOUR[duplicated(data$datestring)] + 12
      dates <- as.Date(paste(data$YEAR,data$MONTH,data$DAY,sep="-"))
      first_day <- min(dates)
      first_day_index <- which(dates==first_day)
      data <- data[-first_day_index,]
    }
    
    data$datestring <- paste(data$YEAR,data$MONTH,data$DAY,data$HOUR,data$MIN,sep="-")
    data$datetime <- strptime(data$datestring,
                              format="%Y-%m-%d-%H-%M",tz=guess_tz)
    data <- data[!is.na(data$datetime),]
    tempts <- xts(data$TEMP,order.by=data$datetime,tz=guess_tz)
    tzone(tempts) <- out_tz
    
    
    ##Snow/ice days are periods with near-constant temps near zero.
    range_fun <- function(x){
      range(x,na.rm=TRUE)[2] - range(x,na.rm=TRUE)[1]
      }
    daily_range <- apply.daily(tempts,FUN=range_fun)
    daily_mean <- apply.daily(tempts,FUN=mean,na.rm=TRUE)
    snow_yn <- (daily_range < 1 & daily_mean < 2) | (daily_range < 1.8 & daily_mean < 34 & daily_mean > 30)
    snow_prop <- mean(as.numeric(snow_yn),na.rm=TRUE)
    snow_days <- as.Date(index(daily_range)[snow_yn])
    
    ##Calculates the circadian mean hour of the day with the maximum temperature for days without snow.
    if(length(snow_days)==0){
      hr_max_ts <- do.call(rbind, lapply(split(tempts,"days"), function(x) x[which.max(x)]))
    }else{
      day_index <- as.Date(index(tempts))
      snow_index <- which(day_index %in% snow_days)
      hr_max_ts <- do.call(rbind, lapply(split(tempts[-snow_index],"days"), function(x) x[which.max(x)]))
    }
    
    hrs <- as.numeric(format(index(hr_max_ts),format="%H"))
    mean_hr_max <- circadian.mean(hrs,hours=TRUE)
    
    if(((mean_hr_max-max_temp_hr) > tz_tolerance | (mean_hr_max-max_temp_hr) < -1*tz_tolerance) &
       snow_prop < 0.8){
      print(paste("Found possible TZ problem, mean max temp at hour",mean_hr_max))
      data$datestring <- paste(data$YEAR,data$MONTH,data$DAY,sep="-")
      data$timestring <- paste(data$HOUR,data$MIN,"00",sep=":")
      data$datetime <- strptime(paste(data$datestring,data$timestring),
                                format="%Y-%m-%d %H:%M:%S",tz="UTC")
      tempts <- xts(data$TEMP,order.by=data$datetime)
      tzone(tempts) <- out_tz
      hr_max_ts <- do.call(rbind, lapply(split(tempts,"days"), function(x) x[which.max(x)]))
      hrs <- as.numeric(format(index(hr_max_ts),format="%H"))
      mean_hr_max <- circadian.mean(hrs,hours=TRUE)
      print(paste("Assuming UTC, mean max temp now at",mean_hr_max))
    }
    if(((mean_hr_max-max_temp_hr) > tz_tolerance | (mean_hr_max-max_temp_hr) < -1*tz_tolerance) &
       snow_prop < 0.8){
      print(paste("Found possible TZ problem, mean max temp at hour",mean_hr_max))
      data$datestring <- paste(data$YEAR,data$MONTH,data$DAY,sep="-")
      data$timestring <- paste(data$HOUR,data$MIN,"00",sep=":")
      data$datetime <- strptime(paste(data$datestring,data$timestring),
                                format="%Y-%m-%d %H:%M:%S",tz="Etc/GMT+9")
      tempts <- xts(data$TEMP,order.by=data$datetime)
      tzone(tempts) <- out_tz
      hr_max_ts <- do.call(rbind, lapply(split(tempts,"days"), function(x) x[which.max(x)]))
      hrs <- as.numeric(format(index(hr_max_ts),format="%H"))
      mean_hr_max <- circadian.mean(hrs,hours=TRUE)
      print(paste("Assuming GMT+9, mean max temp now at",mean_hr_max))
    }
    if(((mean_hr_max-max_temp_hr) > tz_tolerance | (mean_hr_max-max_temp_hr) < -1*tz_tolerance) &
       snow_prop < 0.8){
      print(paste("Found possible TZ problem, mean max temp at hour",mean_hr_max))
      data$datestring <- paste(data$YEAR,data$MONTH,data$DAY,sep="-")
      data$timestring <- paste(data$HOUR,data$MIN,"00",sep=":")
      data$datetime <- strptime(paste(data$datestring,data$timestring),
                                format="%Y-%m-%d %H:%M:%S",tz="Etc/GMT-4")
      tempts <- xts(data$TEMP,order.by=data$datetime)
      tzone(tempts) <- out_tz
      hr_max_ts <- do.call(rbind, lapply(split(tempts,"days"), function(x) x[which.max(x)]))
      hrs <- as.numeric(format(index(hr_max_ts),format="%H"))
      mean_hr_max <- circadian.mean(hrs,hours=TRUE)
      print(paste("Assuming GMT-4, mean max temp now at",mean_hr_max))
    }
    
    ## Checks for and corrects Celcius / Fahrenheit problems
    if(max(tempts) > cf_test_params[1] & max(tempts) < cf_test_params[2] &
       min(tempts) > cf_test_params[3] & mean(tempts) > cf_test_params[2]){
      print(paste("Found possible C / F problem, correcting..."))
      tempts <- (tempts - 32) * (5/9)
    }
    
    ## Checks for and eliminates values outside of the physical range 
    ## and temperature spikes that could be caused by direct sun.
    tempts[tempts < min_temp_thresh] <- NA
    tempts[tempts > max_temp_thresh] <- NA
    tlagged <- lag.xts(tempts,k=1)
    delta <- tempts-tlagged
    tempts[delta > temp_spike_thresh] <- NA
    
    ##Lab days are periods of near-constant temps at lab temp +/- one day for deployment.
    range_fun <- function(x){range(x,na.rm=TRUE)[2] - range(x,na.rm=TRUE)[1]}
    daily_range <- apply.daily(tempts,FUN=range_fun)
    daily_mean <- apply.daily(tempts,FUN=mean,na.rm=TRUE)
    lab_days <- as.Date(index(daily_range)[daily_range < 2 & daily_mean > 20 & daily_mean < 26])
    lab_days <- c(lab_days,(lab_days-1),(lab_days+1))
    
    ##Sets lab measurements to zero.
    if(length(lab_days) > 0){
      print(paste("Possible lab days found, removing..."))
      day_index <- as.Date(index(tempts))
      lab_index <- which(day_index %in% lab_days)
      tempts[lab_index] <- NA
    }
    
    ##Flags time-series with problems
    if((sum(is.na(tempts)) / length(tempts)) > 0.1){
      flag_missing_values <- TRUE
    }else{
      flag_missing_values <- FALSE
    }
    
    if((mean_hr_max-max_temp_hr) > tz_tolerance | (mean_hr_max-max_temp_hr) < -1 * tz_tolerance){
      flag_wrong_tz <- TRUE
    }else{
      flag_wrong_tz <- FALSE
    }
    outts <- data.frame(DATE=index(tempts),TZ=out_tz,TEMP=tempts)
    setwd(output_path)
    if(flag_missing_values==FALSE & flag_wrong_tz==FALSE){
      write.csv(outts,paste("./clean_unflagged/",soiltemp_files[i],sep=""),row.names=FALSE)
    }else{
      write.csv(outts,paste("./clean_flagged/",soiltemp_files[i],sep=""),row.names=FALSE)
    }
    
    # Save figure as pdf
    setwd(figure_path)
    plotname <- strsplit(soiltemp_files[i], ".csv")[[1]]
    if(flag_missing_values==FALSE & flag_wrong_tz==FALSE){
      plotname <- paste("clean_unflagged/",strsplit(soiltemp_files[i], ".csv")[[1]],sep="")
    }else{
      plotname <- paste("clean_flagged/",strsplit(soiltemp_files[i], ".csv")[[1]],sep="")
    }
    figpath <- paste(figure_path,"/",plotname,".pdf", sep = "")
    pdf(file = figpath, width = 10, height = 7)
    
    # Left Y axis
    par(mar = c(4, 6, 3, 6))
    plot(outts$TEMP~outts$DATE, main = plotname, xlab='Date', 
         ylab=expression(paste("Temperature", degree, "C")),col='blue',
         axes = T,ylim=c(-10,35),pch = 20)  
    dev.off()

    ##Assembles metadata.
    metadata$filename[i] <- soiltemp_files[i]
    
    ## Gets the cleaned temp range
    metadata$cleaned_min[i] <- min(tempts,na.rm=TRUE)
    metadata$cleaned_max[i] <- max(tempts,na.rm=TRUE)
    
    ## Gets the hour of mean temperature.
    metadata$mean_hr_max[i] <- mean_hr_max
    
    metadata$flag_missing_data[i] <- flag_missing_values 
    metadata$flag_wrong_tz[i] <- flag_wrong_tz
  }
  
  metadata_unflagged <- metadata[metadata$flag_missing_data==FALSE &
                                   metadata$flag_wrong_tz==FALSE,]
  
  meta_all <- merge(meta,metadata,by.x="out_filename",by.y="filename",all.y=TRUE)
  
  ##Writes cleaned unflagged files to output.
  setwd(output_path)
  write.table(meta_all,output_metadata_filename,sep=",",row.names=FALSE)
  return(metadata)
}


####Function to compute daily soil temperature and snow cover for a collection of cleaned and merged files####

##USAGE: Computes daily summaries of cleaned and merged soil temperature time-series for a collection
##of input files. Computed summaries are daily minumum and maximum temperatures, along with binary snow cover.
##Usage is similar to summarise_airtemp_daily except it outputs snow cover estimates and has additional snow-related
##arguments.

##ARGS: input_path - absolute path to a directory containing input files.

##      output_path - absolute path to a directory containing output files.

##      tzone - time-zone to assume for output time-series, must be in the set 
##              of timezones returned by OlsonNames()

##      snow_range_thresh - the maximum diurnal temperature range considered snow-covered

##      snow_maxt_thresh -  the highest maximum daily temperature of snow-covered days

##      overwrite - if TRUE, overwrite output files if they exist.

summarise_soiltemp_daily <- function(input_path,
                                 output_path,
                                 tzone="Etc/GMT-7",
                                 snow_range_thresh=3,
                                 snow_maxt_thresh=2,
                                 overwrite=FALSE){
  
  require(xts)
  
  ##Checks inputs
  stopifnot(dir.exists(input_path))
  stopifnot(dir.exists(output_path))
  stopifnot(tzone %in% OlsonNames())
  stopifnot(is.numeric(snow_range_thresh))
  stopifnot(snow_range_thresh > 0)
  stopifnot(is.numeric(snow_maxt_thresh))
  
  raw_files <- list.files(input_path,pattern=".csv",full.names = TRUE)
  raw_names <- list.files(input_path,pattern=".csv",full.names = FALSE)
  first_file <- read.csv(raw_files[1])
  if(!all(c("DATETIME","TZ","TEMP") %in% colnames(first_file))){
    stop("Input files must have columns named DATETIME, TZ, TEMP")
  }
  
  snow_fun <- function(ts){
    (min(ts) - max(ts)) < snow_range_thresh & max(ts) < snow_maxt_thresh
  }
  
  ##Loops through each file and estimates whether there was snow on each day.
  for(i in 1:length(raw_files)){
    print(paste("Now processing file",raw_names[i],"(",i," of ",length(raw_files),")"))
    out_file <- paste(output_path,"/",raw_names[i],sep="")
    if(file.exists(out_file) & overwrite==FALSE){
      print(paste("Output file exists, skipping..."))
    }else{
      d <- read.csv(raw_files[i])
      if(!all(c("DATETIME","TZ","TEMP") %in% colnames(d))){
        stop("All input files must have columns named DATETIME, TZ, TEMP")
      }
      d <- d[complete.cases(d),]
      d$DATETIME <- as.POSIXct(d$DATETIME,tz=tzone)
      ts <- xts(d$TEMP,order.by=d$DATETIME)
      ts_min_daily <- apply.daily(ts,FUN=min)
      ts_max_daily <- apply.daily(ts,FUN=max)
      ts_snow <- apply.daily(ts,FUN=snow_fun)
      df <- data.frame(DATE=as.Date(index(ts_snow)),
                       MIN_TEMP=ts_min_daily,
                       MAX_TEMP=ts_max_daily,
                       SNOW=ts_snow)
      out_filename <- paste(output_path,"/",gsub("_hourly.csv","_daily.csv",raw_names[i]),sep="")
      write.csv(df,out_filename,row.names=FALSE)
    }
  }
}

####Function to compile daily snow cover and temperature for a collection of files####

##USAGE: Compiles snow cover and soil temperature daily summaries computed with
##summarise_soiltemp_daily into a single file for each measurement type, trimming each time-series to start
##and end on the same date.

##ARGS: input_path - directory containing files processed with summarise_soiltemp_daily.

##      output_file_snow - path to output file with compiled snow cover data.

##      output_file_tmin - path to output file with compiled minimum temperature data.

##      output_file_tmax - path to output file with compiled maximum temperature data.

##      start_date - POSIX date for the first day represented in the output files.

##      end_date - POSIX date for the last day represented in the output files.

##      add_summer_zero - if TRUE, zeros are added for missing measurements after September 15th.

##      overwrite - if TRUE, overwrite existing output files

##      return_data - if TRUE, return a data frame with compiled data. Data is formatted as a list of 
##                    three data frames, one for each sensor type.

compile_soiltemp_daily <- function(input_path,
                                   output_file_snow,
                                   output_file_tmin,
                                   output_file_tmax,
                                   start_date=as.Date("2009-09-01"),
                                   end_date=as.Date("2017-09-30"),
                                   add_summer_zero=TRUE,
                                   overwrite=FALSE,
                                   return_data=TRUE){
  ##Required package.
  require(xts)
  
  ##Checks inputs.
  stopifnot(dir.exists(input_path))
  if(file.exists(output_file_snow) & overwrite==FALSE){
    stop(paste(output_file_snow,"already exists, select overwrite=TRUE to overwrite."))
  }
  if(file.exists(output_file_tmin) & overwrite==FALSE){
    stop(paste(output_file_tmin,"already exists, select overwrite=TRUE to overwrite."))
  }
  if(file.exists(output_file_tmax) & overwrite==FALSE){
    stop(paste(output_file_tmax,"already exists, select overwrite=TRUE to overwrite."))
  }
  stopifnot("Date" %in% class(start_date))
  stopifnot("Date" %in% class(end_date))
  stopifnot(is.logical(add_summer_zero))
  stopifnot(is.logical(overwrite))
  
  ##Gets file list to process.
  daily_files <- list.files(input_path,pattern=".csv",full.names=TRUE)
  daily_names <- list.files(input_path,pattern=".csv",full.names=FALSE)
  
  ##Processes first file.
  print(paste("Processing file",daily_names[1],"(",1,"of",length(daily_names),")"))
  first_file <- read.csv(daily_files[1])
  if(!all(c("DATE","MIN_TEMP","MAX_TEMP","SNOW") %in% colnames(first_file))){
    stop("Input files must have columns named DATE,MIN_TEMP,MAX_TEMP,SNOW")
  }
  first_snow_ts <- xts(first_file$SNOW,order.by=as.Date(first_file$DATE))
  first_min_ts <- xts(first_file$MIN_TEMP,order.by=as.Date(first_file$DATE))
  first_max_ts <- xts(first_file$MAX_TEMP,order.by=as.Date(first_file$DATE))
  
  ##Creates daily time-series in date range.
  days <- seq(start_date,end_date,by="day")
  day_ts <- xts(rep(FALSE,length(days)),order.by=days)
  sitenames <- rep(NA,length(daily_files))
  
  ##Merges with daily time-series.
  snow_ts_all <- merge(day_ts,first_snow_ts,join="left")
  snow_ts_all <- snow_ts_all[,-1]
  min_ts_all <- merge(day_ts,first_min_ts,join="left")
  min_ts_all <- min_ts_all[,-1]
  max_ts_all <- merge(day_ts,first_max_ts,join="left")
  max_ts_all <- max_ts_all[,-1]
  sitenames[1] <- strsplit(daily_names[1],split="_")[[1]][1]
  
  ##Loops through th rest of the files and merges them.
  for (i in 2:length(daily_files)){
    print(paste("Processing file",daily_names[i],"(",i,"of",length(daily_names),")"))
    fd <- read.csv(daily_files[i])
    if(!all(c("DATE","MIN_TEMP","MAX_TEMP","SNOW") %in% colnames(fd))){
      stop("Input files must have columns named DATE,MIN_TEMP,MAX_TEMP,SNOW")
    }
    sitenames[i] <- strsplit(daily_names[i],split="_")[[1]][1]
    snow_ts <- xts(fd$SNOW,order.by=as.Date(fd$DATE))
    min_ts <- xts(fd$MIN_TEMP,order.by=as.Date(fd$DATE))
    max_ts <- xts(fd$MAX_TEMP,order.by=as.Date(fd$DATE))
    snow_ts_all <- merge(snow_ts_all,snow_ts,join="left")
    min_ts_all <- merge(min_ts_all,min_ts,join="left")
    max_ts_all <- merge(max_ts_all,max_ts,join="left")
  }
  names(snow_ts_all) <- sitenames
  names(min_ts_all) <- sitenames
  names(max_ts_all) <- sitenames
  snow_df_all <- data.frame(DATE=index(snow_ts_all),snow_ts_all)
  min_df_all <- data.frame(DATE=index(min_ts_all),min_ts_all)
  max_df_all <- data.frame(DATE=index(max_ts_all),max_ts_all)
  colnames(snow_df_all) <- c("DATE",sitenames)
  colnames(min_df_all) <- c("DATE",sitenames)
  colnames(max_df_all) <- c("DATE",sitenames)
  DOY <- as.numeric(format(snow_df_all$datetime,format="%j"))
  Year <- as.numeric(format(snow_df_all$datetime,format="%Y"))
  
  ##Replaces missing summer snow measurements with zeros.
  if(add_summer_zero==TRUE){
    year_start <- as.numeric(format(start_date,format="%Y"))
    year_end <- as.numeric(format(end_date,format="%Y"))
    for(i in year_start:year_end){
      for(j in 1:ncol(snow_df_all)){
        prevtest <- !is.na(snow_df_all[Year==i,j][1])
        nexttest <- any(!is.na(snow_df_all[Year==i & DOY >= 273,j]))
        if(prevtest & nexttest){
          daysnow <- snow_df_all[Year==i,j]
          daysnow[is.na(daysnow)] <- 0
          snow_df_all[Year==i,j] <- daysnow
        }
      }
    }
  }
  write.csv(snow_df_all,output_file_snow,row.names=FALSE)
  write.csv(min_df_all,output_file_tmin,row.names=FALSE)
  write.csv(max_df_all,output_file_tmax,row.names=FALSE)
  if(return_data==TRUE){
    return(list(snow=snow_df_all,tmin=min_df_all,tmax=max_df_all))
  }
}

####Custom plotting function to check alignment of a lot of time-series.####

##USAGE: Function to create a time-series plot useful for checking data. Optionally labels time-series.

##ARGS: data_df - data frame with time-series to be compared. Suitable data frames are returned by 
##                  compile_airtemp_daily and compile_soiltemp_daily.

##      year_seq - vector of 4-digit years where data sould be plotted.

##      min_month - mm-dd formatted date suffix representing the plot start date. For example "01-01"
##                  represents January 1st, and "10-01" represents October 1st.

##      max_month - mm-dd formatted date suffix representing the plot end date. For example "01-01"
##                  represents January 1st, and "10-01" represents October 1st.

##      min_y - minimum of plot y-axis.

##      max_y - maximum of plot y-axis.

##      col_subset - vector of names of columns in input data to plot. If missing, plot all columns.

##      ID_text - if TRUE, labels the plotted time-series witht their column names.

alignment_plot <- function(data_df=NULL,
                           year_seq=2008:2017,
                           min_month="01-01",
                           max_month="12-31",
                           min_y=-25,max_y=35,
                           col_subset="all",
                           ID_text=FALSE){
  set.seed(60)
  pal <- sample(rainbow(n=ncol(data_df),start=0.1),size=ncol(data_df),replace=FALSE)
  par(mfrow=c(length(year_seq),1),mar=c(0,1,0,1),oma=c(2,3,1,0))
  for(j in year_seq){
    plot_days <- seq(as.Date(paste(j,min_month,sep="-")),as.Date(paste(j,max_month,sep="-")),by="day")
    filler <- rep(NA,length(plot_days))
    if(j != max(year_seq)){
      plot(plot_days,filler,ylim=c(min_y,max_y),ylab="",xaxt='n')
    }else{
      plot(plot_days,filler,ylim=c(min_y,max_y),ylab="Temp (C)")
    }
    abline(0,0,lty=2)
    legend("topleft",legend=j,bty="n")
    if(col_subset=="all" | is.null(col_subset)){
      airtemp_dr <- data_df[data_df$DATE %in% plot_days,]
    }else{
      airtemp_dr <- data_df[data_df$DATE %in% plot_days,c(1,which(colnames(data_df) %in% col_subset))]
    }
    
    for(i in 2:ncol(airtemp_dr)){
      points(airtemp_dr$DATE,type="l",airtemp_dr[,i],col=pal[i])
      if(ID_text==TRUE){
        text(x=airtemp_dr$DATE[which.max(airtemp_dr[,i])],y=max(airtemp_dr[,i],na.rm=TRUE),
             labels=colnames(airtemp_dr)[i],col=pal[i])
      }
    }
  }
  mtext(2,line=1,text="Temp (C)",outer=TRUE,cex=0.7)
}