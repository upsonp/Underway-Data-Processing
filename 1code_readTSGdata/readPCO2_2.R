#' @title Read pco2out file from BIO underway flow through system
#' 
#' @description Reads in pco2 output file from underway flow through system
#' 
#' @param file the name of the file which the data will be read from
#' 
#' @output a data frame, of relevant data 
#' 
#' @author Diana Cardoso
# Oct 2023
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

read.pco2_2 <- function(file,fileday){
  # read file in
  #   note that warn=FALSE, meaning it won't warn us of an
  #   incomplete final line, which is common
  d <- readLines(file, 
                 warn = FALSE)
  d <- d[2:length(d)]
  # split each string into a list
  ss <- strsplit(d, split = ',')
  sspco2 <- ss  
 
  # date time from computer 
  date <- unlist(lapply(sspco2, function(k) k[1]))
  # find invalid dates - bad data
  datesplit <- strsplit(date, split = '-')
  year <- as.numeric(unlist(lapply(datesplit, function(k) k[1]))) # extract year from date and make numberic
  #  check for year equal to 2021
  ttyear <- table(year)
  goodyear <- as.numeric(names(which.max(ttyear)))
  goodyr <- which(year == goodyear, arr.ind = TRUE)
  sspco2 <- sspco2[goodyr] #only keep data =2021 (goodyr)
  # check for lines with less than length of 18
  gooddata <- which(unlist(lapply(sspco2, length)) > 16)
  sspco2 <- sspco2[ gooddata] #only keep data which has lines longer than 18 variables
  
  # now start getting relevant items out of the lines
  
  #date and time from computer
  date <- unlist(lapply(sspco2, function(k) k[1]))
  # format date and time
  time <- as.POSIXct(date, tz = 'UTC',optional = TRUE)
  
  #date and time from pco2
  pco2year <- unlist(lapply(sspco2, function(k) k[4]))
  pco2month <- unlist(lapply(sspco2, function(k) k[5]))
  pco2day <- unlist(lapply(sspco2, function(k) k[6]))
  pco2hour <- unlist(lapply(sspco2, function(k) k[7]))
  pco2min <- unlist(lapply(sspco2, function(k) k[8]))
  pco2sec <- unlist(lapply(sspco2, function(k) k[9]))
  
  tyear <- table(pco2year)#  check for year equal to 2021, remove bad dates
  goodyear <- as.numeric(names(which.max(tyear)))
  badyr <- which(pco2year != goodyear, arr.ind = TRUE)
  pco2year[badyr] <- as.character(goodyear)  
 
  tmon <- table(pco2month)#  check months remove bad dates
  goodmon <- names(which.max(tmon))
  badm <- which(pco2month != goodmon, arr.ind = TRUE)
  pco2month[badm] <-as.character(goodmon)
  
  tday <- table(pco2day)
  numday <- as.numeric(pco2day) #  check days are not greater than 31 and equal the day in the file name 
  if (fileday==1){
    filelastday <- 31
    badday1 <- which(numday > 31, arr.ind = TRUE)
    badday2 <- which(numday > fileday+1 & numday < 30, arr.ind = TRUE) 
  }  else{
    badday1 <- which(numday < (fileday-1), arr.ind = TRUE)
    badday2 <- which(numday > fileday+1, arr.ind = TRUE) 
  }
  
  thour <- table(pco2hour) #  check hours are not greater than 24 and character length is equal to 2
  numhour <- as.numeric(pco2hour)
  badhour <- which(numhour > 24, arr.ind = TRUE)
  badhour2 <- which(nchar(pco2hour) != 2)
  
  tmin <- table(pco2min) #  check minutes are not greater than 60 and character length is equal to 2
  nummin <- as.numeric(pco2min)
  badmin <- which(nummin > 60, arr.ind = TRUE)
  badmin2 <- which(nchar(pco2min) != 2)
  
  tsec <- table(pco2sec) #  check seconds are not greater than 60 and character length is equal to 2
  numsec <- as.numeric(pco2sec)
  badsec <- which(numsec > 60, arr.ind = TRUE)
  badsec2 <- which(nchar(pco2sec) != 2)
 
  # assemble date and time from pco2 and replace bad dates with NaN
  datepco2 <- paste0(pco2year,"-",pco2month,"-",pco2day," ",pco2hour,":", pco2min,":",pco2sec)
  unique(nchar(datepco2))
  badtime <- which(nchar(datepco2) != 19) #  check date time character length is equal to 19
  datepco2[badtime] <- NA  
  datepco2[badday1] <- NA
  datepco2[badday2] <- NA
  datepco2[badhour] <- NA
  datepco2[badhour2] <- NA
  datepco2[badmin] <- NA
  datepco2[badmin2] <- NA
  datepco2[badsec] <- NA
  datepco2[badsec2] <- NA
  # format date and time from pco2
  timepco2 <- as.POSIXct(datepco2, tz = 'UTC',optional = TRUE) #origin="2021-09-23 23:56:26"
  
  
  # pco2 data getting relevant items out of the lines
  zero_AD_counts <- unlist(lapply(sspco2, function(k) k[10]))
  current_AD_counts <- unlist(lapply(sspco2, function(k) k[11]))
  CO2_ppm <- unlist(lapply(sspco2, function(k) k[12]))
  Ave_IRGA_temp_C <- unlist(lapply(sspco2, function(k) k[13]))
  Humidity_mbar <- unlist(lapply(sspco2, function(k) k[14]))
  Humidity_sensor_temp_C <- unlist(lapply(sspco2, function(k) k[15]))
  Gas_stream_pres_mbar <- unlist(lapply(sspco2, function(k) k[16]))
  IRGA_detector_temp_C <- unlist(lapply(sspco2, function(k) k[17]))
#  IRGA_source_temp_C <- unlist(lapply(sspco2, function(k) k[18]))
#  voltage <- unlist(lapply(sspco2, function(k) k[19]))
  
  # put time and pco2 data together
  
  df <- data.frame(time = time,
                   time_pco2 = timepco2,
                   zero_AD_counts = as.numeric(zero_AD_counts),
                   current_AD_counts = as.numeric(current_AD_counts),
                   CO2_ppm = as.numeric(CO2_ppm),
                   Ave_IRGA_temp_C = as.numeric(Ave_IRGA_temp_C),
                   Humidity_mbar = as.numeric(Humidity_mbar),
                   Humidity_sensor_temp_C = as.numeric(Humidity_sensor_temp_C),
                   Gas_stream_pres_mbar = as.numeric(Gas_stream_pres_mbar),
                   IRGA_detector_temp_C = as.numeric(IRGA_detector_temp_C)
#                   IRGA_source_temp_C = as.numeric(IRGA_source_temp_C),
#                   voltage = as.numeric(voltage)
                  )
  df
  
}