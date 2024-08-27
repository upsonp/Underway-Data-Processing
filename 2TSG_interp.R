##2TSG_interp.R
#***Desciption: Script to read,interpolate hourly, and plot all processed data from underway flow 
# through system. processed files are saved in the processed folder and  
# 2 code_interp_plot_TSGdata folder contains all functions to interpolate and plot

#***INPUT: 
# 1- processed files one per 24 hours in processed folder and are named:
# TSGposition_yyyymmdd.CSV, FLOWdata_yyyymmdd.CSV, TSGdata_yyyymmdd.CSV, PCO2data_yyyymmdd.CSV
# 2- cruise number missionnum <- "xxxyyyy000" second line of code
# 3- the longitude and latitude limits for plots variable lonlim and latlim third line of code
# 4- hourrate can be changed from hour to another frequency

# OUTPUT is in the \2code_interp_plot_TSGdata\hourly_TSG_dataplots folder and includes:

  # - one file per variable of hourly interpolated data, named TSG_xxxxdata_xxxx_HUDyyyyfff.csv 
  # (where xxxx is variable and HUDyyyyfff is the mission number)
  # - a file containing all the variables interpolated hourly, named HUDyyyyfff_TSG_hourly.csv
  # - a plot of each variable versus time and versus longitude, named TSG_xxxxdata_xxxx_HUDyyyyfff.jpeg and  TSG_xxxx_long.jpeg, 
  # - a map of station locations, named TSG_map.jpeg

#**Folder structure:
#C:\TSG_process - working dir
#\1code_readTSGdata - functions to read and cleanup TSG log files and calculate oxygen saturation called from the script "1readTSGdata.R"
#\2code_interp_plot_TSGdata - functions to interpolate the processed data over time called from the script "2TSG_interp.R"
#\2code_interp_plot_TSGdata\hourly_TSG_dataplots - plots and tables of interpolated data from the script "2TSG_interp.R"
#\3code_plot_TSGdata - plots of the interpolated data created from the script "3TSGcontourplot.R"
#\4comparesamples - functions to compare and plot TSG data with water samples and  CTD data  called from the script "4TSGcompare_ctd_samples.R"
#\processed - formatted log files from TSG with corrupted data removed created from the script "1readTSGdata.R"
#\raw - log files from TSG 
#\bottle - water sample data analysed on the ship typically salts, Chlorophyll and oxygen 
#\ctd- ctd files in ODF format processed on the ship
#\elog - log files from ELOG

# FUNCTIONS:

#   interp.tsghourly - filename interpTSGhourly.R
#   assemble.tsg - filename assemble_TSG.R
#   plot.tsg - filename plot_tsg.R

# author Diana Cardoso
# Oct 2021
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

rm(list=ls()) #remove everything in the working environment.

#enter cruise number
missionnum <- "xxxyyyy000"

#enter map boundary limits used by plots in the function plot.tsg
lonlim <- c(-72,-55)
latlim <- c(40,48)

#Install and load specific package versions
#install.packages("oce", "ocedata", "ggplot2")
library(oce)
library(ocedata)
library(ggplot2)
data("coastlineWorldFine")

#set working directory
setwd("C:/TSG_process") # set working directory
#wd <- getwd()
#setwd(wd)
parent <- getwd()
pathprocessed = "processed/" # path with processed output files 
pathfunctions = "2code_interp_plot_TSGdata/"

# function file names
source("2code_interp_plot_TSGdata/interpTSGhourly.R")
source("2code_interp_plot_TSGdata/assemble_TSG.R")
source("2code_interp_plot_TSGdata/plot_tsg.R")

# list the processed log files from TSG including path from the processed folder
filesflow <- list.files(path= pathprocessed, pattern = 'FLOWdata.*\\.csv', full.names = TRUE)
filespco2 <- list.files(path= pathprocessed, pattern = 'PCO2data.*\\.csv', full.names = TRUE)
filestsg <- list.files(path= pathprocessed, pattern = 'TSGdata.*\\.csv', full.names = TRUE)
filesnmea <- list.files(path= pathprocessed, pattern = 'TSGposition.*\\.csv', full.names = TRUE)

#determine the latest start date/time from all files
tsgall <- data.frame(read.csv(filestsg[1], header = TRUE))
flowall <- data.frame(read.csv(filesflow[1], header = TRUE))
pco2all <- data.frame(read.csv(filespco2[1], header = TRUE))
nmeaall <- data.frame(read.csv(filesnmea[1], header = TRUE))

start_date <- max(tsgall$time[1], flowall$time[1],pco2all$time[1],nmeaall$time[1])
start_date <- round(as.POSIXct(start_date,tz = 'UTC'),"hour") # round to nearest hour

#determine the earliest end date/time from all files
tsgalle <- data.frame(read.csv(filestsg[length(filestsg)], header = TRUE))
flowalle <- data.frame(read.csv(filesflow[length(filesflow)], header = TRUE))
pco2alle <- data.frame(read.csv(filespco2[length(filespco2)], header = TRUE))
nmeaalle <- data.frame(read.csv(filesnmea[length(filesnmea)], header = TRUE))

end_date <- min(tsgalle$time[length(tsgalle$time)], flowalle$time[length(flowalle$time)],pco2alle$time[length(pco2alle$time)],nmeaalle$time[length(nmeaalle$time)])
  
# frequency of data collection
freq <- data.frame(frequency(tsgall$time),frequency(flowall$time),frequency(pco2all$time),frequency(nmeaall$time) )

# vector of time by hour between start and end time determined above
hourrate <- seq(from = as.POSIXct(start_date,tz = 'UTC'), to = as.POSIXct(end_date,tz = 'UTC'), by = "hour")

# read each processed file and interpolate each variable hourly and save in individual csv files and 
# plots time series of each variable using the function assemble.tsg
assemble.TSG(filesflow, hourrate, missionnum)
assemble.TSG(filespco2, hourrate,missionnum)
assemble.TSG(filestsg, hourrate,missionnum)
assemble.TSG(filesnmea, hourrate,missionnum)

# read each hourly interpolated file created above and assemble all variables into 
# 1 file named xxxyyy000_TSG_hourly.csv 
filesinterp <- list.files(path= "2code_interp_plot_TSGdata/hourly_TSG_dataplots", pattern = 'TSG_.*\\.csv', full.names = TRUE)
interpdataall <- data.frame(read.csv(filesinterp[1], header = TRUE))
filesinterp2 <- filesinterp[2:length(filesinterp)]

for (i in filesinterp2){
  tsginterpall <- data.frame(read.csv(i, header = TRUE))
  coln <- colnames(tsginterpall) #Get the column name
  cc <- coln[3]
  coln2 <- colnames(interpdataall) #Get the column name
  
  interpdataall <- cbind(interpdataall, tsginterpall[,3])# add new column
  colnames(interpdataall)<-c(coln2,cc) #adding name
}

interpdataall[,7][interpdataall[,7] > 959] <- NA
interpdataall[,8][interpdataall[,8] > 959] <- NA
interpdataall[,12][interpdataall[,12] > 959] <- NA
interpdataall[,10][interpdataall[,10] < 0.5] <- NA
interpdataall[,10][interpdataall[,10] > 15] <- NA

#write a csv with all variables interpolated hourly 
filename <- paste0("2code_interp_plot_TSGdata/hourly_TSG_dataplots/",missionnum,"_TSG_hourly.csv")
write.csv(interpdataall, file = filename,row.names = FALSE)

# plot each variable vs longitude, plot station locations and 
# longitude and latitude limits for plots (lonlim,latlim)

plot.tsg(interpdataall,lonlim,latlim)

# Record session information
sink("session_info.txt")
sessionInfo()
sink()
