#***Desciption:  Script to read all log files from underway flow through system 
#which include NMEA, flow, sensor and PCO2 data and clean it up.

#***INPUT: Log files generated every 24 hours and are named:
# NMEA_yyyymmdd.CSV, FLOW_yyyymmdd.CSV, TSGOUT_yyyymmdd.CSV, PCO2_yyyymmdd.CSV

#***OUTPUT: writes one csv file per log file 
# removes or replaces corrupted data and incomplete lines in log files with 999 or NaN
# and adds column headers 

#**Folder structure:
#C:\TSG_process - working dir
#\1code_readTSGdata - dir for script "1readTSGdata.R" to read all log files and process
#\2code_interp_plot_TSGdata - interpolate the processed data over time
#\3code_plot_TSGdata - plot the interpolated data
#\4comparesamples -  compare TSG water samples with TSG data and compare CTD and TSG data
#\processed - formatted log files from TSG with corrupted data removed
#\raw - log files from TSG 


# author Diana Cardoso
# Oct 2023
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

rm(list=ls()) #remove everything in the working environment.

# Install and load specific package versions
#install.packages("oce", "csasAtlPhys")
library(oce)
library(csasAtlPhys)

setwd("C:/TSG_process") # set working directory
#wd <- getwd()
#setwd(wd)
parent <- getwd()
pathrawdata = "raw/"  # path with raw files 
pathprocesseddata = "processed/" # path with processed output files 

#List of Functions
source("1code_readTSGdata/readflowdata.R")
source("1code_readTSGdata/readFLOW.R")
source("1code_readTSGdata/readnmeadata.R")
source("1code_readTSGdata/readNMEA.R")
source("1code_readTSGdata/readpco2data.R")
source("1code_readTSGdata/readpco2_2.R")
source("1code_readTSGdata/readtsgdata.R")
source("1code_readTSGdata/readTSGout.R")
source("1code_readTSGdata/TSG_optode_SatO2_4.R")

# read and process data
read.flowdata(pathrawdata,pathprocesseddata)
read.nmeadata(pathrawdata,pathprocesseddata)
read.pco2data(pathrawdata,pathprocesseddata)
read.tsgdata(pathrawdata,pathprocesseddata)

# Record session information
sink("session_info.txt")
sessionInfo()
sink()