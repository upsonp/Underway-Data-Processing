##3TSGcontourplot.R

#**NOTE may need to remove data from the interpolated data file when TSG is shut on and off throughout mission

#***Desciption: Script to read interpolated data and plot map contour plots from underway flow 
# through system.  plots are saved in the \3code_plot_TSGdata folder
# The interpolated data file is output from the code 2TSG_interp.R and 
# saved in the \2code_interp_plot_TSGdata\hourly_TSG_dataplots with name HUDyyyyfff_TSG_hourly.csv

#***INPUT: 
# 1- The interpolated data file output from the code 2TSG_interp.R and 
# saved in the \2code_interp_plot_TSGdata\hourly_TSG_dataplots folder with name HUDyyyyfff_TSG_hourly.csv
# May need to delete bad lines from this file if the TSG was turned off and on during mission
# 2- Topographic data downloaded boundaries west = -75, east = -50, south = 38, north = 50,
# 3- The longitude and latitude limits for plots variable lonlim and latlim 
# 4- The data coastlineWorldFine downloaded from the ocedata package

# OUTPUT is in the \2code_interp_plot_TSGdata\hourly_TSG_dataplots folder and includes:
# - one plot per variable of hourly interpolated data, named TSG_variable.png
# - a NetCDF file containing Topographic data

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

# author Diana Cardoso
# Oct 2021
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

rm(list=ls()) #remove everything in the working environment.

#Install and load specific package versions
#install.packages("oce", "ocedata")
library(oce)
library(ocedata)

#set working directory
setwd("C:/TSG_process") # set working directory
#wd <- getwd()
#setwd(wd)
parent <- getwd()
interpdata <- "2code_interp_plot_TSGdata/hourly_TSG_dataplots/"

data('coastlineWorldFine', package="ocedata")

#Topographic data downloaded from a data server that holds the ETOPO1 dataset 
#(Amante, C. and B.W. Eakins, 2009) and saved as a netCDF file 
topoFile <- download.topo(west = -75, east = -50,
                          south = 38, north = 50,
                          resolution = 1)
ocetopo <- read.topo(topoFile)

# Read the file containing all the variables interpolated hourly, named HUDyyyyfff_TSG_hourly.csv
fileinterp <- list.files(path= interpdata, pattern = '*_TSG_hourly.csv', full.names = TRUE)
d <- read.csv(fileinterp)

lon <- d[['longitude']] * -1
lat <- d[['latitude']]

variables <- c('Conductivity_S_m', 
               'FluorescenceUV', 
               'pH', 
               'Temperature_TSG_ITS_90', 'CO2_ppm', 'O2Concentration_ml_L',
               'Fluorescence','salinity_PSU')

proj <- '+proj=merc'
fillcol <- 'lightgray'
lonlim <- c(-70, -56)
latlim <- c(41.5, 49)
for (var in variables){
  png(filename = paste0("3code_plot_TSGdata/",'TSG_', var, '.png'), width = 6, height = 4,
      units = 'in', res = 250, pointsize = 12)
  par(mar = c(2.5, 2.5 , 1.5, 1))
  cm <- colormap(z = d[[var]], col = oceColorsJet) # can change color scheme
  drawPalette(colormap = cm, zlab = var)
  mapPlot(coastlineWorldFine, 
          longitudelim = lonlim,
          latitudelim = latlim,
          col = fillcol, 
          proj = proj,
          grid = c(2,1))
  bathylevels <- c(-3000, -2000, -1000, -200)
  bathycol <- 'lightgrey'
  mapContour(longitude = ocetopo[['longitude']],
             latitude = ocetopo[['latitude']],
             z = ocetopo[['z']],
             levels = bathylevels,
             lwd = 0.8, col = bathycol)
  mapPoints(lon, lat, pch = 20, col = cm$zcol)
  dev.off()
}

# Record session information
sink("session_info.txt")
sessionInfo()
sink()