#' @title interpolate data from processed log files from underway flow through system
#' 
#' @description input data frame (datacol) and rate to interpolate (hourrate) from the assemble.tsg function 
#' interpolates the processed log files from the underway flow through system 
#' 
#' @param file the data frame (datacol) with data and data-time and the rate to interpolate (hourrate) 
#' from the assemble.tsg function
#' 
#' @output a data frame (TSG_hour) of interpolated data and data-time
#' 
#' @author Diana Cardoso
# Oct 2021
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

interp.tsghourly <- function(file, hourrate){
  #Return a list of points which linearly interpolate data in file
  # hourrate containing values specifying where interpolation is to take place.
  interhourly <- approx(file, xout=hourrate, na.rm = TRUE ) 
  flowcolnames <- colnames(interhourly)
  
  TSG_hour <- data.frame(interhourly[["x"]],interhourly[["y"]])
  colnames(TSG_hour) <- c(flowcolnames[2],flowcolnames[3])
  
  TSG_hour
  
}

