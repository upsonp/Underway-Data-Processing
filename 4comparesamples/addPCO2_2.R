#' @title  add PCO2 data to the tables generated from the read_elog_tsg3.R function
#' 
#' @description   read TSG_bottletable2.csv and TSG_samples.csv tables generated from the read_elog_tsg3.R function
#'  and read PCO2 processed data from underway flow through system, and add PCO2 data to the TSG_bottletable2.csv table
#' 
#' @param  TSG processed files directory (pathprocessed)
#' 
#' @output is in the \4comparesamples folder and includes:
# - TSG_PCO2_bottletable2.csv - table: TSG data when the sample was taken, 12 readings per minute 
#'
#' @author Diana Cardoso
# Oct 2021
# Fisheries and Oceans Canada,Bedford Institute of Oceanography, Dartmouth, N.S. Canada B2Y 4A2

read.addPCO2<- function(pathprocessed){

filestsgpco2 <- list.files(path= pathprocessed, pattern = 'PCO2data.*\\.csv', full.names = TRUE)
TSG_bottletable <- data.frame(read.csv("4comparesamples/TSG_bottletable2.csv", header = TRUE))
TSG_samples <- data.frame(read.csv("4comparesamples/TSG_samples.csv", header = TRUE))
# insert a column
TSG_bottletable <- cbind(TSG_bottletable, 0) 
TSG_bottletable <- cbind(TSG_bottletable, 0)
nn <- colnames(TSG_bottletable)
nn[14] <- "PCO2"
nn[15] <- "PCO2time"
colnames(TSG_bottletable) <- c(nn)
cc <- 1
b=1
for (g in filestsgpco2){
  pco2all <- data.frame(read.csv(filestsgpco2[cc], header = TRUE))
  cc <- cc+1
  pco2posix  <- data.frame(time=as.POSIXct(pco2all$time, tz = 'UTC'))
  print(g)
  
  for (t in 1:length(TSG_samples$time)){
    print(t)
    z <- strptime(TSG_samples$POSIXctdate[t], format="%Y-%m-%d %H:%M:%OS", tz = 'UTC')
    op <- options(digits.secs = 0)
    
    for (h in 1:length(pco2posix$time)){
      
      ctsg <- strptime(pco2posix$time[h], format="%Y-%m-%d %H:%M", tz = 'UTC')
      c2 <- strptime(z, format="%Y-%m-%d %H:%M", tz = 'UTC')

      #op <- options(digits.secs = 0)    
      if (ctsg == c2){
        TSG_bottletable[b,14] <- pco2all$CO2_ppm[h]
        TSG_bottletable[b,15] <- pco2all$time_pco2[h]
        pco2all$time[h]
        b=b+1
        
      }
    }
  }
}           
write.csv(TSG_bottletable, file = "4comparesamples/TSG_PCO2_bottletableall.csv")                              

pco2_sec <-data.frame(read.csv("4comparesamples/TSG_PCO2_bottletableall.csv", header = TRUE))

b=1
pco2_5sec <-  0
pco2_5sectime  <-  0
for (d in seq(1,length(pco2_sec$PCO2), by=59)){
  for (h in seq(d,(d+58), by=5)){
    pco2_5sec[b] <- mean(pco2_sec$PCO2[h:(h+4)])
    pco2_5sectime[b] <- pco2_sec$PCO2time[h]
    
    b=b+1
}
}
TSG_bottletable2 <- data.frame(read.csv("4comparesamples/TSG_bottletable2.csv", header = TRUE))
# insert a column
TSG_bottletable2 <- cbind(TSG_bottletable2, 0) 
TSG_bottletable2 <- cbind(TSG_bottletable2, 0)
nn <- colnames(TSG_bottletable2)
nn[14] <- "PCO2"
nn[15] <- "PCO2time"
colnames(TSG_bottletable2) <- c(nn)
TSG_bottletable2$PCO2 <- pco2_5sec[1:length(TSG_bottletable2$PCO2)]
TSG_bottletable2$PCO2time <- pco2_5sectime[1:length(TSG_bottletable2$PCO2)]

write.csv(TSG_bottletable2, file = "4comparesamples/TSG_PCO2_bottletable2.csv")

}