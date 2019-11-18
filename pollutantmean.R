#PART 1

# Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) accross a specified
# list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector
# monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specidfied in the 
# 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded
# as NA. 


pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  #path <- file.path(getwd(),directory)
  #setwd(file.path(getwd(),directory))
  
  path <- paste0(getwd(),"/", directory) #this one is more general
  
  data <- data.frame()
  for (i in id) {
    if (i < 10) {
      dat <- read.csv(paste(path,"/00", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
      data <- rbind(data,dat)
    }
    else if (i < 100) {
      dat <- read.csv(paste(path,"/0", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
      data <- rbind(data,dat)
    }
    else {
      dat <- read.csv(paste(path,"/", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
      data <- rbind(data,dat)
    }
    
  }
  return(mean(data[,pollutant], na.rm = T))
}

