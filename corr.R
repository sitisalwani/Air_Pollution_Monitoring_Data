

#PART 3

# Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation 
# between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is 
# greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold 
# requirement. Of no monitors meet the threshold requiremenr, then the function should return a numeric vector of length 0.

corr <- function(directory, threshold = 0) {
  path <- paste0(getwd(),"/", directory)
  corr_vect <- NULL
  for (i in 1:332) {
    if (i < 10) {
      dat <- read.csv(paste(path,"/00", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
    }
    else if (i < 100) {
      dat <- read.csv(paste(path,"/0", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
    }
    else {
      dat <- read.csv(paste(path,"/", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
    }
    
    data <- dat[complete.cases(dat),]
    if (nrow(data) > threshold) {
      corr_vect <- c(corr_vect, cor(data[,"sulfate"], data[, "nitrate"]))
    }
  }
  
  return(corr_vect)
}

