

#PART 2

# Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation
# between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is 
# greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold
# requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0.

complete <- function(directory, id = 1:332) {
  path <- paste0(getwd(),"/", directory)
  data <- data.frame()
  for (i in id) {
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
    nobs <- sum(complete.cases(dat))
    data9 <- data.frame(i, nobs)
    data <- rbind(data,data9)
    
  }
  return(data)
}

