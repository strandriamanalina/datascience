# Part I
pollutantmean <- function(directory, pollutant, id = 1:332) {
  # Change id format to output the correct one
  format <- formatC(id, width = 3, format = "d", flag = "0")
  # store as a list, combine into one dataframe
  list <- sapply(format, function(x){list.files(path = directory, pattern = x)})
  data <- lapply(list, function(x) {read.csv(paste(directory, "/", x, sep=""))})
  data <- do.call(rbind, data)
  
  # remove NAs
  data1 <- data[[pollutant]][!is.na(data[[pollutant]])]
  
  # calculate the mean of the pollutant
  meanpollutant <- mean(data1)
  meanpollutant
}
