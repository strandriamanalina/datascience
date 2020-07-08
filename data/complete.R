# Part II
complete <- function(directory, id=1:332) {
  # Change id format to output the correct one
  format <- formatC(id, width = 3, format = "d", flag = "0")
  # store as a list, combine into one dataframe
  list <- sapply(format, function(x){list.files(path = directory, pattern = x)})
  data <- lapply(list, function(x) {read.csv(paste(directory, "/", x, sep=""))})
  # reports the number of completely observed cases in each data file
  compl <- function(x) {
    id = names(x)
    nobs = sum(complete.cases(x))
  }
  
  result <- data.frame(nobs=sapply(data, compl))
  result$id <- as.numeric(row.names(result))
  row.names(result) <- c()
  result <- result[,c(2,1)]
  result
}
