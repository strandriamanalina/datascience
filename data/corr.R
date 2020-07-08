# part III

corr <- function(directory, threshold = 0) {
  # import the function
  format <- formatC(1:332, width = 3, format = "d", flag = "0")
  # store as a list, combine into one dataframe
  list <- sapply(format, function(x){list.files(path = directory, pattern = x)})
  data <- lapply(list, function(x) {read.csv(paste(directory, "/", x, sep=""))})
  data <- do.call(rbind, data)
  
  # remove NAs
  data1 <- data[complete.cases(data),]
  
  # select cases superior to the threshold
  result <- complete("specdata",1:332)
  result_threshold <- result[result$nobs>threshold,]
  
  if (length(result_threshold)==0){
    cr <- vector(mode = "numeric", length = OL)
    cr
  } else{
    
    merged <- merge(data1, result_threshold, by.x = "ID", by.y = "id", all.x = FALSE, all.y = FALSE)
    
    # Calculate correlation
    out <- split(merged, f = merged$ID) 
    cr <- lapply(out, function(x){cor(x[,3], x[,4])})
    cr <- unlist(cr)
    names(cr) <- c() 
    cr
  }
}
