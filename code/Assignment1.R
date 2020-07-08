
# A function to unzip
# library(plyr)
# outDir <- "data"
# zipF=list.files(path = "data", pattern = "*.zip", full.names = TRUE)
# ldply(.data=zipF, .fun=unzip, exdir=outDir)

# A function to read a defined CSV
reading <- function(file) {
  data <- read.csv(paste("data/specdata/", file, ".csv", sep=""))
  data
}

id <- 1:332
directory <- "specdata"
format <- formatC(id, width = 3, format = "d", flag = "0")
list <- sapply(format, function(x){list.files(path = directory, pattern = x)})
data <- lapply(list, function(x) {read.csv(paste(directory, "/", x, sep=""))})

compl <- function(x) {
    id = names(x)
    nobs = sum(complete.cases(x))
  }

result <- data.frame(nobs=sapply(data, compl))
result$id <- as.numeric(row.names(result))
row.names(result) <- c()
result <- result[,c(2,1)]



# data <- do.call(rbind, data)
# head(data)
# 
# pollutant <- "sulfate"
# datawtNA <- data[[pollutant]][!is.na(data[[pollutant]])]



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


setwd("data")
getwd()

pollutantmean("specdata", "sulfate", 1:10)

pollutantmean("specdata", "nitrate", 70:72)

pollutantmean("specdata", "nitrate", 23)


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

complete("specdata", c(2, 4, 8, 10, 12))

complete("specdata", 30:25)

complete("specdata", 3)



# Part III

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

cr<- corr("specdata", 150)
head(cr)
summary(cr)

cr <- corr("specdata", 400)
head(cr)


cr <- corr("specdata", 5000)
summary(cr)
length(cr)

cr <- corr("specdata")
summary(cr)
length(cr)

