setwd("/Users/yangchen/Desktop/Coursera/Data_Sci_Coursera/Intro_R")
getwd()

pollutantmean1 <- function(directory, pollutant, id = 1:332) {
        # read all monitor files in the given directory, subset by id range
        specifiedFiles <- list.files(directory, full.names = TRUE)[id]
        # create a list that read all files above and merge data together
        data <- lapply(specifiedFiles, function(x)read.csv(x)[[pollutant]])
        # unlist the data and calculate the mean
        theStats <- unlist(data)
        mean(theStats, na.rm = TRUE)
}

pollutantmean2 <- function(directory, pollutant, id = 1:332) {
        data <- list()
        for(i in id) {
                num = formatC(i, width = 3, flag = "0") #creates a 3-digit string representing an integer
                D <- paste(directory, paste(num, "csv", sep = "."), sep = "/") #D is the file name string
                x <- read.csv(D)[[pollutant]]
                data <- c(data,x) #data is a list of all the data frames we needed to import
        }
        theStats <- unlist(data)
        mean(theStats, na.rm = TRUE)
}