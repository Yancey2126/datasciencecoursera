setwd("/Users/yangchen/Desktop/Coursera/Data_Sci_Coursera/Intro_R")
getwd()

complete <- function(directory, id = 1:332) {
        # read all monitor files in the given directory, subset by id range
        specifiedFiles <- list.files(directory, full.names = TRUE)[id]
        # create a list that read all files above and merge data together
        # set "stringsAsFactors" to FALSE, otherwise "date" would be a 
        # factor column with different levels
        data <- lapply(specifiedFiles, function(x) read.csv(x, stringsAsFactors = FALSE))
        
        # print(
        #         lapply(data, function(x){
        #                 sapply(data.frame(x), class)
        #         }
        #         )
        # )

        theStats <- bind_rows(data)
        # theStats <- do.call("rbind", data) 
        # Add a new column indicating complete status
        theStats$complete <- complete.cases(theStats)
        # Extract the number of complete cases grouped by id
        results <- aggregate(theStats$complete, by = list(Category = theStats$ID), FUN = sum)
        colnames(results) <- c("id", "nobs")
        return(results)
       
}


cc <- complete("specdata", 332:1)
set.seed(42)
use <- sample(332, 10)
use
print(cc[use, "nobs"])

