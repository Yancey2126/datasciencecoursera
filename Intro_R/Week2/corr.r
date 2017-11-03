setwd("/Users/yangchen/Desktop/Coursera/Data_Sci_Coursera/Intro_R")
getwd()
corr <- function(directory, threshold = 0) {
        vec <- numeric()
        filelist <- list.files(directory, full.names = TRUE)
        # for each monitor, determine whether it hits threshold via complete func.
        for (i in filelist) {
                data <- read.csv(i)
                value <- sum(complete.cases(data))
                # extract all complete cases
                complete <- data[!is.na(data$sulfate) & !is.na(data$nitrate), ]
                if (value > threshold) {
                        # Calculate the correlation
                        sul <- complete[["sulfate"]]
                        nit <- complete[["nitrate"]]
                        cr <-  cor(nit,sul,use = "pairwise.complete.obs")
                        vec <- c(vec, cr)
                }
        }
        vec
}

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

# corr1 <- function(directory, threshold = 0) {
#         files_list <- dir(directory, full.names = TRUE)
#         output <- numeric()
#         for (i in files_list) {
#                 corval <- 0        
#                 data <- read.csv(i) #Bind CSV files in data 
#                 value <- sum(complete.cases(data))
#                 data <- data[complete.cases(data),]
#                 if (value > threshold) { 
#                         sulfate <- data[["sulfate"]]
#                         nitrate <- data[["nitrate"]]
#                         corval <- cor(nitrate,sulfate,use = "pairwise.complete.obs")            
#                         output <- c(output,corval)
#                 }
#         }
#         output
#         
# }
