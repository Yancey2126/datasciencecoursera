setwd("/Users/yangchen/Desktop/Coursera/Data_Sci_Coursera/Intro_R/Week4/rprog-data-ProgAssignment3-data")
getwd()

rankall <- function(outcome, num = "best") {
        # Read the outcome data
        ocdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        # Check that outcome is valid
        states = unique(ocdata$State)
        switch(outcome, "heart attack" = {
                col = 11
        }, "heart failure" = {
                col = 17
        }, "pneumonia" = {
                col = 23
        }, stop("invalid outcome"))
        
        ocdata[, col] <- as.numeric(ocdata[, col]) 
        # Keep hospital names, states and mortality rates
        ocdata <- ocdata[, c(2, 7, col)]
        ocdata <- na.omit(ocdata)
        
        # head(ocdata)
        rank_st <- function(state) {
                # Extract sub dataframe of particular state
                df <- ocdata[ocdata[, 2] == state, ]
                sortdf <- arrange(df, df[, 3], df[, 1])
                switch(num, best = {
                        num = 1
                }, worst = {
                        num = nrow(sortdf)
                })
                sortdf[num, 1]
        }
        
        # Loop throught all states and find the list of best hospitals
        output <- do.call(rbind, lapply(states, rank_st))
        # Add states column
        output <- data.frame(output, states)
        output <- output[order(output[, 2]), ] # arrange doesn't work
        rownames(output) = output[, 2]
        colnames(output) = c("hospital", "state")
        data.frame(output)
}

head(rankall("heart attack", 20), 10)

