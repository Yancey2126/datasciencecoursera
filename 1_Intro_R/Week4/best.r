setwd("/Users/yangchen/Desktop/Coursera/Data_Sci_Coursera/Intro_R/Week4/rprog-data-ProgAssignment3-data")
getwd()

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])

best <- function(state, outcome) {
        options(stringsAsFactors = FALSE)
        # Read outcome data
        ocdata <- read.csv("outcome-of-care-measures.csv")
        # Create vectors of all valid states and outcomes
        states <- unique(ocdata$State)
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        # Check that state and outcome are valid
        if (!state %in% states) {
                stop("invalid state")
        } else if (!outcome %in% outcomes) {
                stop("invalid outcome")
        } else {
                # Extract corresponding mortality data and replace the col names
                mortality <- ocdata[, c(11, 17, 23)]
                # Convert character columns to numeric 
                mortality <- as.data.frame(sapply(mortality, as.numeric)) 
                # Extract the mortality data of particular outcome and state
                # then add hospital names
                mortality <- data.frame(State = ocdata$State, Name = ocdata$Hospital.Name, mortality)
                colnames(mortality) <- c("State", "Name", "heart attack", "heart failure", "pneumonia")
                stateMort <- mortality[mortality$State == state, ]
                par_mort <- data.frame(X = stateMort$Name, Y = stateMort[[outcome]])
                # Exclude NAs
                par_mort <- par_mort[complete.cases(par_mort), ]
                # Return the best hospital name
                vec <- par_mort$X[which(par_mort$Y == min(par_mort$Y))]
                # Solve the tie situation
                sort(vec)[1]
        }
}

best("MD", "pneumonia")


best("SC", "heart attack")

best("NY", "pneumonia")

best("AK", "pneumonia")



best1 <- function(state, outcome) {
        
        ## Read the outcome data
        dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        if (!state %in% unique(dat[, 7])) {
                stop("invalid state")
        }
        switch(outcome, `heart attack` = {
                col = 11
        }, `heart failure` = {
                col = 17
        }, pneumonia = {
                col = 23
        }, stop("invalid outcome"))
        ## Return hospital name in that state with lowest 30-day death rate
        df = dat[dat$State == state, c(2, col)]
        df[which.min(df[, 2]), 1]
}
best1("TX", "heart attack")