setwd("/Users/yangchen/Desktop/Coursera/Data_Sci_Coursera/rprog-data-ProgAssignment3-data")
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
                with(par_mort, X[Y == min(Y)])
        }
}
