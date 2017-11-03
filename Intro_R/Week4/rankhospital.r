setwd("/Users/yangchen/Desktop/Coursera/Data_Sci_Coursera/Intro_R/Week4/rprog-data-ProgAssignment3-data")
getwd()

rankhospital <- function(state, outcome, num = "best") {
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
                # Sort by rankings and then by alphabetic name
                arrangedData <- arrange(par_mort, par_mort$Y, par_mort$X)

                if (num == "best") {
                        arrangedData[1, 1]
                } else if (num == "worst") {
                        arrangedData[nrow(arrangedData), 1]
                } else {
                        arrangedData[num, 1]   
                }
               
        }
}


rankhospital("TX", "heart failure", 4)

rankhospital("MD", "heart attack", "worst")

rankhospital("MN", "heart attack", 5000)
