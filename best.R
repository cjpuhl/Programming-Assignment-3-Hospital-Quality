##ranks the hospytal with lowest death rate by state
best <- function(state, decease) {
        
        ## Read outcome data
        outcome <- as.data.frame(read.csv("data/outcome-of-care-measures.csv"))
        
        if(!(state %in% outcome$State)){
                ##stops if arguments given are invalid
                stop("invalid state") 
        }

        if(!(decease %in% c("heart attack", 
                                "heart failure", "pneumonia"))){
                ##stops if arguments given are invalid
                stop("invalid outcome")
        }
        
        ##selects only the hospitals in the given state
        outcome <- outcome[outcome$State == state,]
        
        ##selects only useful columns
        outcome <- outcome[, c(2, 7, 11, 17, 23)]
        names(outcome) <- c("Hospital", "State", "heart attack", 
                            "heart failure", "pneumonia")
        
        ##select only necessary decease and converting values to numeric
        outcome <- outcome[, c("Hospital", "State", decease)]
        outcome[, decease] <- as.numeric(outcome[, decease])
        
        ##removing NAs
        outcomenoNA <- complete.cases(outcome)
        outcome <- outcome[outcomenoNA, ]
        
        ##sorts data by death rate
        outcome <- outcome[order(outcome[, 3], outcome$Hospital), ]
        outcome[1,1]
}

