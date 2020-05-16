
rankhospital <- function(state, decease, num = "best") {
        
        ## Read outcome data
        outcome <- as.data.frame(read.csv("data/outcome-of-care-measures.csv"))
        
        
        ## Check that state and outcome are valid
        if(!(state %in% outcome$State)){
                ##stops if arguments given are invalid
                stop("invalid state") 
        }
        
        if(!(decease %in% c("heart attack", 
                            "heart failure", "pneumonia"))){
                ##stops if arguments given are invalid
                stop("invalid decease")
        }
        
        if(!(num %in% c("best", "worst") || num > 0)){
                ##stops if arguments given are invalid
                stop("invalid num")
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
        
        if(num == "best") num <- 1              ##will select first row
        if(num == "worst") num <- nrow(outcome) ##will select last row
        if(num > 0) outcome <- outcome[num, 1]  ##will assign selected row 
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        outcome
}

