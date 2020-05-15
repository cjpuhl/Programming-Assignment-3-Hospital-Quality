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
        
        else{
                ##selects only the hospitals in the given state
                outcome <- outcome[outcome$State == state,]
                
                ##selects only useful columns
                outcome <- outcome[, c(2, 7, 11, 17, 23)]
                names(outcome) <- c("Hospital", "State", "heart attack", 
                                    "heart failure", "pneumonia")
        }
        
        if (decease == "heart attack"){
                
                ##converts column to numeric
                outcome[, "heart attack"] <- as.numeric(outcome[, "heart attack"])
                
                ##orders data by decease column and hospital name
                outcome <- outcome[order(outcome$`heart attack`, 
                                         outcome$Hospital),]
        }
        
        else if(decease == "heart failure"){
                
                ##converts column to numeric
                outcome[, "heart failure"] <- as.numeric(outcome[, "heart failure"])
                
                ##orders data by decease column and hospital name
                outcome <- outcome[order(outcome$`heart failure`, 
                                         outcome$Hospital),]
        }
        
        else if(decease == "pneumonia"){
                ##converts column to numeric
                outcome[, "pneumonia"] <- as.numeric(outcome[, "pneumonia"])
                
                ##orders data by decease column and hospital name
                outcome <- outcome[order(outcome$pneumonia, 
                                         outcome$Hospital),]
        }
        
        print(paste("Best Hospital for", decease, "in", 
                    state, "is", outcome[1,1]))
        outcome
}

outcome <- best("NY", "hert attack")

