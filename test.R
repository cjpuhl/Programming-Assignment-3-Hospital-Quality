rankH <- data.frame()
for(i in seq_along(states)){
        
        outcome <- s[[states[i]]]
        
        if (num == "worst"){
                ##sorts data by death rate
                outcome <- outcome[order(-outcome[, 3], outcome$Hospital), ]
                View(outcome)
                ##will select first row
                if(num == "worst") num <- 1
                ##will assign selected row 
                if(num > 0) outcome <- outcome[num, ]
        }
        
        if(num == "best" || num > 0){
                ##sorts data by death rate
                outcome <- outcome[order(outcome[, 3], outcome$Hospital), ]
                ##will select first row
                if(num == "best") num <- 1
                ##will assign selected row 
                if(num > 0) outcome <- outcome[num, ]
                
        }
        
        addrow <- data.frame(Hospital = outcome[1], State = outcome[2])
        rankH <- rbind(rankH, addrow)
        
}

rankH