rankall <- function(decease, num = "best") {
        ## Read outcome data
        outcome <- as.data.frame(read.csv("data/outcome-of-care-measures.csv"))
        
        ## Check that state and outcome are valid
        if(!(decease %in% c("heart attack", 
                            "heart failure", "pneumonia"))){
                ##stops if arguments given are invalid
                stop("invalid decease")
        }
        
        if(!(num %in% c("best", "worst") || num > 0 )){
                ##stops if arguments given are invalid
                stop("invalid num")
        }
        
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
        
        ##split the data frame by state
        s <- split(x = outcome, f = outcome$State)
        
        dados <- sapply(X = s, FUN = function(s, i = num){
                if(i == "worst") {
                        i = 1
                ##if worst then orders descending (-s) and returns first
                        s <- s[order(-s[, 3], s[, 1]), ]
                        c(s[i, 1], s[i, 2])
                }else if(i == "best"){
                        i = 1
                ##if best then orders ascending (s) and returns first
                        s <- s[order(s[, 3], s[, 1]), ]
                        c(s[i, 1], s[i, 2])
                }else if(i > 0){
                ##if i is numeric then orders ascending and returns i object
                        s <- s[order(s[, 3], s[, 1]), ]
                        c(s[i, 1], s[i, 2])
                }
                
        })
        ##returns transposed data
        dados <- t.default(dados)
        ##converts to data frame and adds names
        dados <- as.data.frame(dados)
        names(dados) <- c("hospital", "state")
        dados
        
}
