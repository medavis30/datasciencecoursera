rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    choice_outcome <- c("heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if((state %in% outcome_data[, 7]) == FALSE){
        
        stop("invalid state")
        
    }
    
    if((outcome %in% choice_outcome) == FALSE){
        
        stop("invalid outcome")
        
    }
    
    ## Pull out rows which satisfy the "state" input
    my_state <- subset(outcome_data,outcome_data[, 7] == state)
    
    ## Use the appropriate column which satisfies the "outcome" input
    if(outcome == "heart attack"){
        
        my_outcome <- as.numeric(my_state[, 11])
        
    }else if(outcome == "heart failure"){
        
        my_outcome <- as.numeric(my_state[, 17])
        
    }else{
        
        my_outcome <- as.numeric(my_state[, 23])
        
    }
    
    ## Return NA if "num" is larger than number of hospitals in the given "state"
    if (is.numeric(num) == TRUE) {
        
        if (length(outcome_data[, 2]) < num) {
            
            return(NA)
        
        }
        
    }
    
    ## Remove NAs
    complete <- complete.cases(my_outcome)
    
    clean_outcome <- my_state[complete, ]
    
    ## Order the dataframe in ascending order based on lowest rate than alphabetical
    if(outcome == "heart attack"){
        
        select_outcome <- names(clean_outcome)[11]
        
    }else if(outcome == "heart failure"){
        
        select_outcome <- names(clean_outcome)[17]
        
    }else{
        
        select_outcome <- names(clean_outcome)[23]
        
    }
    
    select_hospital <- names(clean_outcome)[2]
    
    group <- with(clean_outcome, order(clean_outcome[select_outcome], clean_outcome[select_hospital]))
    
    ranked_data <- clean_outcome[group, ]
    
    ## Link "best" or "worst" input to appropriate numerical ranking
    if (is.character(num) == TRUE) {
        
        if (num == "best") {
            
            num = 1
            
        }
        else if (num == "worst") {
            
            if(outcome == "heart attack"){
                
                num = length(ranked_data[, 11])
                
            }else if(outcome == "heart failure"){
                
                num = length(ranked_data[, 17])
                
            }else{
                
                num = length(ranked_data[, 23])
                
            }
            
        }
        
    }
    
    ranked_data[num, 2]
    
}