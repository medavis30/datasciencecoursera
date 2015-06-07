complete <- function(directory, id = 1:332) {

    specdata1 <- list.files(directory, full.names = TRUE)
    
    df1 <- data.frame()
    
    nobs <- data.frame()
    
    full <- data.frame()
    
    for (i in id) { 
        df1 <- (read.csv(specdata1[i],header=TRUE))
        nobs <- sum(complete.cases(df1))
        full <- rbind(full, data.frame(i,nobs))
    }
    
    colnames(full) <- c("id", "nobs")
    
    return(full)
}