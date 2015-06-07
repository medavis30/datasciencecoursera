corr <- function(directory, threshold = 0) {
    
    specdata1 <- list.files(directory, full.names = TRUE)
    
    full <- complete(directory)
    
    casesmet <- full[full$nobs > threshold, 1]
    
    corrs <- vector()
    
    for (i in casesmet) {
        df1 <- (read.csv(specdata1[i],header=TRUE))
        full <- complete.cases(df1)
        sulfate1 <- df1[full, 2]
        nitrate1 <- df1[full, 3]
        corrs[i] <- cor(sulfate1,nitrate1)
    }
    
    corrs <- corrs[complete.cases(corrs)]
}