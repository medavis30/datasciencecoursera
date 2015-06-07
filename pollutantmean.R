pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    specdata1 <- list.files(directory, full.names = TRUE)
    
    df1 <- data.frame()
    
    for (i in id) {
        df1 <- rbind(df1, read.csv(specdata1[i]))
    }
    
    mean(df1[ , pollutant], na.rm = TRUE)    
}