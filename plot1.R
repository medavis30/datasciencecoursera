plot1 <- function(){
    
    ## Load data into R
    whole <- read.csv("household_power_consumption.txt", header = TRUE, sep = ';', na.strings = "?")
    
    ## Convert Date variable
    whole$Date <- as.Date(whole$Date, "%d/%m/%Y")
    
    ## Convert Time variable
    whole$Time <- strptime(paste(whole$Date, whole$Time), "%d/%m/%Y %H:%M:%S")
    
    ## Subset data for dates between 01/02/2007 and 02/02/2007
    myData <- subset(whole, subset = (Date >= "2007-02-01" & Date <= "2007-02-02"))
    
    ## Open PNG device
    png(file = "plot1.png", width = 480, height = 480)
    
    ## Create Plot 1
    with(myData,hist(Global_active_power, main = "Global Active Power", col = "red", xlab = "Global Active Power (kilowatts)"))

    ## Close the PNG device
    dev.off()
}