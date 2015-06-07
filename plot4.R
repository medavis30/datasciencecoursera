plot4 <- function(){
    
    ## Load data into R
    whole <- read.csv("household_power_consumption.txt", header = TRUE, sep = ';', na.strings = "?")
    
    ## Convert Date variable
    whole$Date <- as.Date(whole$Date, "%d/%m/%Y")
    
    ## Convert Time variable
    whole$Time <- strptime(paste(whole$Date, whole$Time), "%d/%m/%Y %H:%M:%S")
    
    ## Subset data for dates between 01/02/2007 and 02/02/2007
    myData <- subset(whole, subset = (Date >= "2007-02-01" & Date <= "2007-02-02"))
    
    ## Open PNG device
    png(file = "plot4.png", width = 480, height = 480)
    
    ## Set up space for four plots
    par(mfcol = c(2, 2))
    
    ## Set margins for optimal display
    par(mar = c(4,4,2,2))
    
    ## Create Plot 4.1
    with(myData, plot(Time, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power"))
    
    ## Create Plot 4.2
    with(myData, plot(Time, Sub_metering_1, type = "l", xlab = "", ylab = "Energy Sub metering"))
    
    with(myData, lines(Time, Sub_metering_2, col = "red"))
    
    with(myData, lines(Time, Sub_metering_3, col = "blue"))
    
    with(myData, legend("topright", lty = 1, bty = "n", xjust = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")))
    
    ## Create Plot 4.3
    with(myData, plot(Time, Voltage, type = "l", xlab = "datetime", ylab = "Voltage"))
    
    ## Create Plot 4.4
    with(myData, plot(Time, Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power"))
    
    ## Close the PNG device
    dev.off()
}