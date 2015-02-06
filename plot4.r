####################################################################################################
#
#       Exploratory Data Analysis / Johns Hopkins University / Coursera / Course Project #1
#
#                                           PLOT #4
#
#The code below was created for this assignment. Save the file into your working directory and
#source it into your R workspace. Follow the directions to run the program and generate the plot.
#This script is comprised of two primary blocks: a preliminary chunk that fetches, unzips, and 
#cleans the code, and the chunk that generates a plot to match the one specified by the assignment.
#
####################################################################################################



#Instructions printed for the user
print("Create a PNG file of plot #4 by calling the function 'getPlot4()'")

#Function for creating the plot
getPlot4 <- function() {
    
    
    ## Preliminary Chunk ##
    
    #Create the project directory if it doesn't exist, download/unzip the data file if it doesn't exist, 
    #read the data into R and assign it to the rawData variable.
    if (!file.exists("ExpData_A1_Plots")) {
        dir.create("ExpData_A1_Plots")
    }
    if (!file.exists("ExpData_A1_Plots/household_power_consumption.txt")) {
        fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        pkgTest <- function (x) {   #Package check and install function
            if (!require(x, character.only = TRUE)) {
                install.packages(x, dep=TRUE)
            }
        }
        pkgTest("downloader") #For download of HTTPS link on Windows
        library(downloader)
        download(fileURL, "ExpData_A1_Plots/rawdata.zip", mode="wb")
        unzip("ExpData_A1_Plots/rawdata.zip", exdir="ExpData_A1_Plots")
        unlink("ExpData_A1_Plots/rawdata.zip")
    }
    rawData <- read.table("ExpData_A1_Plots/household_power_consumption.txt", sep=";", header=T, 
                        na.strings="?", nrows=2075259, check.names=F, stringsAsFactors=F, 
                        comment.char="", quote='\"')
    #Clean up the raw data by converting the Date values into the as.Date format, subsetting only the 
    #two days in question, and appending a Datetime column to the DF.
    rawData$Date <- as.Date(rawData$Date, format="%d/%m/%Y")
    plotData <- subset(rawData, subset=(Date == "2007-02-01" | Date == "2007-02-02"))
    rm(rawData)
    colDatetime <- paste(as.Date(plotData$Date), plotData$Time)
    plotData$Datetime <- as.POSIXct(colDatetime)
    
    
    ## Plotting Chunk - Plot #4 ##
    
    #Overlay 3 line charts showing "Energy sub metering" levels from Thu-Sat for Sub_metering 1-3. 
    #Color the lines and add a legend as shown.
    
    #Build 4 charts in a 2x2 configuration. Topleft is plot 2 and bottomleft is plot 3, both of which 
    #have minor changes to them. Topright is a line graph with x="datetime" and y="Voltage", and the
    #bottomright is a line graph of x="datetime" and y="Global_relative_power".
    
    png("ExpData_A1_Plots/plot4.png", width=480, height=480, bg="transparent")
    par(mfrow=c(2,2))
    with(plotData, {
        plot(Global_active_power~Datetime, 
             type="l",
             ylab="Global Active Power",
             xlab=""
        )
        plot(Voltage~Datetime,
             type="l",
             ylab="Voltage",
             xlab="datetime"
        )
        plot(Sub_metering_1~Datetime, 
             type="l",
             ylab="Energy sub metering",
             xlab="",
        )
        lines(Sub_metering_2~Datetime, col="red")
        lines(Sub_metering_3~Datetime, col="blue")
        legend("topright", 
               legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
               col=c("black", "red", "blue"),
               lwd=1,
               bty="n",
        )
        plot(Global_reactive_power~Datetime,
             type="l",
             ylab="Global_reactive_power",
             xlab="datetime",
             
        )
    })
    dev.off()
}