####################################################################################################
#
#       Exploratory Data Analysis / Johns Hopkins University / Coursera / Course Project #1
#
#                                           PLOT #1
#
#The code below was created for this assignment. Save the file into your working directory and
#source it into your R workspace. Follow the directions to run the program and generate the plot.
#This script is comprised of two primary blocks: a preliminary chunk that fetches, unzips, and 
#cleans the code, and the chunk that generates a plot to match the one specified by the assignment.
#
####################################################################################################



#Instructions printed for the user
print("Create a PNG file of plot #1 by calling the function 'getPlot1()'")

#Function for creating the plot
getPlot1 <- function() {

    
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
    
    
    ## Plotting Chunk - Plot #1 ##
    
    #Create a histogram with x = Global Active Power, y = Frequency, with bar widths = 0.5 kilowatts. 
    #Change bar color to red, add the title and labels as shown, and create a png file of the plot.
    png("ExpData_A1_Plots/plot1.png", width=480, height=480, bg="transparent")
    hist(plotData$Global_active_power, 
         freq = T, 
         col = "red",
         main = "Global Active Power", 
         xlab = "Global Active Power (kilowatts)", 
         ylab = "Frequency")
    dev.off()
}


