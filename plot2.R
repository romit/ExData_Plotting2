## Question 2: Have total emissions from PM2.5 decreased in the Baltimore City, 
## Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system 
## to make a plot answering this question.

## Solution approach: follow the same approach as plot 1 except make sure that 
## NEI is subsetted with only Baltimore readings. 

## Will do a subset of NEI before carrting out the aggregate function like I did
## in plot1.R


## Code Starts!

# Check to see if the data is loaded, else load it
if (!"NEI" %in% ls()){
  NEI <- readRDS("summarySCC_PM25.rds")  
}

if (!"SCC" %in% ls()){
  SCC <- readRDS("Source_Classification_Code.rds")  
}

# Set margins and open the PNG file
par("mar"=c(5.1, 4.5, 4.1, 2.1))
png(filename = "plot2.png", 
    width = 480, height = 480)

# Subset NEI for only Baltimore
NEI.baltimore <- subset(NEI, NEI$fips=="24510")

# Aggregate by summing (could have used dplyr)
totalEmissions.baltimore <- aggregate(NEI.baltimore$Emissions, 
                                      list(NEI.baltimore$year), FUN = "sum")

# Set column names
names(totalEmissions.baltimore) <- c("year","totem")

# Generate a trend line using the lm function to explicitly show trendline
trend.baltimore <- lm(totalEmissions.baltimore$totem ~ 
                        totalEmissions.baltimore$year)

## Start the plot

plot(totalEmissions.baltimore,
     type = "l",
     # Add Plot labels and 
     main = expression('Baltimore Total PM'[2.5]*' Emissions from 1999 to 2008'),
     xlab = "Year",
     ylab = expression('Total PM'[2.5]*" Emission")
)

# Add points
points(totalEmissions.baltimore, pch=16, cex=2)

# Add the trendline and colour it blue
abline(trend.baltimore, lwd=2, col='blue')

# Add a text to explain the trendline
text(2002, 3050, labels='Linear-fit trendline', col='blue')


dev.off()