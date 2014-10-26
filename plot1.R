## Question1: Have total emissions from PM2.5 decreased in the United States 
## from 1999 to 2008? Using the base plotting system, make a plot showing the 
## total PM2.5 emission from all sources for each of the years 1999, 2002, 
## 2005, and 2008.

# Solution approach: Need to sum up all the emisssions for a particular year so
# that we have a dataframe of years vs sum of emissions. 

# Sums can be done using the aggregate function or maybe using the dplyr theme.
# I will try using the aggregate.


## Code starts!

# Check to see if the data is loaded, else load it
if (!"NEI" %in% ls()){
  NEI <- readRDS("summarySCC_PM25.rds")  
}

if (!"SCC" %in% ls()){
  SCC <- readRDS("Source_Classification_Code.rds")  
}

# Set margins and open the PNG file
par("mar"=c(5.1, 4.5, 4.1, 2.1))
png(filename = "plot1.png", 
    width = 480, height = 480)

# Aggregate by summing (could have used dplyr)
totalEmissions <- aggregate(NEI$Emissions, list(NEI$year), FUN = "sum")

# Set column names
names(totalEmissions) <- c("year","totem")

# Generate a trend line using the lm function to explicitly show trendline
trend <- lm(totalEmissions$totem ~ totalEmissions$year)

## Start the plot

plot(totalEmissions,
    type = "l",
    # Add Plot labels and text
    main = expression('US Total PM'[2.5]*' Emissions from 1999 to 2008'),
    xlab = "Year",
    ylab = expression('Total PM'[2.5]*" Emission")
)

# Add the points
points(totalEmissions, pch=16, cex=2)

# Draw trendline and colour it Blue
abline(trend, lwd=2, col='blue')

# Add a text to explain the trendline
text(2004, 6200000, labels='Linear-fit trendline', col='blue')

# Close device
dev.off()