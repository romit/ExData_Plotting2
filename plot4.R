## Question 4: Across the United States, how have emissions 
## from coal combustion-related sources changed from 1999-2008?

## Solution approach: Take a subset of SCC by adding rows with 
## the term "coal" in it. Use the grepl function for this. 
## Then correlate NEI with SCC by merging by SCC.
## Finally, like before, sum up each year and then plot


## Code Starts!

# Check to see if the data is loaded, else load it
if (!"NEI" %in% ls()){
  NEI <- readRDS("summarySCC_PM25.rds")  
}

if (!"SCC" %in% ls()){
  SCC <- readRDS("Source_Classification_Code.rds")  
}

# Load ggplot
library(ggplot2)

# Take a subset of SCC by searching through Short.Name for "coal"
SCC.coal = SCC[grepl("coal", SCC$Short.Name, ignore.case=TRUE),]

# Merge the two dataframes 
SCCmerge <- merge(x=NEI, y=SCC.coal, by='SCC')

# Take the aggregate by summing
SCCmerge.agg <- aggregate(SCCmerge[, 'Emissions'], 
                          by=list(SCCmerge$year), FUN="sum")

# Set column names
names(SCCmerge.agg) <- c('Year', 'Emissions')

## Plot construction
g <- ggplot(data=SCCmerge.agg, aes(x=Year, y=Emissions)) 
# Add a line plot and highlight the data points
g <- g + geom_line() + geom_point(shape=16, size=4) 
# Create a linear fit trendline
g <- g + geom_smooth(aes(group=1), method="lm", se=FALSE, size = 1.5) 
# Add the text denoting trendline
g <- g + annotate("text", x=2004, y=550000, label="Linear-fit trendline", 
            col="Blue", size=4)
# Add labels and title
g <- g + ylab(expression('Total PM'[2.5]*" Emissions")) +
  ggtitle("Total Emissions in US from 1999-2008 from Coal Combustion Sources ")

# Open PNG file and create dimensions with sufficient width 
png(filename = "plot4.png", 
    width = 700, height = 480)
# Print plot
print(g)

# Close device
dev.off()

