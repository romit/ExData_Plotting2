## Question 5: How have emissions from motor vehicle sources 
## changed from 1999-2008 in Baltimore City?

## Solution Approach: Use dplyr to first merge NEI with SCC 
## Then filter by Baltimore grep with "vehicle"
## Take the aggregate and plot as before


## Code Starts!
# Check to see if the data is loaded, else load it
if (!"NEI" %in% ls()){
  NEI <- readRDS("summarySCC_PM25.rds")  
}

if (!"SCC" %in% ls()){
  SCC <- readRDS("Source_Classification_Code.rds")  
}

# Load ggplot and dplyr
library(ggplot2)
library(dplyr)


# Chose to use dplyr since the transformation got tricky!
emissions <- left_join(NEI, SCC, by='SCC') %>% # Merging the two dataframes
  # filter by the string "vehicle" and by Baltimore
  filter(grepl('vehicle', EI.Sector, ignore.case=T)  & fips == "24510") %>% 
  group_by(year) %>% 
  summarize(tot=sum(Emissions)) # Aggregate as total sum




## Plot Construction
g <- ggplot(data=emissions, aes(x=year, y=tot)) 
# Add line plot and highlight points
g <- g + geom_line() + geom_point(shape=16, size=4)
# Create a trendline using linear fit
g <- g + geom_smooth(aes(group=1), method="lm", se=FALSE, size = 1.5)
# Add the text denoting trendline
g <- g + annotate("text", x=2004, y=200, label="Linear-fit trendline", 
                  col="Blue", size=4)
# Add labels and title
g <- g + ylab(expression('Total PM'[2.5]*" Emissions")) + xlab("Year") +
ggtitle("Total Emissions in Baltimore from 1999-2008 from Motor Vehicle Sources ")

# Open PNG file and create dimensions with sufficient width 
png(filename = "plot5.png", 
    width = 700, height = 480)
# Print plot
print(g)

# Close device
dev.off()