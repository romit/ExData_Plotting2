## Question 6: Compare emissions from motor vehicle sources in Baltimore City 
## with emissions from motor vehicle sources in Los Angeles County, California. 
## Which city has seen greater changes over time in motor vehicle emissions?

## Solution Approach: Use dplyr to first merge NEI with SCC 
## Then filter by Baltimore or LA and grep with "vehicle"
## Take the aggregate and plot as before
## Add a new variable called county to help in the plotting


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

# Transform the data using dplyr
emissions <- left_join(NEI, SCC, by='SCC') %>% # merge NEI and SCC
  filter(grepl('vehicle', EI.Sector, ignore.case=T)) %>% # Search for "vehicles"
  filter(fips == "06037" | fips == "24510") %>% # Extract only LA and BAltimore
  group_by(year, fips) %>% 
  summarize(tot=sum(Emissions)) %>% # Summarise
  mutate(county=factor(fips, labels=c('Los Angeles', 'Baltimore'))) # Create County


## Plot Construction
g <- ggplot(data=emissions, aes(x=year, y=tot)) 
# Add line plot and highlight points
g <- g + geom_line() + geom_point(shape=16, size=4)
g <- g + facet_grid(. ~ county)
# Create a trendline using linear fit
g <- g + geom_smooth(aes(group=1), method="lm", se=FALSE, size = 1.5)
# Add the text denoting trendline
#g <- g + annotate("text", x=2004, y=200, label="Linear-fit trendline", 
#                  col="Blue", size=4)
# Add labels and title
g <- g + ylab(expression('Total PM'[2.5]*" Emissions")) + xlab("Year") +
  ggtitle("Comparison between Total Emissions in Baltimore and Los Angeles 
          from 1999-2008 from Motor Vehicle Sources 
          (blue lines indicate linear-fit trendlines)")

# Open PNG file and create dimensions with sufficient width 
png(filename = "plot6.png", 
    width = 700, height = 480)
# Print plot
print(g)

# Close device
dev.off()