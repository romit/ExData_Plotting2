## Question 3: Of the four types of sources indicated by the type (point, nonpoint, 
## onroad,nonroad) variable, which of these four sources have seen decreases 
## in emissions from 1999-2008 for Baltimore City? Which have seen increases 
## in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot 
## answer this question.

## Solution approach: Proceed as plot2.R except use ggplot and create a facet
## grid plot on type

## I will try to carry out the subset using dplyr 
## (http://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html)


## Code Starts!

# Check to see if the data is loaded, else load it
if (!"NEI" %in% ls()){
  NEI <- readRDS("summarySCC_PM25.rds")  
}

if (!"SCC" %in% ls()){
  SCC <- readRDS("Source_Classification_Code.rds")  
}

# Load the libraries
library(ggplot2)
library(dplyr)

# Use dplyr to transform NIE 
em.balt <- NEI %>%
filter(fips=="24510") %>%
group_by(year,type) %>%
summarize(total = sum(Emissions))

## Plot construction
g <- ggplot(em.balt, aes(x=year, y=total)) 
# Add line plot as well as highlight the data points
g <- g + geom_line() + geom_point(shape=16, size=4)
# Create a linear fit trendline, set confidence interval=false
g <- g + geom_smooth(aes(group=1), method="lm", se=FALSE, size = 1.5)
# Add the facet grid by type
g<- g + facet_grid( ~ type) 
# Add numeric labels
# g <- g + geom_text(aes(label=total), size=2, hjust=1.5, vjust=1.5)
# Add annotation across all facets about linear fit line
# g <- g + annotate("text", label = "Linear-fit trendline", 
#                  size = 4, x = 2004, y = 2000, col="blue")
g <- g + ylab(expression('Total PM'[2.5]*" Emissions")) +
  ggtitle("Total Emissions in Baltimore from 1999-2008 by Type (Blue lines are trendlines) ")

# Open PNG file and create dimensions with sufficient width 
png(filename = "plot3.png", 
    width = 700, height = 480)
# Print plot
print(g)

# Close device
dev.off()