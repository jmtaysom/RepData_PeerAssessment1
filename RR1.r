#########################################
# PM2_5analysis.R
# Created by jmtaysom
# R version: 3.1.3
# 2015Mar17
# System: Mac OS 10.6.8
#
#
#########################################

## Import libraries
library(plyr)
library(ggplot2)

## Download, unzip and cleanup
setwd("~/datasciencecoursera/Explore/Project2")
zip <- 'pmdata.zip'
url = 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
download.file(url, zip, method='curl')
unzip(zip)
file.remove(zip)
rm(list=ls())

## Read in the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## summarise the data to find the total emissions (in tons) per year
vehiclesSCC <- SCC[grepl("Vehicle", SCC$SCC.Level.Two), ]
vehicles <- NEI[NEI$SCC %in% vehiclesSCC$SCC,] 
compare_vehicles <- vehicles[vehicles$fips == '24510' | vehicles$fips == '06037',]
totals <- ddply(compare_vehicles,.(year, fips), summarise, Emissions=sum(Emissions))
## change the fips codes to City Names
totals$fips[totals$fips=='24510'] <- 'Baltimore'
totals$fips[totals$fips=='06037'] <- 'Los Angeles'
colnames(totals)[which(names(totals) == 'fips')] <- 'City'

## Plot the emissions over time and add a linear regression line
png(file="plot6.png",width=480,height=480)
g <- ggplot(totals, aes(year, Emissions, color=City)) 
g + geom_point() + geom_smooth(method='lm', se=FALSE) + 
  labs(title='PM 2.5 Emissions from Vehicles',
       y='Emission in tons', x="Year") +
  scale_x_continuous(breaks=c(1999,2002,2005,2008)) +
  theme(legend.justification=c(1,0), legend.position=c(1,0.4))

dev.off()
