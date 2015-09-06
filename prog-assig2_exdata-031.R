##
##  Program Assignment 2 - exdata 031
##

## PM2.5 Emissions Data (summarySCC_PM25.rds): 
## This file contains a data frame with all of the PM2.
## 5 emissions data for 1999, 2002, 2005, and 2008. 
## For each year, the table contains number of tons of PM2.5 
## emitted from a specific type of source for the entire year. 

## Source Classification Code Table (Source_Classification_Code.rds): 
## This table provides a mapping from the SCC digit strings in the 
## Emissions table to the actual name of the PM2.5 source. 
## The sources are categorized in a few different ways from more general 
## to more specific. 

setwd("~/prog_assig2_exdata-031")
library(stringr)
library(httr)
library(dplyr)
library(ggplot2)
library(jpeg)

## clean the work area
rm(list=ls())

## read data

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


## Address the following questions and tasks in your exploratory analysis. 

## 1) Have total emissions from PM2.5 decreased in the United States
##  from 1999 to 2008? 

q1 <- aggregate(Emissions ~ year,NEI, sum)

plot((q1/1000), type = "b",  xlab="Year",
        ylab="PM2.5 Emissions (1000 Tons)",
        main="Total PM2.5 Emissions From All US Sources")

dev.copy(png, file="plot1.png", width=480, height=480)
dev.off()
cat("Plot1.png saved in", getwd())



## 2) Have total emissions from PM2.5 decreased in the Baltimore City, 
## Maryland (fips == "24510") from 1999 to 2008? 

q2 <- subset(NEI, fips == 24510)
q2 <- aggregate(Emissions ~ year, q2, sum)

plot((q1/1000), type = "b",  xlab="Year",
     ylab="PM2.5 Emissions (1000 Tons)",
     main="Total PM2.5 Emissions - Baltimore City, Maryland") 

dev.copy(png, file="plot2.png", width=480, height=480)
dev.off()
cat("Plot2.png saved in", getwd())




## 3) Of the four types of sources indicated by the type (point,
## nonpoint, onroad, nonroad) variable, which of these four sources 
## have seen decreases in emissions from 1999-2008 for Baltimore City? 

q3 <- ddply(NEI[NEI$fips == "24510",], c("year", "type"), 
        function(df)sum(df$Emissions, na.rm=TRUE))
##
## Many thanks to Wayne E. Seguin (wayneeseguin) for this 
## so-prepared application of ggplot in these comparison charts
## 
ggplot(q3,aes(factor(year),V1,fill=type)) +
  geom_bar(stat="identity") +
  theme_bw() + guides(fill=FALSE)+
  facet_grid(.~type,scales = "free",space="free") + 
  labs(x="year", y=expression("Total PM2.5 Emission")) + 
  labs(title=expression("PM2.5 Emissions, Baltimore City by Source Type"))


dev.copy(png, file="plot3.png", width=480, height=480)
dev.off()
cat("Plot3.png saved in", getwd())




## 4) Which have seen increases in emissions from 1999-2008? 



## 5) Across the United States, how have emissions from coal 
## combustion-related sources changed from 1999-2008?

# find coal and combustion ids
idcomb   <- grepl("comb", SCC$SCC.Level.One, ignore.case=TRUE)
idcoal   <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE) 
combcoal <- (idcomb & idcoal)
combSCC  <- SCC[combcoal,]$SCC
q5       <- NEI[NEI$SCC %in% combSCC,]

q51      <- aggregate(Emissions ~ year,q5, sum)

plot(q51, type = "b", pch = 18, col = "blue", ylab = "Emissions", xlab = "Year", 
     main = "US Annual PM2.5 Emissions from coal combustion-related sources")

dev.copy(png, file="plot5.png", width=480, height=480)
dev.off()
cat("Plot5.png saved in", getwd())


## 6) How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

# find vehicle  ids
idvehic  <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
vehicSCC <- SCC[idvehic,]$SCC
vehicNEI  <- NEI[NEI$SCC %in% vehicSCC,]
q6 <- vehicNEI[vehicNEI$fips == 24510,]

q61 <- aggregate(Emissions ~ year,q6, sum)


plot(q61, type = "b", pch = 18, col = "red", ylab = "Emissions", 
     xlab = "Year", main = "Baltimore Annual PM2.5 Emissions from motor vehicle sources")

dev.copy(png, file="plot6.png", width=480, height=480)
dev.off()
cat("Plot6.png saved in", getwd())


## 7) Compare emissions from motor vehicle sources in Baltimore City with emissions from 
## motor vehicle sources in Los Angeles County, California (fips == "06037"). 

## use the last answer from BAltimore and find LA data
q6$city <- "Baltimore City"
q7      <- vehicNEI[vehicNEI$fips == "06037",]
q7$city <- "Los Angeles County"
q71     <- rbind(q6,q7)

##
## Many thanks to Wayne E. Seguin (wayneeseguin) for this 
## so-prepared application of ggplot in these comparison charts
##

ggplot(q71, aes(x=factor(year), y=Emissions, fill=city)) +
   geom_bar(aes(fill=year),stat="identity") +
   facet_grid(scales="free", space="free", .~city) +
   guides(fill=FALSE) + theme_bw() +
   labs(x="year", y=expression("Total PM2.5 Emission")) + 
   labs(title=expression("PM2.5 Motor Vehicle Source Emissions in Baltimore & Los Angeles"))

dev.copy(png, file="plot7.png", width=480, height=480)
dev.off()
cat("Plot1.png saved in", getwd())


## 8) Which city has seen greater changes over time in motor vehicle emissions?

