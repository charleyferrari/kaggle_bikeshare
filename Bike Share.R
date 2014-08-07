
library(ggplot2)
#library(xts)

#library(TTR)
#library(forecast)
library(reshape2)

setwd("C:/Users/Charley/Downloads/Courses/Bike Share Kaggle")
dummysource <- "Train.csv"

variables <- read.csv(dummysource, header = TRUE)


variables$datetime <- strptime(variables$datetime, format = "%Y-%m-%d %H:%M:%S", tz="")

variables$Year <- factor(variables$datetime$year + 1900)
variables$Month <- factor(variables$datetime$mon + 1)
variables$Day <- factor(variables$datetime$mday)
variables$Hour <- factor(variables$datetime$hour)

dayvariables <- aggregate(count ~ season + holiday + workingday + Day + Month + Year, data=variables, FUN="mean")

dayvariables$datetime <- strptime(paste(dayvariables$Year, dayvariables$Month, dayvariables$Day, sep=":"),
                                   format = "%Y:%m:%d", tz="")

monthvariables <- aggregate(count ~ Month + Year, data=variables, FUN="mean")
monthvariables$registered <- aggregate(registered ~ Month + Year, data=variables, FUN="mean")$registered
monthvariables$casual <- aggregate(casual ~ Month + Year, data=variables, FUN="mean")$casual

monthvariables$datetime <- strptime(paste(monthvariables$Year, monthvariables$Month, "1", sep=":"),
                                    format = "%Y:%m:%d", tz="")


#Exploratory Data Analysis

Months <- c("January", "February", "March", "April", "May", "June",
            "July", "August", "September", "October", "November", "December")

monthvariablesstacked <- monthvariables[,(!(colnames(monthvariables) == "Month") &
                                    !(colnames(monthvariables) == "Year") & 
                                    !(colnames(monthvariables) == "count"))]
monthvariablesstacked$datetime <- as.character(monthvariablesstacked$datetime)


monthvariablesstacked <- melt(monthvariablesstacked, id.vars="datetime", variable_name="RiderType", value.name="count")

monthvariablesstacked$datetime <- strptime(monthvariablesstacked$datetime, format = "%Y-%m-%d", tz="")

monthvariablesstacked$RiderType <- factor(monthvariablesstacked$RiderType)

PDFPath <- "BikeShareGraphs.pdf"
pdf(file=PDFPath)

################
# Over Time
################

ggplot(dayvariables, aes(x=datetime, y=count)) +
  geom_line() + 
  ggtitle("Average Rides Per Hour Daily")

ggplot(monthvariables, aes(x=datetime, y=count)) + 
  geom_line() + 
  ggtitle("Average Rides Per Hour Monthly")

ggplot(monthvariablesstacked, aes(x=datetime, y=count, fill=RiderType)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Blues", breaks=rev(levels(monthvariablesstacked$RiderType))) + 
  ggtitle("Average Rides Per Hour Monthly: Registered vs Casual")

############################
#for loop test
#m<- 4
#y <- 2011
#
#columntitle <- "count"
#
#newvariables <- variables[,(!(colnames(variables) == "count") &
#                              !(colnames(variables) == "registered") & 
#                              !(colnames(variables) == "casual"))]
#newvariables[,"usedcount"] <- variables[,"count"]
#
############################

for(columntitle in c("count", "registered", "casual")){
  newvariables <- variables[,(!(colnames(variables) == "count") &
                                !(colnames(variables) == "registered") & 
                                !(colnames(variables) == "casual"))]
  newvariables[,"usedcount"] <- variables[,columntitle]
  

################
# Weekends & Holidays
################


  for(y in 2011:2012){
    for(m in 1:12){
      slice <- newvariables[(newvariables$Year == y & newvariables$Month == m),]
      if(NROW(slice[slice$holiday == 1,]) == 0){
        print(ggplot(slice,aes(x=datetime, y=usedcount)) +
                geom_rect(data = subset(slice, workingday == 0), aes(ymin = -Inf, ymax = Inf, xmin = datetime - 1000, xmax = datetime + 1000),
                          alpha = 1, colour = "gray") + 
                #geom_rect(data = subset(slice, holiday == 1), aes(ymin = -Inf, ymax = Inf, xmin = datetime - 1000, xmax = datetime + 1000),
                #          alpha = 1, colour = "yellow") +
                geom_rect(data = subset(slice, Hour == 0), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                          alpha = 1, colour = "white") + 
                geom_line() + 
                ggtitle(paste("Weekends and Holidays", columntitle, Months[m], y, sep=" ")) + 
                coord_fixed(ratio=1000))
      }else{
        print(ggplot(slice,aes(x=datetime, y=usedcount)) +
                geom_rect(data = subset(slice, workingday == 0), aes(ymin = -Inf, ymax = Inf, xmin = datetime - 1000, xmax = datetime + 1000),
                          alpha = 1, colour = "gray") + 
                geom_rect(data = subset(slice, holiday == 1), aes(ymin = -Inf, ymax = Inf, xmin = datetime - 1000, xmax = datetime + 1000),
                          alpha = 1, colour = "yellow") +
                geom_rect(data = subset(slice, Hour == 0), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                          alpha = 1, colour = "white") + 
                geom_line() + 
                ggtitle(paste("Weekends and Holidays", columntitle, Months[m], y, sep=" ")) + 
                coord_fixed(ratio=1000))
      }
    }
  }

################
# Weather
# My original plan with weather was to
# draw rectangles for the weather. This
# ended up looking too busy. 
################



  for(y in 2011:2012){
    for(m in 1:12){
      slice <- newvariables[(newvariables$Year == y & newvariables$Month == m),]
      if(NROW(slice[slice$holiday == 1,]) == 0){      
        #slice$weather = factor(slice$weather, levels=c(1,2,3,4))
        print(ggplot(slice,aes(x=datetime, y=usedcount, colour=weather)) + 
                #geom_rect(data = subset(slice, weather == 1), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                #          alpha = 1, colour = "gray") + 
                #geom_rect(data = subset(slice, weather == 2), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                #          alpha = 1, colour = "green") + 
                #geom_rect(data = subset(slice, weather == 3), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                #          alpha = 1, colour = "yellow") + 
                #geom_rect(data = subset(slice, weather == 4), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                #          alpha = 1, colour = "red") + 
                geom_rect(data = subset(slice, Hour == 0), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                          alpha = 1, colour = "white") + 
                geom_rect(data = subset(slice, Hour == 0), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                          alpha = 1, colour = "white") + 
                geom_rect(data = subset(slice, workingday == 0), aes(ymin = -Inf, ymax = 25, xmin = datetime - 1000, xmax = datetime + 1000),
                          alpha = 1, colour = "gray") +
                #geom_rect(data = subset(slice, holiday == 1), aes(ymin = -Inf, ymax = 25, xmin = datetime - 1000, xmax = datetime + 1000),
                #          alpha = 1, colour = "yellow") +
                geom_line() + 
                ggtitle(paste("Weather", columntitle, Months[m], y, sep=" ")) + 
                scale_colour_gradient(limits=c(1, 4)) + 
                #scale_colour_manual(values = cols) + 
                coord_fixed(ratio=1000))
      }else{
        print(ggplot(slice,aes(x=datetime, y=usedcount, colour=weather)) + 
                #geom_rect(data = subset(slice, weather == 1), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                #          alpha = 1, colour = "gray") + 
                #geom_rect(data = subset(slice, weather == 2), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                #          alpha = 1, colour = "green") + 
                #geom_rect(data = subset(slice, weather == 3), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                #          alpha = 1, colour = "yellow") + 
                #geom_rect(data = subset(slice, weather == 4), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                #          alpha = 1, colour = "red") + 
                geom_rect(data = subset(slice, Hour == 0), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                          alpha = 1, colour = "white") + 
                geom_rect(data = subset(slice, Hour == 0), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                          alpha = 1, colour = "white") + 
                geom_rect(data = subset(slice, workingday == 0), aes(ymin = -Inf, ymax = 25, xmin = datetime - 1000, xmax = datetime + 1000),
                          alpha = 1, colour = "gray") +
                geom_rect(data = subset(slice, holiday == 1), aes(ymin = -Inf, ymax = 25, xmin = datetime - 1000, xmax = datetime + 1000),
                          alpha = 1, colour = "yellow") +
                geom_line() + 
                ggtitle(paste("Weather", columntitle, Months[m], y, sep=" ")) + 
                scale_colour_gradient(limits=c(1, 4)) + 
                #scale_colour_manual(values = cols) + 
                coord_fixed(ratio=1000))
      }
    }
  }

################
# Temp
################

  #cols <- c("1" = "red","2" = "blue","3" = "darkgreen", "4" = "orange")
  
  for(y in 2011:2012){
    for(m in 1:12){
      slice <- newvariables[(newvariables$Year == y & newvariables$Month == m),]
      if(NROW(slice[slice$holiday == 1,]) == 0){
        print(ggplot(slice,aes(x=datetime, y=usedcount, colour=temp)) + 
                geom_rect(data = subset(slice, Hour == 0), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                          alpha = 1, colour = "white") + 
                geom_rect(data = subset(slice, Hour == 0), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                          alpha = 1, colour = "white") + 
                geom_rect(data = subset(slice, workingday == 0), aes(ymin = -Inf, ymax = 25, xmin = datetime - 1000, xmax = datetime + 1000),
                          alpha = 1, colour = "gray") +
                #geom_rect(data = subset(slice, holiday == 1), aes(ymin = -Inf, ymax = 25, xmin = datetime - 1000, xmax = datetime + 1000),
                #          alpha = 1, colour = "yellow") +
                geom_line() + 
                ggtitle(paste("Temp", columntitle, Months[m], y, sep=" ")) + 
                scale_colour_gradient(limits=c(min(newvariables$temp), max(newvariables$temp)), low="blue", high="red") + 
                coord_fixed(ratio=1000))
      }else{
        print(ggplot(slice,aes(x=datetime, y=usedcount, colour=temp)) + 
                geom_rect(data = subset(slice, Hour == 0), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                          alpha = 1, colour = "white") + 
                geom_rect(data = subset(slice, Hour == 0), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                          alpha = 1, colour = "white") + 
                geom_rect(data = subset(slice, workingday == 0), aes(ymin = -Inf, ymax = 25, xmin = datetime - 1000, xmax = datetime + 1000),
                          alpha = 1, colour = "gray") +
                geom_rect(data = subset(slice, holiday == 1), aes(ymin = -Inf, ymax = 25, xmin = datetime - 1000, xmax = datetime + 1000),
                          alpha = 1, colour = "yellow") +
                geom_line() + 
                ggtitle(paste("Temp", columntitle, Months[m], y, sep=" ")) + 
                scale_colour_gradient(limits=c(min(newvariables$temp), max(newvariables$temp)), low="blue", high="red") + 
                coord_fixed(ratio=1000))
      }
    }
  }

################
# ATemp
################

  #cols <- c("1" = "red","2" = "blue","3" = "darkgreen", "4" = "orange")
  
  for(y in 2011:2012){
    for(m in 1:12){
      slice <- newvariables[(newvariables$Year == y & newvariables$Month == m),]
      if(NROW(slice[slice$holiday == 1,]) == 0){
        print(ggplot(slice,aes(x=datetime, y=usedcount, colour=atemp)) + 
                geom_rect(data = subset(slice, Hour == 0), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                          alpha = 1, colour = "white") + 
                geom_rect(data = subset(slice, Hour == 0), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                          alpha = 1, colour = "white") + 
                geom_rect(data = subset(slice, workingday == 0), aes(ymin = -Inf, ymax = 25, xmin = datetime - 1000, xmax = datetime + 1000),
                          alpha = 1, colour = "gray") +
                #geom_rect(data = subset(slice, holiday == 1), aes(ymin = -Inf, ymax = 25, xmin = datetime - 1000, xmax = datetime + 1000),
                #          alpha = 1, colour = "yellow") +
                geom_line() + 
                ggtitle(paste("ATemp", columntitle, Months[m], y, sep=" ")) + 
                scale_colour_gradient(limits=c(min(newvariables$atemp), max(newvariables$atemp)), low="blue", high="red") + 
                coord_fixed(ratio=1000))
      }else{
        print(ggplot(slice,aes(x=datetime, y=usedcount, colour=atemp)) + 
                geom_rect(data = subset(slice, Hour == 0), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                          alpha = 1, colour = "white") + 
                geom_rect(data = subset(slice, Hour == 0), aes(ymin = -Inf, ymax = Inf, xmin = datetime, xmax = datetime),
                          alpha = 1, colour = "white") + 
                geom_rect(data = subset(slice, workingday == 0), aes(ymin = -Inf, ymax = 25, xmin = datetime - 1000, xmax = datetime + 1000),
                          alpha = 1, colour = "gray") +
                geom_rect(data = subset(slice, holiday == 1), aes(ymin = -Inf, ymax = 25, xmin = datetime - 1000, xmax = datetime + 1000),
                          alpha = 1, colour = "yellow") +
                geom_line() + 
                ggtitle(paste("ATemp", columntitle, Months[m], y, sep=" ")) + 
                scale_colour_gradient(limits=c(min(newvariables$atemp), max(newvariables$atemp)), low="blue", high="red") + 
                coord_fixed(ratio=1000))
      }
    }
  }

}
dev.off()


