# GEOG 331 Activity 2
# Jahanvi Chamria
# 9/11/25

#make a vector of tree heights in meters
heights <- c(30,41,20,22)
#convert to cm
heights_cm <- heights*100
heights_cm

#look at the first tree height
heights[1]

#look at the 2nd and 3rd tree heights
heights[2:3]

#get more info on the matrix function
help(matrix)

#set up a matrix with 2 columns and fill in by rows
#first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

#set up a matrix that fills in by columns
#first argument is the vector of numbers to fill in the matrix
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol

#subset the matrix to look at row 1, column2
Mat.bycol[1,2]

#look at all values in row 1
Mat.bycol[1,]

#look at all values in column 2
Mat.bycol[,2]

#read in weather station file from the data folder
#datW <- read.csv("Z:\\jchamria\\data\\noaa_weather\\2011124.csv",
                 stringsAsFactors = T)
datW <- read.csv("C:\\Users\\jahan\\OneDrive\\Documents\\GitHub\\data\\2011124.csv",
                 stringsAsFactors = T)

#get more information about the dataframe
str(datW)

#specify a column with a proper date format
#note the format here dataframe$column
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")

#create a date column by reformatting the date to only include years
#and indicating that it should be treated as numeric data
datW$year <- as.numeric(format(datW$dateF,"%Y"))

#Question 2:
help(character)
help(numeric)
help(integer)
help(factor)

#create vectors of each type with 5 objects each
my_char <- c("Asia", "North America", "Europe", "Africa", "South America")
my_num <- c(c(3.14, 2.71, 1, 0.57, 6.28))
my_int <- c(1L, 2L, 3L, 4L, 5L)
my_fac <- factor(c("low", "medium", "high", "medium", "low"))

#find out all unique site names
unique(datW$NAME)

#look at the mean maximum temperature for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])

#look at the mean maximum temperature for Aberdeen
#with na.rm argument set to true to ingnore NA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#calculate the average daily temperature
#This temperature will be halfway between the minimum and maximum temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#get the mean across all sites
#the by function is a list of one or more variables to index over.
#FUN indicates the function we want to use
#if you want to specify any function specific arguments use a comma and add them after the function
#here we want to use the na.rm arguments specific to 
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#change the automatic output of column names to be more meaningful
#note that MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#convert level to number for factor data type
#you will have to reference the level output or look at the row of data to see the character designation.
datW$siteN <- as.numeric(datW$NAME)

#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#Question 3
help(hist)
help(paste)

#Question 4
par(mfrow=c(2,2))

#make a histogram for the first site in our levels, Aberdeen
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#Question 4
hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="lightcyan3",
     border="white")
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

hist(datW$TAVE[datW$siteN == 5],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[5]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="thistle3",
     border="white")
abline(v = mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="palegreen3",
     border="white")
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
#note I've named the histogram so I can reference it later
h1 <- hist(datW$TAVE[datW$siteN == 1],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[1]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")
#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x.plot <- seq(-10,30, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot is always the same between the two datasets on the plot. Here both plots share zero as a minimum.
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph  
#the first two arguements are the x coordinates and the y coordinates.

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

help(dnorm)

#pnorm(value to evaluate at (note this will evaluate for all values and below),mean, standard deviation)
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))- pnorm(0,
                                                        mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                                                        sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#pnorm of 20 gives me all probability (area of the curve) below 20 
#subtracting from one leaves me with the area above 20
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#pnorm of 20 gives me all probability (area of the curve) below 20 
#subtracting from one leaves me with the area above 20
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#Question 6
#current climate for Aberdeen 
current_mean <- mean(datW$TAVE[datW$siteN == 1], na.rm=TRUE)
current_sd <- sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE)

#climate change: mean increases by 4°C, SD stays same
future_mean <- current_mean + 4
future_sd <- current_sd

#current threshold for extreme high temperatures (95th percentile)
current_threshold <- qnorm(0.95, current_mean, current_sd)

#probability of exceeding current threshold under future climate
prob_exceed_threshold <- 1 - pnorm(current_threshold, future_mean, future_sd)

#results
round(current_mean, 2)
round(current_threshold, 2)
round(future_mean, 2)
round(prob_exceed_threshold * 100, 1)

#Question 7
#histogram of daily precipitation for Aberdeen 
hist(datW$PRCP[datW$siteN == 1], 
     freq = FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Daily Precipitation (mm)",
     ylab = "Relative Frequency",
     col = "grey50",
     border = "white",
     breaks = 50)  
abline(v = mean(datW$PRCP[datW$siteN == 1], na.rm=TRUE), 
       col = "red", lwd = 2, lty = 2)

#results
round(mean(datW$PRCP[datW$siteN == 1], na.rm=TRUE), 2)


#Question 8
#annual precipitation totals for each site and year
annual_precip <- aggregate(datW$PRCP, 
                           by = list(datW$NAME, datW$year), 
                           FUN = "sum", 
                           na.rm = TRUE)
colnames(annual_precip) <- c("NAME", "YEAR", "ANNUAL_PRCP")
#printing values for the first 2 years
print(head(annual_precip, 10))

#Aberdeen histogram
aberdeen_annual <- annual_precip[annual_precip$NAME == "ABERDEEN, WA US", ]
hist(aberdeen_annual$ANNUAL_PRCP,
     freq = FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Annual Precipitation (mm)",
     ylab = "Relative Frequency",
     col = "darkgreen",
     border = "white")
abline(v = mean(aberdeen_annual$ANNUAL_PRCP,na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
abline(v = mean(aberdeen_annual$ANNUAL_PRCP,na.rm=TRUE) - sd(aberdeen_annual$ANNUAL_PRCP,na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
abline(v = mean(aberdeen_annual$ANNUAL_PRCP,na.rm=TRUE) + sd(aberdeen_annual$ANNUAL_PRCP,na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#results
mean(aberdeen_annual$ANNUAL_PRCP, na.rm=TRUE)

#Question 9
#mean annual precipitation for each site
mean_annual_precip <- aggregate(annual_precip$ANNUAL_PRCP, 
                                by = list(annual_precip$NAME), 
                                FUN = "mean", 
                                na.rm = TRUE)
colnames(mean_annual_precip) <- c("NAME", "MEAN_ANNUAL_PRCP")
mean_annual_precip

#mean annual temperatures from earlier calculation
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean", na.rm=TRUE)
colnames(averageTemp) <- c("NAME","MAAT")

#combine temperature and precipitation data
climate_summary <- merge(averageTemp, mean_annual_precip, by = "NAME")

#results
climate_summary

