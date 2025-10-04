#Activity 3
#Jahanvi Chamria
#9/23/25

#create a function. The names of the arguments for your function will be in parentheses. Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}

#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")

#evaluate a true statement
assert(2 == 2, "error: unequal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")

#read in the data file
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
datW <- read.csv("Z:\\jchamria\\data\\bewkes\\bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#preview data
print(datW[1,])

#get sensor info from file
# this data table will contain all relevant units
sensorInfo <-   read.csv("Z:\\jchamria\\data\\bewkes\\bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

print(sensorInfo)

#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])

#use install.packages to install lubridate
#install.packages(c("lubridate"))
#it is helpful to comment this line after you run this line of code on the computer
#and the package installs. You really don't want to do this over and over again.

library(lubridate)

#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calculations
datW[1,]

#see how many values have missing data for each sensor observation
#air temperature
length(which(is.na(datW$air.temperature)))

#wind speed
length(which(is.na(datW$wind.speed)))

#precipitation
length(which(is.na(datW$precipitation)))

#soil temperature
length(which(is.na(datW$soil.moisture)))

#soil moisture
length(which(is.na(datW$soil.temp)))

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

#I'm going to make a new column to work with that indicates that I am conducting QAQC
#because overwriting values should be done cautiously and can lead to confusing issues.
#It can be particularly confusing when you are just learning R.
#Here I'm using the ifelse function
#the first argument is a logical statement to be evaluated as true or false on a vector
#the second argument is the value that my air.tempQ1 column will be given if the statement
#is true. The last value is the value that will be given to air.tempQ1 if the statement is false.
#In this case it is just given the air temperature value
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

#check the values at the extreme range of the data
#and throughout the percentiles
quantile(datW$air.tempQ1)

#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]  

#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]  

#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

#question 5 (testing that lightscale can be used to subset datW)
#test that lightscale has the same length as datW using assert from part 1
assert(length(lightscale) == nrow(datW), 
       "error: lightscale and datW have different lengths")
#if the above test passes (no error message), then lightscale can be used to subset datW
#because they have matching dimensions

#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

#question 6 (filtering suspect measurements from wind speed data)
#create a test to check that the right conditions were filtered
#test 1: Check that values with precipitation >= 2 AND lightning > 0 were converted to NA
storm_conditions <- datW$precipitation >= 2 & datW$lightning.acvitivy > 0
assert(sum(is.na(datW$wind.speedQ1[storm_conditions])) == sum(storm_conditions), 
       "error: not all storm conditions (precip >= 2 & lightning) were filtered")
#test 2: Check that values with precipitation > 5 were converted to NA
heavy_rain <- datW$precipitation > 5
assert(sum(is.na(datW$wind.speedQ1[heavy_rain])) == sum(heavy_rain),
       "error: not all heavy rain conditions (precip > 5) were filtered")
#test 3: Check that values NOT meeting these conditions kept their original values
safe_conditions <- !(storm_conditions | heavy_rain)
assert(sum(datW$wind.speedQ1[safe_conditions] == datW$wind.speed[safe_conditions], na.rm=TRUE) == sum(safe_conditions),
       "error: some safe condition values were incorrectly changed")

#plot wind speed with QA/QC filtering applied
plot(datW$DD, datW$wind.speedQ1, pch=19, type="b", 
     xlab="Day of Year", 
     ylab="Wind Speed (m/s)",
     main="Wind Speed with Storm Conditions Removed")

#question 7 (check soil sensor reliability before outage)

#find the last day with soil data
last_day <- max(datW$DD[!is.na(datW$soil.temp)])

#look at last 5 days before it stopped
last_5_days <- datW[datW$DD > (last_day - 5), ]

#plot soil temp and air temp together
plot(last_5_days$DD, last_5_days$soil.temp, 
     type="b", pch=19, col="brown",
     xlab="Day of Year", 
     ylab="Temperature (C)",
     main="Temperature Before Sensor Stopped")
lines(last_5_days$DD, last_5_days$air.temperature, 
      type="b", pch=19, col="red")
legend("topleft", 
       legend=c("Soil Temp", "Air Temp"), 
       col=c("brown", "red"), 
       pch=19)

#plot soil moisture
plot(last_5_days$DD, last_5_days$soil.moisture,
     type="b", pch=19, col="blue",
     xlab="Day of Year", 
     ylab="Soil Moisture",
     main="Soil Moisture Before Sensor Stopped")

#question 8 (calculate statistics)

#count total observations after QA/QC
total_obs <- nrow(datW)

avg_air_temp <- mean(datW$air.tempQ2, na.rm = TRUE)
avg_wind_speed <- mean(datW$wind.speedQ1, na.rm = TRUE)
avg_soil_moisture <- mean(datW$soil.moisture, na.rm = TRUE)
avg_soil_temp <- mean(datW$soil.temp, na.rm = TRUE)
total_precip <- sum(datW$precipitation, na.rm = TRUE)

#count observations that went into each calculation
obs_air_temp <- sum(!is.na(datW$air.tempQ2))
obs_wind_speed <- sum(!is.na(datW$wind.speedQ1))
obs_soil_moisture <- sum(!is.na(datW$soil.moisture))
obs_soil_temp <- sum(!is.na(datW$soil.temp))
obs_precip <- sum(!is.na(datW$precipitation))

#time period
start_date <- datW$timestamp[1]
end_date <- datW$timestamp[nrow(datW)]

#round to appropriate decimal places based on sensor accuracy
print(paste("Study Period:", start_date, "to", end_date))
print(paste("Total Observations:", total_obs))
print(paste("Average Air Temperature:", round(avg_air_temp, 1), "°C"))
print(paste("Observations used:", obs_air_temp))
print(paste("Average Wind Speed:", round(avg_wind_speed, 1), "m/s"))
print(paste("Observations used:", obs_wind_speed))
print(paste("Average Soil Moisture:", round(avg_soil_moisture, 2), "m³/m³"))
print(paste("Observations used:", obs_soil_moisture))
print(paste("Average Soil Temperature:", round(avg_soil_temp, 1), "°C"))
print(paste("Observations used:", obs_soil_temp))
print(paste("Total Precipitation:", round(total_precip, 1), "mm"))
print(paste("Observations used:", obs_precip))

#question 9 (plot data)

#find the x-axis range for all plots
x_min <- min(datW$DD, na.rm = TRUE)
x_max <- max(datW$DD, na.rm = TRUE)

par(mfrow = c(2, 2))

#plot 1: Soil Moisture
plot(datW$DD, datW$soil.moisture, 
     type = "b", pch = 19, col = "blue",
     xlab = "Day of Year", 
     ylab = "Soil Moisture (m³/m³)",
     main = "Soil Moisture",
     xlim = c(x_min, x_max))

#plot 2: Air Temperature 
plot(datW$DD, datW$air.tempQ2, 
     type = "b", pch = 19, col = "red",
     xlab = "Day of Year", 
     ylab = "Air Temperature (°C)",
     main = "Air Temperature",
     xlim = c(x_min, x_max))

#plot 3: Soil Temperature
plot(datW$DD, datW$soil.temp, 
     type = "b", pch = 19, col = "brown",
     xlab = "Day of Year", 
     ylab = "Soil Temperature (°C)",
     main = "Soil Temperature",
     xlim = c(x_min, x_max))

#plot 4: Precipitation
plot(datW$DD, datW$precipitation, 
     type = "h", lwd = 2, col = "darkblue",
     xlab = "Day of Year", 
     ylab = "Precipitation (mm)",
     main = "Precipitation",
     xlim = c(x_min, x_max))
