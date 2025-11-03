#Activity 5
#load in lubridate
library(lubridate)
library(ggplot2)

#read in streamflow data
datH <- read.csv("Z:\\jchamria\\data\\hw5_data\\stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)  

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("Z:\\jchamria\\data\\hw5_data\\2049867.csv")                            
head(datP)

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))          

#Question 2: leap_year function
leap_year(2016)  # TRUE: 2016 is a leap year
leap_year(2017)  # FALSE: 2017 is not a leap year

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#Question 3: no. of observations and frequency
#streamflow data
nrow(datH)  #total observations in original data
nrow(datD)  #observations after filtering for quality flag "A"
#precipitation data
nrow(datP)

#frequency of streamflow observations
#time differences between consecutive observations
head(datD[, c("date", "time")], 10)  #first 10 observations
#observations are every 15 minutes, except for the very first one 
#frequency of precipitation observations
head(datP[, c("DATE")], 20)  #first 20 observations
#observations are hourly, but with gaps (missing hours when no precipitation)

print(paste("Streamflow observations (original):", nrow(datH)))
print(paste("Streamflow observations (quality filtered):", nrow(datD)))
print(paste("Precipitation observations:", nrow(datP)))
print(paste("Streamflow frequency: 15-minute intervals"))
print(paste("Precipitation frequency: Hourly (with gaps for non-precipitation periods)"))


#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

#Question 5: 2017 observations
#subset data for 2017
data2017 <- datD[datD$year == 2017,]
#daily average for 2017
#ave2017 <- aggregate(data2017$discharge, by=list(data2017$doy), FUN="mean")
#colnames(ave2017) <- c("doy", "dailyAve2017")
#bigger margins
par(mai=c(1,1,1,1))
#make plot with adjusted y-axis limits to accommodate 2017 data
plot(aveF$doy, aveF$dailyAve, 
     type="l", 
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,140),  #increased upper limit for 2017 data
     xlim=c(1,365),  #extend x-axis to end of year
     xaxs="i", yaxs="i",
     axes=FALSE)
polygon(c(aveF$doy, rev(aveF$doy)), #x coordinates
        c(aveF$dailyAve-sdF$dailySD, rev(aveF$dailyAve+sdF$dailySD)), #ycoord
        col=rgb(0.392, 0.584, 0.929, .2), #color that is semi-transparent
        border=NA) #no border
#2017 line in red
lines(data2017$doy, data2017$discharge, col="red", lwd=1)
#lines(ave2017$doy, ave2017$dailyAve2017, col="red", lwd=1)
#x-axis with month labels at midpoints and extend the axis line
axis(1, at=c(15, 45, 75, 106, 136, 167, 197, 228, 259, 289, 320, 350),
     labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
     pos=0)  #position axis at y=0 
lines(c(1, 365), c(0, 0), lwd=1)
axis(2, seq(0,140, by=20),
     seq(0,140, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", 
       c("mean", "1 standard deviation", "2017"), #legend items
       lwd=c(2, NA, 2), #lines
       col=c("black", rgb(0.392, 0.584, 0.929, .2), "red"), #colors
       pch=c(NA, 15, NA), #symbols
       bty="n") #no legend border

#Question 7: days with complete precipitation data
#aggregate precipitation data to count observations per day
precip_counts <- aggregate(datP$HPCP, by=list(datP$year, datP$doy), FUN="length")
colnames(precip_counts) <- c("year", "doy", "n_obs")

#days with full 24 hours of data
precip_counts$complete <- precip_counts$n_obs == 24

#merge with discharge data to identify which discharge measurements have complete precip data
datD$complete_precip <- FALSE
for(i in 1:nrow(precip_counts)) {
  if(precip_counts$complete[i]) {
    datD$complete_precip[datD$year == precip_counts$year[i] & 
                           datD$doy == precip_counts$doy[i]] <- TRUE
  }
}
#plot all discharge measurements with different colors for complete/incomplete precip days
par(mai=c(1,1,1,1))
plot(datD$decYear[!datD$complete_precip], datD$discharge[!datD$complete_precip],
     col="gray", pch=16, cex=0.3,
     xlab="Year",
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     main="Streamflow with Days Having Complete Precipitation Data")
points(datD$decYear[datD$complete_precip], datD$discharge[datD$complete_precip],
       col="blue", pch=16, cex=0.3)
legend("topright",
       c("Incomplete precip data", "Complete precip data (24 hrs)"),
       col=c("gray", "blue"),
       pch=16,
       bty="n")

#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

min(hydroD$discharge)
#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

#Question 8: winter hydrograph
#add month to help identify winter days
datP$month <- month(ymd_hm(datP$DATE))

#create month column in precip_counts by matching year and doy
precip_counts$month <- NA
for(i in 1:nrow(precip_counts)) {
  matching_rows <- datP[datP$year == precip_counts$year[i] & 
                          datP$doy == precip_counts$doy[i], ]
  #get the month from first matching row
  if(nrow(matching_rows) > 0) {
    precip_counts$month[i] <- matching_rows$month[1]
  }
}

#filter for winter days (December = 12, January = 1, February = 2)
winter_complete <- precip_counts[precip_counts$complete & 
                                   (precip_counts$month == 12 | 
                                      precip_counts$month == 1 | 
                                      precip_counts$month == 2), ]
print(winter_complete)

#pick one day from the list 
chosen_winter_day <- winter_complete[3, ]
print(chosen_winter_day)

#subset discharge data for chosen winter period (2 days)
hydroD_winter <- datD[datD$doy >= chosen_winter_day$doy & 
                        datD$doy < (chosen_winter_day$doy + 2) & 
                        datD$year == chosen_winter_day$year, ]

#subset precipitation data for chosen winter period
hydroP_winter <- datP[datP$doy >= chosen_winter_day$doy & 
                        datP$doy < (chosen_winter_day$doy + 2) & 
                        datP$year == chosen_winter_day$year, ]

#y-axis limits for discharge
yl_winter <- floor(min(hydroD_winter$discharge)) - 1
yh_winter <- ceiling(max(hydroD_winter$discharge)) + 1

#limits for precipitation
pl_winter <- 0
pm_winter <- ceiling(max(hydroP_winter$HPCP)) + 0.5

#scale precipitation to fit on discharge plot
hydroP_winter$pscale <- (((yh_winter - yl_winter) / (pm_winter - pl_winter)) * 
                           hydroP_winter$HPCP) + yl_winter

#winter hydrograph plot
par(mai=c(1,1,1,1))
plot(hydroD_winter$decDay,
     hydroD_winter$discharge, 
     type="l", 
     ylim=c(yl_winter, yh_winter), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     main=paste("Winter Hydrograph: Day", chosen_winter_day$doy, 
                "Year", chosen_winter_day$year))
#add bars to indicate precipitation
for(i in 1:nrow(hydroP_winter)){
  polygon(c(hydroP_winter$decDay[i] - 0.017, hydroP_winter$decDay[i] - 0.017,
            hydroP_winter$decDay[i] + 0.017, hydroP_winter$decDay[i] + 0.017),
          c(yl_winter, hydroP_winter$pscale[i], hydroP_winter$pscale[i], yl_winter),
          col=rgb(0.392, 0.584, 0.929, .2), 
          border=NA)
}
legend("topright",
       c("Discharge", "Precipitation"),
       lwd=c(2, NA),
       col=c("black", rgb(0.392, 0.584, 0.929, .2)),
       pch=c(NA, 15),
       bty="n")


#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()

#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin()

#Question 9: violin plots by season for 2016 and 2017
#subset data for 2016 and 2017 only
dat_2016_2017 <- datD[datD$year == 2016 | datD$year == 2017, ]

#get month from the date
dates_2016_2017 <- as.Date(dat_2016_2017$date, "%m/%d/%Y")
dat_2016_2017$month <- month(dates_2016_2017)

#season variable based on calendar definitions
#winter: December (12), January (1), February (2)
#spring: March (3), April (4), May (5)
#summer: June (6), July (7), August (8)
#fall: September (9), October (10), November (11)

dat_2016_2017$season <- NA

#assign seasons
dat_2016_2017$season[dat_2016_2017$month == 12 | 
                       dat_2016_2017$month == 1 | 
                       dat_2016_2017$month == 2] <- "Winter"

dat_2016_2017$season[dat_2016_2017$month == 3 | 
                       dat_2016_2017$month == 4 | 
                       dat_2016_2017$month == 5] <- "Spring"

dat_2016_2017$season[dat_2016_2017$month == 6 | 
                       dat_2016_2017$month == 7 | 
                       dat_2016_2017$month == 8] <- "Summer"

dat_2016_2017$season[dat_2016_2017$month == 9 | 
                       dat_2016_2017$month == 10 | 
                       dat_2016_2017$month == 11] <- "Fall"

#check 
table(dat_2016_2017$season, dat_2016_2017$year)

#make season a factor with specific order so they plot in order
dat_2016_2017$season <- factor(dat_2016_2017$season, 
                               levels = c("Winter", "Spring", "Summer", "Fall"))

#subset data for each year separately
dat_2016 <- dat_2016_2017[dat_2016_2017$year == 2016, ]
dat_2017 <- dat_2016_2017[dat_2016_2017$year == 2017, ]

#violin plot for 2016
ggplot(data = dat_2016, aes(x = season, y = discharge)) + 
  geom_violin(fill = "lightblue", color = "black") +
  labs(title = "Seasonal Streamflow Discharge: 2016",
       x = "Season",
       y = expression(paste("Discharge (ft"^"3", " sec"^"-1", ")"))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

#violin plot for 2017
ggplot(data = dat_2017, aes(x = season, y = discharge)) + 
  geom_violin(fill = "lightcoral", color = "black") +
  labs(title = "Seasonal Streamflow Discharge: 2017",
       x = "Season",
       y = expression(paste("Discharge (ft"^"3", " sec"^"-1", ")"))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))
