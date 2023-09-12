###Leaf Temp  Plots###
###Code written by Joe Endris###

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(multcomp)
library(multcompView)
library(lubridate)
library(readxl)
library(gridExtra)
library(MuMIn)
library(reshape2)
library(gridGraphics)

##################################
### Data entry and preparation ###
##################################

leaf_temps <- read_excel("~/Documents/College/02- R code/heating/leaf_temperatures.xlsx")

#create column for julian date
leaf_temps$julian_date <- yday(leaf_temps$date)

#determine mean leaf temp
mean_temps <- leaf_temps%>%
  group_by(species,date)%>%
  dplyr::summarise(leaf_temp_mean= mean(temp))

#Load NOAA Climate Data Online data
climate <- read.csv("~/Documents/College/02- R code/heating/Tennessee_climate.csv")

#keep only sewage plant
climate <- climate%>%filter(NAME=="CLARKSVILLE SEWAGE PLANT, TN US")

#omit NA in temperature recordings 
climate<-climate[complete.cases(climate[,8]),]

#create column for year
climate <- mutate(climate, year=year(climate$DATE))

#create column for month
climate <- mutate(climate, month=month(climate$DATE))

## create column for julian date##
climate$julian_date <- yday(climate$DATE)

#filter dates to study period
climate <- filter(climate, DATE >= "2022-01-01")

#####################################################
### Plot to compare highest leaf temp to air temp ###
#####################################################

#Acer saccharum
maple <- filter(mean_temps, species == 'Acer saccharum')

maple_plot <- ggplot(maple, aes(x=date, y=leaf_temp_mean))+
  geom_point()+
  geom_line(data=climate, aes(x=DATE, y=TMAX, color="grey"))+
  xlab("Date")+
  ylab("Temperature")+
  theme_bw()

maple_plot
