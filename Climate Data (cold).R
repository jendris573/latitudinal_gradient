## R code to manipulate and plot climate data for my three field sites ##
## Code from Joe Joe Endris ##

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)

##temp<-read.csv("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/5- Climate/Climate Data/Alabama.csv")
setwd("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/5- Climate/")

## create objects from datasets##
AL <- read_csv("Alabama.csv")
TN <- read_csv("Tennessee.csv")
IN <- read_csv("Indiana.csv")

##################################
####Alabama segment starts here###
##################################

str(AL)  #view structure of data ##

## create column for julian date##
AL <- mutate(AL, julian_date=format(DATE,"%j"))

#omit NA in temperature recordings 
AL<-AL[complete.cases(AL[,5]),]

#calculate last day below freezing for each year
temp<-AL_last <- AL%>%
  filter(TMIN<0)%>%
  filter(year(DATE)>1979)%>%
  filter(julian_date<180)%>%
  group_by(year(DATE))%>%
  filter(row_number()==n())
mean(as.numeric(temp$julian_date))

## monthly absolute low temp ##
AL_TMIN <- AL %>%
  group_by(month=lubridate::floor_date(DATE, "month")) %>%
  summarise(temp = min(TMIN))

## create graph for temps by month of year ##
AL_TMIN_plot <-
  ggplot(AL_TMIN, aes(x = month, y = temp)) +
  geom_point(color = "black") +
  geom_smooth(stat="smooth",method="lm")+
  labs(title = "Annual Lowest Temperatures",
       subtitle = "Tuscaloosa, AL",
       y= "Temperature (Celcius)",
       x= "Year") + theme_bw(base_size = 15)

AL_TMIN_plot

#Number of Days Below -2
AL_freeze <- AL %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(n=sum(TMIN < 0))

#plot Number of Days Below zero
AL_freeze %>%
  filter(as.integer(year)>1980)%>%
  filter(n>0)%>%
  ggplot(aes(x = year, y = n)) +
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Number of Days Below Zero",
       subtitle = "Tuscaloosa, AL",
       y= "Number of Days",
       x= "Year") + theme_bw(base_size = 15)


AL_freeze %>%
  filter(n>0)%>%
  filter(year>1980)

#Calculate mean temperature by Julian date
AL_mean <- AL %>%
  group_by(julian_date) %>%
  summarise(mean_low = mean(TMIN))

AL_mean$julian_date = as.numeric(as.character(AL_mean$julian_date))

#plot mean temperature by julian date
AL_mean_plot <- ggplot(AL_mean, aes(x= julian_date, y=mean_low))+
                         xlim(1,135)+
                         geom_point(color = "grey") +
                         geom_smooth(method="lm")+
                         labs(title = "Mean Low Temperture by Julian Date",
                              subtitle = "Tuscaloosa, AL",
                              y= "Temperature (C)",
                              x= "Julian Date") + theme_bw(base_size = 15)

AL_mean_plot


## plot monthly mean low temps for Jan-May ##

AL_month_mean <- AL %>%
  group_by(month=lubridate::floor_date(DATE, "month")) %>%
  summarise(mean_low = mean(TMIN))

AL_month_mean$month2<-month(AL_month_mean$month)
sub<-AL_month_mean%>%
  filter(month2%in%c(1,2,3,4,5))

ggplot(sub,aes(x=month,y=mean_low))+
  geom_point()+
  facet_wrap(~month2,scales="free")+
  geom_smooth(method="lm")

AL_monthly_mean_plot <- ggplot(AL_month_mean, aes(x= month, y=mean_low))+
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Monthly Mean Low Temperture",
       subtitle = "Tuscaloosa, AL",
       y= "Temperature (C)",
       x= "Month") + theme_bw(base_size = 15)

AL_monthly_mean_plot







####################################
####Tennessee segment starts here###
####################################



##################################
####Indiana segment starts here###
##################################



#Issues to correct
#how to account for/correct leap days in Julian date count
#Line 32 - calculation for last freeze of the year
