## R code to manipulate and plot climate data for my three field sites ##
## Code from Joe Joe Endris ##
#specifically focusing on cold temperatures and last freeze date needed for latitudinal testing of freeze tolerance

library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)

##temp<-read.csv("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/5- Climate/Climate Data/Alabama.csv")
setwd("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/5- Climate/")
#read in data
clim<-read_excel("climate.xlsx")
#limit data to that only since 1980
clim<-clim%>%
  filter(year(DATE)>1979)

## create objects from datasets##
AL <- clim%>%filter(STATION=="USC00018385")
TN <- clim%>%filter(STATION=="USC00401790")
IN <- clim%>%filter(STATION=="USC00120784")

##################################
####Alabama segment starts here###
##################################

str(AL)  #view structure of data ##

## create column for julian date##
AL <- mutate(AL, julian_date=format(DATE,"%j"))

#omit NA in temperature recordings 
AL<-AL%>%
  na.omit(TMIN)

#calculate last day below freezing for each year
AL_last <- AL%>%
  filter(TMIN<0)%>%
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

################################################################
################################################################
#code previously written by Evan
#investigate the date of the last frost in spring
#getting last day of last frost below 0, -2,-5 and -8
lowest<-subset(temp,temp=="TMIN")#create a dataframe of only minimum temperatures
spring<-lowest%>%#use only the minimum temperature data
  filter(month<=6)%>%#use only dates before July 1
  #filter(year>=1990)%>%#use only years after 1990
  filter(celcius<=-1)%>%#can change this to whatever temperature you like, currently I am taking only temperature less than 0C
  group_by(year,site)%>%#group within a year and by site so we get the latest temp per year
  slice(tail(row_number(),1))%>%#slicing out the last date when temperature is below 0C
  summarise(last_spring=last(celcius),
            month=month,#keeping the month variable in the new dataframe
            day=day,#keeping the day variable in the new dataframe
            DATE=DATE,#keeping the DATE variable in the new dataframe
            Julian=Julian)#keeping the Julian variable in the new dataframe

#Remove 2022 as data are incomplete
spring<-spring[which(spring$year!=2022),]

#plot last freezing date over time
ggplot(spring,aes(year,Julian,group=site,color=site))+
  geom_line()+
  theme_classic()+
  ylab("Julian date of last -1C temperature")

#Average date of last freezing date
spring%>%
  group_by(site)%>%
  summarise(ave=mean(Julian))

#Average date of last freezing date since 2000
spring%>%
  filter(year>1999)%>%
  group_by(site)%>%
  summarise(ave=mean(Julian))

ggplot(spring,aes(year,dayofyear))+
  geom_point()+
  geom_line()+
  geom_smooth(method=lm)

#calculate last spring frost as a moving average (5-year moving average)
move_ave<-spring%>%
  select(year,dayofyear)%>%
  mutate(srate=rollmean(dayofyear,k=5,fill=NA))#change k to determine interval
#remove 2021 as data are not complete
