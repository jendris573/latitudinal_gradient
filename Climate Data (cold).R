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
library(xlsx)

##temp<-read.csv("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/5- Climate/Climate Data/Alabama.csv")
setwd("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/5- Climate/")
#read in data
clim<-read.csv("data/spring_climate.csv")
#turn date from character to date
clim$DATE<-mdy(clim$DATE)

#limit data to that only since 1980
clim<-clim%>%
  filter(year(DATE)>1979)
#add column of julian date
clim <- mutate(clim, julian_date=format(DATE,"%j"))
#remove date before May 31
clim<-clim%>%
  filter(julian_date<152)
clim$julian_date<-as.numeric(clim$julian_date)
#remove missing data
clim<-clim%>%
  na.omit(TMIN)
## create objects from datasets##
AL <- clim%>%filter(STATION=="USC00018385")#alabama data is a mess
TN <- clim%>%filter(STATION=="USC00401790")
IN <- clim%>%filter(STATION=="USC00120784")

#read in summary LT50 data
outputs_sum<-read.xlsx("data/LT50_summary.xlsx",sheetIndex=1)
#start with Indinia since Alabama climate data has holes and lots of repeats
##################################
####Indiana segment starts here###
#-5.6C is the worst cold tolerance any species had in IN#
#individuals were sampled on julian date 95 (before) and 116 (after)
##################################

str(IN)  #view structure of data ##

#calculate last day below freezing for each year
IN_last <- IN%>%
  filter(TMIN<0)%>%
  group_by(year(DATE))%>%
  filter(row_number()==n())
mean(as.numeric(IN_last$julian_date))
colnames(IN_last)[9]<-'year'
#plot it
ggplot(IN_last,aes(year,julian_date))+
  geom_point()+
  geom_smooth(stat="smooth",method="lm")

#coldest temperature per julian date
IN_TMIN_day <- IN %>%
  group_by(julian_date) %>%
  summarise(temp = min(TMIN))
#plot it
ggplot()+
  geom_point(data=IN_TMIN_day,aes(x=julian_date,y=temp))+
  geom_line(data=IN_TMIN_day,aes(x=julian_date,y=temp))+
  geom_point(data=outputs_sum%>%filter(State=="IN"),
                         aes(x=julian_date,y=mean,shape=factor(Species)),size=3,colour="red")+
  geom_errorbar(data=outputs_sum%>%filter(State=="IN"),aes(ymin=mean-se,ymax=mean+se,x=julian_date),width=2)
  
#now just for 2022
#plot it
ggplot()+
  geom_point(data=IN%>%filter(year(DATE)==2022),aes(julian_date,TMIN))+
  geom_line(data=IN%>%filter(year(DATE)==2022),aes(julian_date,TMIN))+
  geom_point(data=outputs_sum%>%filter(State=="IN"),
             aes(x=julian_date,y=mean,shape=factor(Species)),size=3,colour="red")+
  geom_errorbar(data=outputs_sum%>%filter(State=="IN"),aes(ymin=mean-se,ymax=mean+se,x=julian_date),width=2)

## monthly absolute low temp ##
IN_TMIN <- IN %>%
  group_by(month=lubridate::floor_date(DATE, "month")) %>%
  summarise(temp = min(TMIN))

## create graph for temps by month of year ##
IN_TMIN_plot <-
  ggplot(IN_TMIN, aes(x = month, y = temp)) +
  geom_point(color = "black") +
  geom_smooth(stat="smooth",method="lm")+
  labs(title = "Annual Lowest Temperatures",
       subtitle = "Bloomington, IN",
       y= "Temperature (Celcius)",
       x= "Year") + theme_bw(base_size = 15)
IN_TMIN_plot

#Number of Days Below -2
IN_freeze <- IN %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(n=sum(TMIN < 0))

#plot Number of Days Below zero
IN_freeze %>%
  filter(as.integer(year)>1980)%>%
  filter(n>0)%>%
  ggplot(aes(x = year, y = n)) +
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Number of Days Below Zero",
       subtitle = "Bloomington, IN",
       y= "Number of Days",
       x= "Year") + theme_bw(base_size = 15)

IN_freeze %>%
  filter(n>0)%>%
  filter(year>1980)

#Calculate mean temperature by Julian date
IN_mean <- IN %>%
  group_by(julian_date) %>%
  summarise(mean_low = mean(TMIN))

#plot mean temperature by julian date
IN_mean_plot <- ggplot(IN_mean, aes(x= julian_date, y=mean_low))+
  xlim(1,135)+
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Mean Low Temperture by Julian Date",
       subtitle = "Bloomington, IN",
       y= "Temperature (C)",
       x= "Julian Date") + theme_bw(base_size = 15)

IN_mean_plot

## plot monthly mean low temps for Jan-May ##

IN_month_mean <- IN %>%
  group_by(month=lubridate::floor_date(DATE, "month")) %>%
  summarise(mean_low = mean(TMIN))

IN_month_mean$month2<-month(IN_month_mean$month)
sub<-IN_month_mean%>%
  filter(month2%in%c(1,2,3,4,5))

ggplot(sub,aes(x=month,y=mean_low))+
  geom_point()+
  facet_wrap(~month2,scales="free")+
  geom_smooth(method="lm")

IN_monthly_mean_plot <- ggplot(IN_month_mean, aes(x= month, y=mean_low))+
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Monthly Mean Low Temperture",
       subtitle = "Bloomington, IN",
       y= "Temperature (C)",
       x= "Month") + theme_bw(base_size = 15)

IN_monthly_mean_plot

###############################################
#Evan stopped here as latitudinal data doesn't seem to produce many interesting results
###############################################

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
mean(as.numeric(AL_last$julian_date))

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
