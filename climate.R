## R code to manipulate and plot cliamte data for my four field sites ##
## aka another hot mess from Joe Endris ##

library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)

#read in Tennessee climate data
TN <- read_csv("data/Tennessee_climate.csv")

####################################
####Tennessee segment starts here###
####################################

str(TN)  #view structure of data ##

## create column for julian date##
TN$julian_date <- yday(TN$DATE)

#omit NA in temperature recordings 
TN<-TN[complete.cases(TN[,9]),]

## monthly mean low temp ##
## update this after creating julian dates ##
TN_TMAX <- TN %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(total = max(TMAX))

## create graph for temps by month of year ##

TN_TMAX %>%
  filter(year>1980) %>%
  ggplot(aes(x = year, y = total)) +
  geom_point(color = "grey") +
  geom_smooth(stat="smooth",method="lm")+
  labs(title = "Annual Highest Temperatures (°C)",
       subtitle = "Clarksville, TN",
       y= "Temperature °C",
       x= "Year") + theme_bw(base_size = 15)


#number of days above 32.2
TN_32.2 <- TN %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(n=sum(TMAX>32.2))

#plot number of days above 32.2
TN_32.2 %>%
  filter(as.integer(year)>1980)%>%
  filter(n>0)%>%
  ggplot(aes(x = year, y = n)) +
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Number of Days Above 32°C",
       subtitle = "Clarksville, TN",
       y= "Number of Days",
       x= "Year") + theme_bw(base_size = 15)


TN_32.2 %>%
  filter(n>0)%>%
  filter(year>1960)

##################################
####Alabama segment starts here###
##################################

str(AL)  #view structure of data ##

## create column for julian date##
## trying to replicate https://stackoverflow.com/questions/21414847/convert-a-date-vector-into-julian-day-in-r##
AL <- mutate(AL, Julian=format(DATE,"%j"))

#omit NA in temperature recordings 
AL<-AL[complete.cases(AL[,4]),]

## monthly mean low temp ##
## update this after creating julian dates ##
AL_TMAX <- AL %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(total = max(TMAX))

###max temp by month May-Sep##
AL_monthly_TMAX <- AL %>%


## create graph for temps by month of year ##
  
  AL_TMAX %>%
  filter(year>1950) %>%
  ggplot(aes(x = year, y = total)) +
  geom_point(color = "grey") +
  geom_smooth(stat="smooth",method="lm")+
  labs(title = "Annual Highest Temperatures (°C)",
       subtitle = "Tuscaloosa, AL",
       y= "Temperature °C",
       x= "Year") + theme_bw(base_size = 15)

#number of days above 32.2
AL_32.2 <- AL %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(n=sum(TMAX>32.2))

#plot number of days above 32.2
AL_32.2 %>%
  filter(as.integer(year)>1950)%>%
  filter(n>0)%>%
  ggplot(aes(x = year, y = n)) +
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Number of Days >32.2 (°C)",
       subtitle = "Tuscaloosa, AL",
       y= "Number of Days",
       x= "Year") + theme_bw(base_size = 15)


AL_32.2 %>%
  filter(n>0)%>%
  filter(year>1960)

##################################
####Indiana segment starts here###
##################################

str(IN)  #view structure of data ##

## create column for julian date##
## trying to replicate https://stackoverflow.com/questions/21414847/convert-a-date-vector-into-julian-day-in-r##
IN <- mutate(IN, Julian=format(DATE,"%j"))

#omit NA in temperature recordings 
IN<-IN[complete.cases(IN[,4]),]

## monthly mean low temp ##
## update this after creating julian dates ##
IN_TMAX <- IN %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(total = max(TMAX))

## create graph for temps by month of year ##

IN_TMAX %>%
  filter(year>1980) %>%
  ggplot(aes(x = year, y = total)) +
  geom_point(color = "grey") +
  geom_smooth(stat="smooth",method="lm")+
  labs(title = "Annual Highest Temperatures (°C)",
       subtitle = "Hoosier National Forest, IN",
       y= "Temperature °C",
       x= "Year") + theme_bw(base_size = 15)


#number of days above 32.2
IN_32.2 <- IN %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(n=sum(TMAX>32.2))

#plot number of days above 32.2
IN_32.2 %>%
  filter(as.integer(year)>1980)%>%
  filter(n>0)%>%
  ggplot(aes(x = year, y = n)) +
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Number of Days >32.2 (°C)",
       subtitle = "Hoosier National Forest, IN",
       y= "Number of Days",
       x= "Year") + theme_bw(base_size = 15)


IN_32.2 %>%
  filter(n>0)%>%
  filter(year>1960)

##################################
####Michigan segment starts here###
##################################

str(MI)  #view structure of data ##

## create column for julian date##
## trying to replicate https://stackoverflow.com/questions/21414847/convert-a-date-vector-into-julian-day-in-r##
MI <- mutate(MI, Julian=format(DATE,"%j"))

#omit NA in temperature recordings 
MI<-MI[complete.cases(MI[,4]),]

## monthly mean low temp ##
## update this after creating julian dates ##
MI_TMAX <- MI %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(total = max(TMAX))

## create graph for temps by month of year ##

MI_TMAX %>%
  filter(year>1980) %>%
  ggplot(aes(x = year, y = total)) +
  geom_point(color = "grey") +
  geom_smooth(stat="smooth",method="lm")+
  labs(title = "Annual Highest Temperatures (°C)",
       subtitle = "Chelsea, MI",
       y= "Temperature °C",
       x= "Year") + theme_bw(base_size = 15)


#number of days above 32.2
MI_32.2 <- MI %>%
  group_by(year=lubridate::floor_date(DATE, "year")) %>%
  summarise(n=sum(TMAX>32.2))

#plot number of days above 32.2
MI_32.2 %>%
  filter(as.integer(year)>1980)%>%
  filter(n>0)%>%
  ggplot(aes(x = year, y = n)) +
  geom_point(color = "grey") +
  geom_smooth(method="lm")+
  labs(title = "Number of Days >32.2 (°C)",
       subtitle = "Chelsea, MI",
       y= "Number of Days",
       x= "Year") + theme_bw(base_size = 15)


MI_32.2 %>%
  filter(n>0)%>%
  filter(year>1960)
