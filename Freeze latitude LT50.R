#analyze latitudinal data on LT50 of the core species
#Onnly have data for 2022 and not for 2023
#concern that LT50 values -11C are odd since -11 was our lowest temp
#therefore any LT50 value below them is just a best estimate between -11 and -40 and could be random
#meaning interpretation is not possible

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(multcomp)
library(lubridate)
library(readxl)
library(gridExtra)
library(MuMIn)
library(fitdistrplus)

#read in raw data for LT50 values
outputs<-read_excel("data/LT50 master.xlsx")

#create column for julian date
outputs$julian_date <- yday(outputs$Date)

#create column for month
outputs <- mutate(outputs, month=month(outputs$Date))

#create column for year
outputs <- mutate(outputs, year=year(outputs$Date))

#keep only data that is before/after average last freeze
#this value was determined from climate data by Evan at some point
#see climate station comparison script for these values
outputs<-outputs%>%
  drop_na(last_freeze)

#reorder before/after level
outputs$last_freeze<-factor(outputs$last_freeze,levels=c("before","after"))
#reorder state level from north to south
outputs$State<-factor(outputs$State,levels=c("IN","TN","AL"))
#make species a factor
outputs$Species<-as.factor(outputs$Species)
#plot LT50 before after for each species and location
#no clear pattern - but maybe before LT50 in TN is better than after
#in other states it looks like the reverse
#sugar maple may also have consistently worse LT50
#no clear state levels patterns either
ggplot(outputs,aes(x=Species,y=LT50,fill=last_freeze))+
  geom_boxplot()+
  facet_wrap(~State)

#Statistical testing to determine which factors may be important
#build global model with all 2-way interactions - avoid 3-way interaction since we don't have a large dataset
mod<-glm(LT50~(State+Species+last_freeze)^2,data=outputs,na.action="na.fail")
summary(mod)#looks like species differences and maybe state differences, but no before/after difference
#dredge this global model
dredge(mod)
#best model includes all terms and even some interactions
mod_best<-glm(LT50~State*last_freeze+Species,data=outputs)
summary(mod_best)#clear species level differences that need a post-hoc test, state also needs post-hoc
summary(glht(mod_best, linfct =  mcp(Species="Tukey")))#same pattern as in larger TN dataset, Acer is worse
summary(glht(mod_best, linfct =  mcp(State="Tukey")))
AIC(mod_best)

mod_simple<-glm(LT50~State+last_freeze+Species,data=outputs,na.action="na.fail")
summary(mod_simple)
AIC(mod_simple)

