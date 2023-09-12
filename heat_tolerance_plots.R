###Heating Tolerance Plots###
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

outputs <- read_excel("~/Documents/College/01- Data/crit_values_final.xlsx")

#For Evan's readin
#outputs <- read_excel("C:/R/heat_tolerance/crit_values_final.xlsx")

#filter just TN data
outputs<-outputs[which(outputs$state=="TN"),]

#create column for julian date
outputs$julian_date <- yday(outputs$date)

#create column for month
outputs <- mutate(outputs, month=month(outputs$date))

#create column for year
outputs <- mutate(outputs, year=year(outputs$date))

##############################
###Individual Species Plots### 
##############################

#filter for only June/July data for comparisons
species_outputs <- 

#Acer saccharum
maple <- filter(outputs, id == 'Acer saccharum')

maple_plot <- ggplot(maple, aes(x=date, y=Tcrit.mn))+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci))+
  xlab("Date")+
  ylab("Critical Temperature")+
  theme_bw()+
  facet_wrap(~id)

maple_plot

#Liriodendron tulipifera
poplar <- filter(outputs, id == 'Liriodendron tulipifera')

poplar_plot <- ggplot(poplar, aes(x=date, y=Tcrit.mn))+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci))+
  xlab("Date")+
  ylab("Critical Temperature")+
  theme_bw()+
  facet_wrap(~id)

poplar_plot

#Fagus grandifolia
beech <- filter(outputs, id == 'Fagus grandifolia')

beech_plot <- ggplot(beech, aes(x=date, y=Tcrit.mn))+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci))+
  xlab("Date")+
  ylab("Critical Temperature")+
  theme_bw()+
  facet_wrap(~id)

beech_plot

grid.arrange(maple_plot, poplar_plot, beech_plot,nrow=3)

########################################
### 2022 vs 2023 comparison by month ###
########################################

###june to june comparison

june <- filter(outputs, month == 6)
class(outputs$month)

june_plot <- ggplot(june, aes(x=year, y=Tcrit.mn))+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci))+
  xlab("Year")+
  ylab("Critical Temperature")+
  theme_bw()+
  facet_wrap(~id)

june_plot

july <- filter(outputs, month == 7)
class(outputs$month)

july_plot <- ggplot(june, aes(x=year, y=Tcrit.mn))+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci))+
  xlab("Year")+
  ylab("Critical Temperature")+
  theme_bw()+
  facet_wrap(~id)

july_plot

#######################
###Full species plot###
#######################

#This summary below takes a mean of confidence intervals created for each study period
#You wouldn't want to do that. We would either need to recalculate the thermal tolerance
#for each species while ignoring sample period (Which I don't recommend)
#or plot the Tcrit and confidence interval for each sample period separately
outputs_species <- outputs%>%
  group_by(id)%>%
  dplyr::summarise(across(Tcrit.lci:T95.uci, list(mean=~mean(.))))

str(outputs_species)

outputs_species$id <- as.factor(outputs_species$id)
outputs$id <- as.factor(outputs$id)
class(outputs_species$id)

full_plot <- ggplot(outputs_species, aes(y= Tcrit.mn_mean, x= reorder(id, Tcrit.mn_mean, decreasing = TRUE))) +
  coord_flip()+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci_mean,ymin=Tcrit.lci_mean))+
 # geom_point(x= outputs_species$T50.mn_mean, color = "blue")+
 # geom_point(x= outputs_species$T95.mn_mean, color = "black")+
  ylab("Critical Temperature")+
  xlab("Species")+
  ylim(30, 55)+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+

full_plot

#Evan's version to plot LT50 by each sample period
full_plot <- ggplot(outputs, aes(y= Tcrit.mn, x= reorder(id, Tcrit.mn, decreasing = TRUE))) +
  coord_flip()+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci))+
  # geom_point(x= outputs_species$T50.mn_mean, color = "blue")+
  # geom_point(x= outputs_species$T95.mn_mean, color = "black")+
  ylab("Critical Temperature")+
  xlab("Species")+
  ylim(30, 55)+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  facet_wrap(~month+year)#since facet_wrap - the species will stay in the same order regardless of
#changing LT values. Therefore it is probably necessary to make a figure for each sample period
full_plot

#June 2022
June2022 <- ggplot(outputs%>%
                      filter(year==2022,month==6), aes(y= Tcrit.mn, x= reorder(id, Tcrit.mn, decreasing = TRUE))) +
  coord_flip()+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci))+
  # geom_point(x= outputs_species$T50.mn_mean, color = "blue")+
  # geom_point(x= outputs_species$T95.mn_mean, color = "black")+
  ylab("Critical Temperature")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("June 2022")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
June2022

July2022 <- ggplot(outputs%>%
                     filter(year==2022,month==7), aes(y= Tcrit.mn, x= reorder(id, Tcrit.mn, decreasing = TRUE))) +
  coord_flip()+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci))+
  # geom_point(x= outputs_species$T50.mn_mean, color = "blue")+
  # geom_point(x= outputs_species$T95.mn_mean, color = "black")+
  ylab("Critical Temperature")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("July 2022")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
July2022

June2023 <- ggplot(outputs%>%
                     filter(year==2023,month==6), aes(y= Tcrit.mn, x= reorder(id, Tcrit.mn, decreasing = TRUE))) +
  coord_flip()+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci))+
  # geom_point(x= outputs_species$T50.mn_mean, color = "blue")+
  # geom_point(x= outputs_species$T95.mn_mean, color = "black")+
  ylab("Critical Temperature")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("June 2023")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
June2023

July2023 <- ggplot(outputs%>%
                     filter(year==2023,month==7), aes(y= Tcrit.mn, x= reorder(id, Tcrit.mn, decreasing = TRUE))) +
  coord_flip()+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci))+
  # geom_point(x= outputs_species$T50.mn_mean, color = "blue")+
  # geom_point(x= outputs_species$T95.mn_mean, color = "black")+
  ylab("Critical Temperature")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("July 2023")+
  theme_bw()+
  theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
July2023

#look at variation in species by year

july_variation <- outputs %>%
  filter(month==7)

july_var <- ggplot(july_variation, aes(y= Tcrit.mn, x= id, color=year)) +
  coord_flip()+
  geom_point()+
  geom_errorbar(aes(ymax=Tcrit.uci,ymin=Tcrit.lci))+
  ylab("Critical Temperature")+
  xlab("Species")+
  ylim(30, 55)+
  ggtitle("July")+
  theme_bw()+
  #theme(legend.position="none")+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
july_var

grid.arrange(June2022,June2023,July2022,July2023,ncol=2)
grid.arrange(July2022,July2023,ncol=2)
