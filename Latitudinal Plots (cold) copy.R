### Code for Latitudinal gradient plots
### written by Joe Endris

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(multcomp)
library(multcompView)
library(lubridate)
library(readxl)
library(gridExtra)

outputs<-read_excel("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/1- Freezing/Data/LT50 master.xlsx")


######################################################
###plot with LT values grouped by species and state###
######################################################

options(dplyr.summarise.inform = FALSE)

locations <- outputs%>%
  group_by(Species, State)%>%
  dplyr::summarise(across(LT15:LT95,list(mean=~mean(.),sd=~sd(.),se=~sd(./sqrt(12)))))

locations2 <- locations%>%
  pivot_longer(locations, cols=starts_with("LT"),
               names_to = "dmg_threshold",
               values_to = "values")

g1<-ggplot(locations, aes(x = State, y=LT15_mean, group=Species,color=Species)) +
  geom_point(position=position_dodge(0.5))+
  geom_errorbar(aes(ymax=LT15_mean+LT15_se,ymin=LT15_mean-LT15_se),position=position_dodge(0.5))+
  ylab("Temperature (°C)")+
  ylim(-35,5) +
  theme_bw()+
  theme(legend.position="none")+
  theme(legend.position=c(0.25,0.25))+
  ggtitle("Mean LT15 by State")

g2<-ggplot(locations, aes(x = State, y=LT50_mean, group=Species,color=Species)) +
  geom_point(position=position_dodge(0.5))+
  geom_errorbar(aes(ymax=LT50_mean+LT50_se,ymin=LT50_mean-LT50_se),position=position_dodge(0.5))+
  ylab("Temperature (°C)")+
  ylim(-35,5) +
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Mean LT50 by State")

g3<- ggplot(locations, aes(x = State, y=LT95_mean, group=Species,color=Species)) +
  geom_point(position=position_dodge(0.5))+
  geom_errorbar(aes(ymax=LT95_mean+LT95_se,ymin=LT95_mean-LT95_se),position=position_dodge(0.5))+
  ylab("Temperature (°C)")+
  ylim(-35,5)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Mean LT95 by State")

grid.arrange(g1,g2,g3,nrow=1)

###########################################
##Before and After Mean last freeze plots##
###########################################

#omit any blank spots in the last_freeze column
outputs_LF <- outputs[complete.cases(outputs[,8]),]

outputs_LF <- outputs_LF%>%
  group_by(last_freeze, State, Species)%>%
  dplyr::summarise(across(LT15:LT95,list(mean=~mean(.),sd=~sd(.),se=~sd(./sqrt(6)))))

##Grouped by location##
BA_loc_plot <- ggplot(outputs_LF, aes(x=State, y=LT50_mean, color=Species, last_freeze)) +
  geom_point(position=position_dodge(0.5))+
  geom_errorbar(aes(ymax=LT50_mean+LT50_se,ymin=LT50_mean-LT50_se), position=position_dodge(0.5))+
  xlab ("Location") +
  ylab ("Temperature (°C)")+
  theme_bw()

outputs_LF$Last_freeze<-factor(outputs_LF$last_freeze,levels=c("Before","After"))

BA_species_plot <- ggplot(outputs_LF, aes(x=Species, y=LT50_mean, color=last_freeze))+
  geom_point(position=position_dodge(0.5))+
  geom_errorbar(aes(ymax=LT50_mean+LT50_se,ymin=LT50_mean-LT50_se), position=position_dodge(0.5))+
  xlab ("Species") +
  ylab ("Temperature (°C)")+
  facet_wrap(~State)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1))

BA_loc_plot
BA_species_plot
grid.arrange(BA_loc_plot,BA_species_plot,nrow=1)
