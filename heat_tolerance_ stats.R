###Heat Tolerance Statistical Tests
###Written by Joe Endris

#Libraries
library(dplyr)
library(tidyr)
library(multcomp)
library(multcompView)
library(lubridate)
library(readxl)
library(xlsx)
library(gridExtra)
library(car)
library(stringr)

##################################
### Data entry and preparation ###
##################################

outputs <- read_excel("~/Documents/College/01- Data/crit_values_June 2023.xlsx")

#create column for julian date
outputs$julian_date <- yday(outputs$date)

#create column for month
outputs <- mutate(outputs, month=month(outputs$date))

#create column for year
outputs <- mutate(outputs, year=year(outputs$date))

#######################
###Statistical Tests###
#######################

june_mod1 <- glm(Tcrit.mn ~ year, data=june)

summary(june_mod1)

outputs$id <- as.factor(outputs$id)

june_mod2 <- glm(Tcrit.mn ~ id , data = outputs)
summary(june_mod2)

glht(june_mod2, mcp(id = "Tukey"))
summary(glht(june_mod2, mcp(id = "Tukey")))
