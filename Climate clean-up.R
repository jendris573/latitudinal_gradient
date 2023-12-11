library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(prism)

# # # # # # # # # #
## NOAA Data ----
# # # # # # # # # #

#read in Alabama climate data
AL <- read_csv("data/Alabama.csv")

sum(!complete.cases(AL$TMAX))

sum(!complete.cases(AL$TMIN))

#read in Tennessee climate data
TN <- read_csv("data/Tennessee.csv")

sum(!complete.cases(TN$TMAX))

sum(!complete.cases(TN$TMIN))

#read in Indiana climate data
IN <- read_csv("data/Indiana.csv")

sum(!complete.cases(IN$TMAX))

sum(!complete.cases(IN$TMIN))

#read in Michigan climate data
MI <- read_csv("data/Michigan.csv")

sum(!complete.cases(MI$TMAX))

sum(!complete.cases(MI$TMIN))

#read in summer climate data
summer <- read_csv("data/summer_climate.csv")

sum(!complete.cases(summer$TMAX))

sum(!complete.cases(summer$TMIN))



