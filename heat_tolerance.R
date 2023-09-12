###Heating Tolerance Threshold Analysis###
###Code written by Joe Endris###
###Function written by Evan Rehm

#Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
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

#read in data
heating_data<-read_excel("~/Library/CloudStorage/GoogleDrive-jendris@my.apsu.edu/.shortcut-targets-by-id/1p5eHgH8eX9-QjkyyA3uRz5Lk7ontMZtO/Rehm lab - General/Trees/2- Heating/Data/Leaf Heating.xlsx")

#look at data structure
str(heating_data)

#create column for year
heating_data <- mutate(heating_data, year=year(heating_data$date))

#create column for month
heating_data <- mutate(heating_data, month=month(heating_data$date))

## create column for Julian date##
heating_data$julian_date <- yday(heating_data$date)

#create column with unique ID
heating_data <- mutate(heating_data, Unique_ID = paste(date, location, id, sep = "."))

heating_data <- heating_data %>%
  filter(year > 2021)

#heating_data <- heating_data %>%
  #filter(location == "TN")

###############################################
### Code to estimate temperature thresholds ###
###############################################

#random number generation starting point for reproduceability
set.seed(99)

#chunk of code that creates a function that will estimate the various tcrit values for each species for each date
#Note that this needs to be run for each sampling date separately
psiiht=function(Temperature, FvFm, control.temp, id, plot.est, boots){
  l1=list(control.temp=control.temp, plot.est=plot.est, boots=boots)
  attach(l1)
  HTdf=data.frame(Temperature=Temperature, FvFm=FvFm, id=id)
  
  return(do.call("rbind", by(HTdf, list(HTdf$id), function(df){
    Temperature=df[,which(colnames(df)=="Temperature")]
    FvFm=df[,which(colnames(df)=="FvFm")]
    id=df[,which(colnames(df)=="id")]#this sets the species grouping
    #get parameter estimates for logistic decay model
    cof=coef(lm(logit(FvFm)~Temperature)) 
    #Fit a non linear least squares model to the FvFm and Temperature data
    HT.model <- nls(FvFm ~ theta1/(1 + exp(-(theta2 + theta3*Temperature))),  start=list(theta1 = .8, theta2 = cof[1], theta3 = cof[2]),
                    trace=F, control=list(maxiter=5000, tol=0.001))#had to relax the tolerance to get model convergence, original tolerance is 1e-3
    
    #Use the parameter estimates (coef(HT.model)[#])from the HT.model to predict a new fit based on a heat treatments from 23-62 degrees celcius. Here, # = 1:3.
    y<-coef(HT.model)[1]/(1+exp(-(coef(HT.model)[2]+coef(HT.model)[3]*seq(23,62)))) 
    
    #Calculate half of the control Fv/Fm & a 95% reduction in FvFm with reference to control
    half=mean(na.omit(FvFm[which(Temperature==control.temp)]))/2  
    nine5=mean(na.omit(FvFm[which(Temperature==control.temp)]))*0.05  
    #95 Confidence Interval
    predict.boot=matrix(NA,40, boots)
    T95=T50=Tcrit=c()
    for(k in 1:boots){
      #print(k)
      srows <- sample(1:length(Temperature), length(Temperature),TRUE)
      
      if(class(try(nls(FvFm[srows] ~ theta1/(1 + exp(-(theta2 + theta3*Temperature[srows]))),  start=list(theta1 = .8, theta2 = cof[1], theta3 = cof[2]),
                       trace=F, control=list(maxiter=1000, tol=.001)), silent=T)[[1]])=="nlsModel")
      {HT.model2 <- nls(FvFm[srows] ~ theta1/(1 + exp(-(theta2 + theta3*Temperature[srows]))),  start=list(theta1 = .8, theta2 = cof[1], theta3 = cof[2]),
                        trace=F, control=list(maxiter=1000, tol=0.001))
      predict.boot[,k]=coef(HT.model2)[1]/(1+exp(-(coef(HT.model2)[2]+coef(HT.model2)[3]*seq(23,62)))) 
      #Estimate T95
      T95[k]=(-log((coef(HT.model2)[1]/nine5)-1)-coef(HT.model2)[2])/coef(HT.model2)[3] 
      
      #Estimate T50
      T50[k]=(-log((coef(HT.model2)[[1]]/half)-1)-coef(HT.model2)[[2]])/coef(HT.model2)[[3]]
      T50k=(-log((coef(HT.model2)[[1]]/half)-1)-coef(HT.model2)[[2]])/coef(HT.model2)[[3]]
      #Use model to predict changes in FvFm & make new dataframe
      predict=data.frame(x=seq(23,62,length.out=80),y=coef(HT.model2)[1]/(1+exp(-(coef(HT.model2)[2]+coef(HT.model2)[3]*seq(23,62,length.out=80)))) ) #create a dataframe of predictions
      df1=cbind(predict[-1,], predict[-nrow(predict),])[,c(3,1,4,2)]
      #Use new dataframe to estimate the slope at between each 1-degree interval
      df1$slp=as.vector(apply(df1, 1, function(x) summary(lm((x[3:4]) ~ x[1:2])) [[4]][[2]] ))
      slp.at.tcrit=round(min(df1$slp), 3)*.15 #Determine where slope is 15% of max slope & round
      #Estimate the FvFm at which the slope is 15% of max slope & less than T50
      fvfv.at.tcrit=df1[which(abs(df1[which(df1[,1]<T50k),]$slp-slp.at.tcrit)==min(abs(df1[which(df1[,1]<T50k),]$slp-slp.at.tcrit))),][1,3]
      Tcrit[k]=(-log((coef(HT.model2)[[1]]/fvfv.at.tcrit)-1)-coef(HT.model2)[[2]])/coef(HT.model2)[[3]] # Estimate the temperatureat which the slope is 15% of max slope
      
      }else{(class(try(nls(FvFm ~ theta1/(1 + exp(-(theta2 + theta3*Temperature))),  start=list(theta1 = .8, theta2 = cof[1], theta3 = cof[2]),
                           data=data2[srows,], trace=F, control=list(maxiter=1000, tol=0.001)), silent=T)[[1]])=="list")
        predict.boot[,k]=NA
        T95[k]=NA
        T50[k]=NA
        Tcrit[k]=NA }}
    
    FvFm.boot=t(apply(predict.boot, 1, function(x){quantile(x,c(0.025,0.975),na.rm=T)}))
    
    Tcrit.ci=quantile(Tcrit,c(0.025,0.975),na.rm=T)
    T50.ci=quantile(T50,c(0.025,0.975),na.rm=T)
    T95.ci=quantile(T95,c(0.025,0.975),na.rm=T)
    #need to turn into ggplot if I want to write the plots to a list 
    #plist<-list()
    if(plot.est==T){ 
      # p.plot<-plot(NULL, NULL,xlab="Temperature",ylab="Fv/Fm",xlim=c(23,65),ylim=c(0,0.9), bty="l", lty=2)
      # text(22,0.3, pos=4, paste(unique(paste(id))), font=4)
      # points(Temperature, FvFm, xlab="Temperature", ylab="Fv/Fm",xlim=c(23,65), ylim=c(0,0.85), bty="l", lty=2, pch=5,  col="black")
      # lines(seq(23,62), y,lwd=1, col="black")
      # if(boots>1){
      # lines(seq(23,62), FvFm.boot[,1],lty=3, col="black")
      # lines(seq(23,62), FvFm.boot[,2],lty=3, col="black")
      # }
      # text(30,0, paste('Tcrit:', round(mean(na.omit(Tcrit)),1)), pos=4,col="light gray")
      # abline(v=round(mean(na.omit(Tcrit)),1), lty=2,lwd=1.5, col="light gray",  cex=0.8)
      # text(30,.1, paste('T50:',round(mean(na.omit(T50)),1)), pos=4, col="gray")
      # abline(v=round(mean(na.omit(T50)),1), lty=2,lwd=1.5, col="gray",cex=0.8)
      # text(30,.2, paste('T95:',round(mean(na.omit(T95)),1)),pos=4, col="black")
      # abline(v=round(mean(na.omit(T95)),1), lty=2,lwd=1.5, col="black", cex=0.8)
      # p <- recordPlot()
      # text(22,0.3, pos=4, paste(unique(paste(id))), font=4)
      # points(Temperature, FvFm, xlab="Temperature", ylab="Fv/Fm",xlim=c(23,65), ylim=c(0,0.85), bty="l", lty=2, pch=5,  col="black")
      # lines(seq(23,62), y,lwd=1, col="black")
      # if(boots>1){
      #    lines(seq(23,62), FvFm.boot[,1],lty=3, col="black")
      #    lines(seq(23,62), FvFm.boot[,2],lty=3, col="black")
    }
    
    #plist<-c(plist,p.plot)
    return(list(data.frame(id=(unique(id)),FvFm=y,
                           FvFmlow=round(FvFm.boot[,1],5),
                           FvFmhigh=round(FvFm.boot[,2],5),predict=predict.boot),
                data.frame(id=(unique(id)), 
                           Tcrit.lci=round(Tcrit.ci[[1]],1),
                           Tcrit.mn=round(mean(na.omit(Tcrit)),1),  
                           Tcrit.uci=round(Tcrit.ci[[2]],1),
                           T50.lci=round(T50.ci[[1]],1),
                           T50.mn=round(mean(na.omit(T50)),1),  
                           T50.uci=round(T50.ci[[2]],1),
                           T95.lci=round(T95.ci[[1]],1),
                           T95.mn=round(mean(na.omit(T95)),1),  
                           T95.uci=round(T95.ci[[2]],1))))
    
  })))
  detach(l1)
}#end of the function


temp<-psiiht(Temperature=heating_data$temperature, FvFm=heating_data$fv_fm, control.temp=23, id=heating_data$Unique_ID, plot.est=T, boots=100)

### WAIT FOR CODE TO RUN!! ###


#Create a single dataframe of bootstrap estimates
n_ID <- length(unique(heating_data$Unique_ID))
pred<-bind_rows(temp[1:n_ID])

#Create a single dataframe of the critical values
crits<-bind_rows(temp[n_ID+1:length(temp)])

#these are the critical values and need to be written to a file,
#but need columns for plots added

#Separate back out the date from the ID column
crits$date<-substr(crits$id, 1, 10)

#Separate back out the date from the ID column
crits$state<-substr(crits$id, 12, 13)

#remove the date from the ID column
crits$id<-str_sub(crits$id, 15, )

#convert these new columns back to numeric
crits$date<-as.Date(crits$date)

######NOTE: MAKE SURE YOU CHANGE THE FILE NAME SO YOU DON'T OVERWRITE A PREEXISTING FILE##############
write.xlsx(crits,"/Users/Joe/Documents/College/01- Data/crit_values_final.xlsx",
           col.names=TRUE, row.names=FALSE)

