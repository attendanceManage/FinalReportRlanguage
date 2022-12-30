install.packages("tidyverse")
library(tidyverse)
library(stats)

library("dplyr")
library(ggplot2)
library(patchwork)
library(lubridate)

##import data into R language
data = read.csv("C:\\Users\\oladayo\\Documents\\MyRProject\\data.csv"")
#data cleaning and removing two columns - Business.Associate.Present, Web.Description

## Performing EDA

str(data)


#calculate the mean of one category inside Individuals.Affected
mean(data$Individuals.Affected[data$Type.of.Breach=="Hacking/IT Incident" ])

median(data$Individuals.Affected[data$Type.of.Breach=="Hacking/IT Incident" ])


#calculate the mean of two categories
mean(data$Individuals.Affected[data$Type.of.Breach=="Hacking/IT Incident" & data$Location.of.Breached.Information=="Email"])

median(data$Individuals.Affected[data$Type.of.Breach=="Hacking/IT Incident" & data$Location.of.Breached.Information=="Email"])

colSums(data$Individuals.Affected)

max(data$Individuals.Affected[data$Type.of.Breach=="Hacking/IT Incident" & data$Location.of.Breached.Information=="Email"])
min(data$Individuals.Affected[data$Type.of.Breach=="Hacking/IT Incident" & data$Location.of.Breached.Information=="Email"])

quantile(data$Individuals.Affected[data$Type.of.Breach=="Hacking/IT Incident" & data$Location.of.Breached.Information=="Email"])

sd(data$Individuals.Affected[data$Type.of.Breach=="Hacking/IT Incident" & data$Location.of.Breached.Information=="Email"])

var(data$Individuals.Affected[data$Type.of.Breach=="Hacking/IT Incident" & data$Location.of.Breached.Information=="Email"])

mode = function(x){
  ta = table(x)
  tam = max(ta)
  if(all(ta==tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta==tam])
  else
    mod =names(ta)[ta==tam]
  return(mod)
}
mode(data$Individuals.Affected[data$Type.of.Breach=="Hacking/IT Incident" & data$Location.of.Breached.Information=="Email"])

cdata$newdate <-mdy(data$Breach.Submission.Date)
#year

data$year <- year(data$newdate)

data$year


head(data)

#formular for correlation

cor.test(data$Individuals.Affected, data$year)


#hypotesis
mean(data$Individuals.Affected)

t.test(data$Individuals.Affected[data$Type.of.Breach=="Hacking/IT Incident" ],mu =203093.1)
str(data)


data$Individuals.Affected <-as.numeric(data$Individuals.Affected)


ggplot(data, aes(x=State,y=Individuals.Affected))+geom_point()

ggplot(data, aes(x=State,y=year))+geom_boxplot()


table(data$State)
table(data$Individuals.Affected)

ggplot(aes(x=data(Individuals.Affected))+
         geom_bar(fill="blue")
         theme_bw()+
         labs(x="State",y=NULL)) 