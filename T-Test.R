setwd("D:/DataAnalysisRProject/")

install.packages("tidyverse")
install.packages("gplots")
install.packages("ggpubr")
install.packages("ggplot2")
library(gplots)
library(ggpubr)
library(ggplot2)
library("dplyr")
data = read.csv("data.csv")

names(data)

dataSubmission <-  data.frame(Date 
                              = substring(data$Breach.Submission.Date,0,4),
                              Individual 
                              = data$Individuals.Affected,
                              Breached = data$Location.of.Breached.Information )

dataSubmission <- dataSubmission %>% 
                                    filter(dataSubmission$Date >='2018' & dataSubmission$Date<='2018' )

#View(dataSubmission)
install.packages("lessR")
library(lessR)     


sd(dataSubmission$Date, na.rm=TRUE)

hist <- ggplot(dataSubmission, aes(x=Date)) +
  geom_histogram(color="darkblue", fill="lightblue") +
  labs(title="Histogram for Years in School")
hist

#hist <- ggplot(aes(x=dataSubmission$Individual,fill=dataSubmission$Individual,y=dataSubmission$Date)) +
 #       geom_col(position = "dodge") +
  #      labs(title="Histogram for Years in School")
#hist
#plot(dataSubmission$Individual,data=dataSubmission,by1=dataSubmission$Breached)
#mean(as.numeric(dataSubmission$Date),na.rm=TRUE)
