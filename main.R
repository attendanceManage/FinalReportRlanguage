
install.packages("tidyverse")
install.packages("lubridate")

library(tidyverse)
library(stats)

library("dplyr")   
library(ggplot2)
library(patchwork)

#########
##import data into R language
#########
data = read.csv("D:/DataAnalysisRProject/data_breaches_all_clean.csv")

## data cleaning and remove two columns 
# Business.Associate.Present, Web.Description
#########

data %>% select(-Business.Associate.Present,-Web.Description) %>%
        slice(-8,-9)->datasetCleaned   
#########
#getting date from date between 2009 to 2019  not above than
#########

datasetCleaned$Breach.Submission.Date <- as.Date(datasetCleaned$Breach.Submission.Date, format = "%m/%d/%Y") 
write.csv(datasetCleaned %>% filter(datasetCleaned$Breach.Submission.Date  < '2020-1-1' 
                          & datasetCleaned$Covered.Entity.Type!="Business Associate"), 
                          "D:/DataAnalysisRProject/data.csv", row.names=FALSE)


#########
##getting date from date between 2009 to 2019  not above than
##2019 and we are analysis data only health department
#########
datafrom = read.csv("D:/DataAnalysisRProject/data.csv")


#########
#Which is biggest medium of data (hacking)
# Create the function.
#########
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

biggestMedium <- getmode(
  datafrom$Location.of.Breached.Information)
#########
# biggest medium of data (hacking) 
#answer Network Server
#########
print(biggestMedium)

#How does it affect individuals

GFG <- data.frame(
  State         =   datafrom$State, 
  Frequency= datafrom$Individuals.Affected

)

GFG %>% group_by(State)%>%
 summarise(individual=mean(Frequency))%>%

    ggplot(aes(x=State,fill=State,y=individual)) +  
     geom_col(position = "dodge") +
     labs(title="Affect individuals according state wise USA")


#########
#How many times did each organisation get hacked
#How many times do data types lose / hack a particular organisation?
#########
Organization <- data.frame(
       Name         = datafrom$Name.of.Covered.Entity, 
       Date         = datafrom$Breach.Submission.Date
)




#Organization %>% group_by(Name)%>%
  #summarise(date=mean(Date))%>%
  
   # ggplot(aes(x=Name,fill=Name,y=date))  +
    #geom_col(position = "dodge") +
    #labs(title="each organisation get hacked") 
  
  

#########
#What was the central tendency of healthcare data breach by a year?
# 2015 to 2019  
#########

dataSubmission <-  data.frame(Date 
                              = substring(datafrom$Breach.Submission.Date,0,4))


dataSubmission <- dataSubmission %>% filter(dataSubmission$Date >='2019' & dataSubmission$Date  <= '2019'
                                            | dataSubmission$Date >='2018' & dataSubmission$Date  <= '2018' 
                                            | dataSubmission$Date >='2017' & dataSubmission$Date  <= '2017' 
                                            | dataSubmission$Date >='2016' & dataSubmission$Date  <= '2016'
                                            | dataSubmission$Date >='2015' & dataSubmission$Date  <= '2015'
                  ) 
  


mean(as.numeric(dataSubmission$Date),na.rm=TRUE)

hist(as.numeric(dataSubmission$Date),
     xlab="Count",
     ylab="Central tendency of healthcare data breach by a year",
     ylim=c(0,500),
     xlim=c(2015,2019),
     col="darkmagenta"
)


#############
#
#When was the worst year for an overall number of healthcare data breaches
#############
worstYear <- getmode(dataSubmission$Date)
print(worstYear)



  

