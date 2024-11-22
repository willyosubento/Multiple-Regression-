#load the packages
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
#=============================================================================================================================================
Emotional_data<-read.csv("C:\\Users\\user\\Desktop\\RDocuments\\12.Multiple Regression Model\\Emotional Intelligence Test.csv")
view(Emotional_data)
names(Emotional_data)
model1<-lm(Project.Average~Intrapersonal.Range + Stress.Range + Mood.Range + Intrapersonal.Range*Stress.Range +
           Intrapersonal.Range*Mood.Range + Intrapersonal.Range*Stress.Range*Mood.Range +
           I(Intrapersonal.Range^2) + I(Stress.Range^2) + I(Mood.Range^2),data = Emotional_data)
summary(model1)
#===================================================================================================================================================
Employee_data<-read.csv("C:\\Users\\user\\Desktop\\RDocuments\\12.Multiple Regression Model\\Eployees_work_Hours.csv")
view(Employee_data)
names(Employee_data)
model2<-lm(WORK_HOURS_MISSED~ANNUAL.WAGES,data = Employee_data)
summary(model2)
#==============================================================================================================================================
# Plot residuals
par(mfrow = c(2, 2))  # Set up 2x2 grid for diagnostic plots
plot(model2,main = "Plot for Regression Residuals")
#=====================================================================================================================================================
#Remove the 13th row entirely from the data set(Removing the outlier)
Employee_data_13 <- Employee_data[-13, ]
view(Employee_data_13)
#Fitting the model without an outlier
model3<-lm(WORK_HOURS_MISSED~ANNUAL.WAGES,data = Employee_data_13)
summary(model3)
#=================================================================================================================
#Rcode for the dummy variables
milk_data<-data.frame(Group_=c("x1","x2","x3","x1","x2","x3"))
#create dummy variable
model_matrix<-model.matrix(~0 + Group_, data = milk_data)
#view the dummy variable
view(model_matrix)
#OR
model_matrix













