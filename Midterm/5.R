# # # # # # # # # # # # # # # #
# First Name : Avirat         #
# Last Name : Belekar         #
# CWID : 10454332             #
# Course : CS513- B           #
# Purpose : HW_Midterm_Exam   #
# Email:abelekar@stevens.edu  #
# # # # # # # # # # # # # # # #

# Question 5
library(e1071)
rm(list= ls())
setwd("~/Desktop/spring/kdd/Assignments")
dataset = read.csv('COVID19_v3.csv')

# Omit NA values
dataset<-na.omit(dataset)

# Discretize the “MonthAtHospital” into “less than 6 months” and “6 or more months”
dataset$Age<-cut(dataset$Age , c(0,35,50,60))
dataset$Age<- factor(dataset$Age, levels = c("(0,35]","(35,50]","(50,60]") , labels = c("Less 35","35 to 50","More than 50"))

# Discretize the Age into “less than 35”, “35 to 50” and “51 or over”
dataset$MonthAtHospital<-cut(dataset$MonthAtHospital, c(0,6,32))
dataset$MonthAtHospital<- factor(dataset$MonthAtHospital, levels = c("(0,6]","(6,32]") , labels = c("Less than 6 Months","6 months or more"))

# Split into training and testing (70% training  and 30% testing)
index<-sort(sample(nrow(dataset),round(0.30*nrow(dataset))))
training<-dataset[-index,]
testing<-dataset[index,]

# Train the model with Naive Bayes Algorithm 
nbayes_all<-naiveBayes(Infected~.,data = training)

# Predict using the testing data
category_all<-predict(nbayes_all,testing)
table(NBAYES_ALL = category_all,Class = testing$Infected)

# Calculate Error Rate
NB_wrong<-sum(category_all!=testing$Infected)
NB_error_rate<-NB_wrong/length(category_all)

#Calculating accuracy of the Naive Bayes Model 
accuracy<- 1 - NB_error_rate
accuracy
