# # # # # # # # # # # # # # # #
# First Name : Avirat         #
# Last Name : Belekar         #
# CWID : 10454332             #
# Course : CS513- B           #
# Purpose : HW_05_NaiveBayes  #
# Email:abelekar@stevens.edu  #
# # # # # # # # # # # # # # # #

#Clear all previous variables
rm(list = ls())
#installed.packages()
#install.packages('rpart')
#install.packages('rpart.plot')
#install.packages('rattle')
#install.packages('RColourBrewer')

# Import all libraries after installing packages
library(e1071)

#Set working directory
setwd("~/Desktop/spring/kdd/Assignments")

#Import breastcancer dataset
dataset<-read.csv(file = 'breast-cancer-wisconsin.data.csv')
dataset[dataset == '?']<- NA

#Remove Rows with NA
new_data<-na.omit(dataset)


# Convert 2,4 in class to Benign and Malignant
new_data<-transform(new_data, F1 = as.factor(F1),F2 = as.factor(F2),F3 = as.factor(F3),F4 = as.factor(F4),F5 = as.factor(F5),F6 = as.factor(F6),F7 = as.factor(F7),F8 = as.factor(F8),F9 = as.factor(F9),Class = as.factor(Class))
new_data$Class<- factor(new_data$Class , levels = c("2","4") , labels = c("Benign","Malignant"))

# Split the data into training and testing(Training = 70%, Testing = 30%)
index<-sort(sample(nrow(new_data),round(0.30*nrow(new_data))))
training<-new_data[-index,]
testing<-new_data[index,]

# Train the model with Naive Bayes Algorithm 
nbayes_all<-naiveBayes(Class~.,data = training)

# Predict using the testing data
category_all<-predict(nbayes_all,testing)

# Print Confusion Matrix 
table(NBAYES_ALL = category_all,Class = testing$Class)

# Calculate Error Rate
NB_wrong<-sum(category_all!=testing$Class)
NB_error_rate<-NB_wrong/length(category_all)

# Compute Accuracy
accuracy<- 1 - NB_error_rate
accuracy
