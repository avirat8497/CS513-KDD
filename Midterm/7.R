# # # # # # # # # # # # # # # #
# First Name : Avirat         #
# Last Name : Belekar         #
# CWID : 10454332             #
# Course : CS513- B           #
# Purpose : HW_Midterm_Exam   #
# Email:abelekar@stevens.edu  #
# # # # # # # # # # # # # # # #

#Question 7
rm(list=ls())
setwd("~/Desktop/spring/kdd/Assignments")
dataset<-read.csv('COVID19_v3.csv')

# Omit NA values
dataset<-na.omit(dataset)

#split the dataset into training and testing
train_data = floor(0.7*nrow(dataset)) 
train_ind = sample(seq_len(nrow(dataset)),size = train_data)
train =dataset[train_ind,]
test=dataset[-train_ind,]

#install the KNN package
install.packages("kknn") 

# load important libraries
library(kknn)
library(class)

# KNN prediction 
predict_k5<-kknn(formula = Infected~.,train, test, k=5, kernel = "rectangular")
fit<-fitted(predict_k5)

# Displaying Confusion matrix
table(Actual=test$Infected, Fitted=fit)
conf_mat1<-table(Actual = test$Infected, Fitted = fit)
conf_mat1

# Accuracy of the model
accuracy1<-sum(diag(conf_mat1)/nrow(test)) * 100 
accuracy1 
