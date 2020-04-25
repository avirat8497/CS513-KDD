# # # # # # # # # # # # # # # # 
# First Name : Avirat         #
# Last Name : Belekar         #
# CWID : 10454332             #
# Course : CS513- B           #
# Purpose : HW_06_C50         #
# Email:abelekar@stevens.edu  #
# # # # # # # # # # # # # # # # 
#install.packages("C50")
library('C50')

#Clear all previous variables
rm(list = ls())


#Set working directory 
setwd("~/Desktop/spring/kdd/Assignments/Homework")

#Import breastcancer dataset
dataset<-read.csv(file = 'breast-cancer-wisconsin.data.csv',stringsAsFactors = FALSE)

#Replace all NA with zeroes
dataset[dataset == '?']<-0

#Convert all columns to factor datatype
new_data<-transform(dataset, F1 = as.factor(F1),F2 = as.factor(F2),F3 = as.factor(F3),F4 = as.factor(F4),F5 = as.factor(F5),F6 = as.factor(F6),F7 = as.factor(F7),F8 = as.factor(F8),F9 = as.factor(F9),Class = as.factor(Class))

# Convert 2,4 in class to Benign and Malignant
new_data$Class<- factor(new_data$Class , levels = c("2","4") , labels = c("Benign","Malignant"))

#Split training and testing(training = 75% , testing = 25%)
set.seed(135)
index<-sort(sample(nrow(new_data),round(0.25*nrow(new_data))))
training<-new_data[-index,]
testing<-new_data[index,]

# Create c50 classification model
C50_class<-C5.0(Class~.,data = training)

# Summary of the c50 class
summary(C50_class)

#Plot C50 class
plot(C50_class)

# Predict whether the new testing value is Benign or Malignant
C50_predict<-predict( C50_class ,testing , type="class" )

#Confusin Matrix
conf_mat<-table(actual=testing[,11],C50=C50_predict)

#Print Accuracy
accuracy<-sum(diag(conf_mat)/nrow(testing)) * 100
accuracy


