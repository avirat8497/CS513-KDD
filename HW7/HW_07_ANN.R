# # # # # # # # # # # # # # # # 
# First Name : Avirat         #
# Last Name : Belekar         #
# CWID : 10454332             #
# Course : CS513- B           #
# Purpose : HW_07_ANN         #
# Email:abelekar@stevens.edu  #
# # # # # # # # # # # # # # # # 
#install.packages("neuralnet")
library('neuralnet')

#Clear all previous variables
rm(list = ls())

#Set working directory 
setwd("~/Desktop/spring/kdd/Assignments/Homework")

#Import breastcancer dataset
dataset<-read.csv(file = 'wisc_bc_ContinuousVar.csv')

# Converting Diagnosis column to Numeric
dataset<-data.frame(lapply(na.omit(dataset),as.numeric))
#dataset<-transform(dataset, diagnosis = as.numeric(diagnosis), id = as.numeric(id))
#sapply(dataset,class)

#Split training and testing(training = 75% , testing = 25%)
set.seed(135)
index<-sort(sample(nrow(dataset),as.integer(0.25*nrow(dataset))))
training<-dataset[-index,]
testing<-dataset[index,]

# Implement Neural Network Model with 5 hidden layers
ann_class<-neuralnet(diagnosis~.,data = training[-1],hidden = 5,threshold = 0.01,act.fct = "logistic")

#Plot Artificial Neural Network model
plot(ann_class)

# Predict
ann_predict<-predict(ann_class,testing)
ann_predict
ann_predict_cat<-ifelse(ann_predict<1.5,'1','2')

# Print confusion Matrix
conf_mat<-table(Actual=testing[,2],Predicted=ann_predict_cat)

#Evaluating the accuracy
accuracy<-sum(diag(conf_mat))/ nrow(testing) * 100
accuracy

