# # # # # # # # # # # # # # # # 
# First Name : Avirat         #
# Last Name : Belekar         #
# CWID : 10454332             #
# Course : CS513- B           #
# Purpose : HW_03_KNN         #
# Email:abelekar@stevens.edu  #
# # # # # # # # # # # # # # # # 

rm(list = ls())
# import breast_cancer dataset and store it in dataset variable
dataset<-read.csv('/Users/aviratbelekar/Desktop/spring/kdd/breast-cancer-wisconsin.data.csv')

# Replace all ? with NA
dataset[dataset == '?']<- NA

#Remove Rows with NA
new_data<-na.omit(dataset)

# Convert all Categories to Factor datatype
new_data<-transform(new_data, F1 = as.factor(F1),F2 = as.factor(F2),F3 = as.factor(F3),F4 = as.factor(F4),F5 = as.factor(F5),F6 = as.factor(F6),F7 = as.factor(F7),F8 = as.factor(F8),F9 = as.factor(F9),Class = as.factor(Class))
new_data$Class<- factor(new_data$Class , levels = c("2","4") , labels = c("Benign","Malignant"))

# Split the data into training and testing 
idx<-sample(nrow(new_data),as.integer(.70*nrow(new_data)))
training<-new_data[idx,]
test<-new_data[-idx,]

# Install kknn library
library(kknn)
#Classification Model for k = 3
predict_k3<-kknn(formula = Class~.,training,test,k = 3,kernel = 'rectangular')
fit<-fitted(predict_k3)
#Display Confusion Matrix
table(Actual = test$Class, Fitted = fit)
conf_mat2<-table(Actual = test$Class, Fitted = fit)
conf_mat2
#Evaluate Accuracy
accuracy2<-sum(diag(conf_mat2)/nrow(test)) * 100 
accuracy2


#Classification Model for k = 5
predict_k5<-kknn(formula = Class~.,training,test,k = 5,kernel = 'rectangular')
fit<-fitted(predict_k5)
#Display Confusion Matrix
table(Actual = test$Class, Fitted = fit)
conf_mat1<-table(Actual = test$Class, Fitted = fit)
conf_mat1
# Evaluate Accuracy 
accuracy1<-sum(diag(conf_mat1)/nrow(test)) * 100 
accuracy1


#Classification Model for k = 10
predict_k10<-kknn(formula = Class~.,training,test,k = 10,kernel = 'rectangular')
fit<-fitted(predict_k10)
#Display Confusion Matrix
conf_mat<-table(Actual = test$Class, Fitted = fit)
conf_mat
# Evaluate Accuracy
accuracy<-sum(diag(conf_mat)/nrow(test)) * 100 
accuracy
