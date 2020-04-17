# # # # # # # # # # # # # # # # 
# First Name : Avirat         #
# Last Name : Belekar         #
# CWID : 10454332             #
# Course : CS513- B           #
# Purpose : HW_05_DTree       #
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
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

#Set working directory 
setwd("~/Desktop/spring/kdd/Assignments")

#Import breastcancer dataset
dataset<-read.csv(file = 'breast-cancer-wisconsin.data.csv')

#Convert all columns to factor datatype
new_data<-transform(dataset, F1 = as.factor(F1),F2 = as.factor(F2),F3 = as.factor(F3),F4 = as.factor(F4),F5 = as.factor(F5),F6 = as.factor(F6),F7 = as.factor(F7),F8 = as.factor(F8),F9 = as.factor(F9),Class = as.factor(Class))

# Convert 2,4 in class to Benign and Malignant
new_data$Class<- factor(new_data$Class , levels = c("2","4") , labels = c("Benign","Malignant"))

#Split training and testing(training = 75% , testing = 25%)
set.seed(135)
index<-sort(sample(nrow(new_data),round(0.25*nrow(new_data))))
training<-new_data[-index,]
testing<-new_data[index,]

#Train the model on the rpart library
cart_class<-rpart(Class~.,data = training)

# Plot the trained class
rpart.plot(cart_class)

# Predict using the trained model based on class.
cart_predict2<-predict(cart_class,testing,type = 'class')
#cart_predict2

# Print the confusion matrix
table(Actual =testing[,11],CART = cart_predict2)

# Predict using the trained model based on probabilities.
cart_predict<-predict(cart_class,testing)
#cart_predict
str(cart_predict)
cart_predict_cat<-ifelse(cart_predict[,1]<=0.5,'Benign','Malignant')
table(Actual=testing[,11],CART=cart_predict_cat)

#Calculate the error rate
cart_wrong<-sum(testing[,11]!=cart_predict_cat)
cart_error_rate<-cart_wrong/ length(testing[,11])

cart_predict2<-predict(cart_class,testing, type="class")
cart_wrong2<-sum(testing[,11]!=cart_predict2)
cart_error_rate2<-cart_wrong2/length(testing[,11])
#cart_error_rate2

#Plotting the decision tree
library(rpart.plot)
prp(cart_class)

# Fancier plot of Decison Tree
fancyRpartPlot(cart_class)
