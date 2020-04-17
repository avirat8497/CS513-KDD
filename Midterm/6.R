# # # # # # # # # # # # # # # #
# First Name : Avirat         #
# Last Name : Belekar         #
# CWID : 10454332             #
# Course : CS513- B           #
# Purpose : HW_Midterm_Exam   #
# Email:abelekar@stevens.edu  #
# # # # # # # # # # # # # # # #

# Question 6
rm(list= ls())

# Import all libraries after installing packages
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

# Set Working Directory
setwd("~/Desktop/spring/kdd/Assignments")
#Import dataset
dataset = read.csv('COVID19_v3.csv')

# Omit Na Values
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

#Train the model on the rpart library
cart_class<-rpart(Infected~.,data = training)

# Plot the trained class
rpart.plot(cart_class)

# Predict using the trained model based on class.
cart_predict2<-predict(cart_class,testing,type = 'class')

# Print the confusion matrix
table(Actual =testing[,7],CART = cart_predict2)

# Predict using the trained model based on probabilities.
cart_predict<-predict(cart_class,testing)
#cart_predict
str(cart_predict)
cart_predict_cat<-ifelse(cart_predict[,1]<=0.5,'Yes','No')
table(Actual=testing[,7],CART=cart_predict_cat)

#Calculate the error rate
cart_wrong<-sum(testing[,7]!=cart_predict_cat)
cart_error_rate<-cart_wrong/ length(testing[,7])

cart_predict2<-predict(cart_class,testing, type="class")
cart_wrong2<-sum(testing[,7]!=cart_predict2)
cart_error_rate2<-cart_wrong2/length(testing[,7])

#Plotting the decision tree
library(rpart.plot)
prp(cart_class)

# Fancier plot of Decison Tree
fancyRpartPlot(cart_class)

