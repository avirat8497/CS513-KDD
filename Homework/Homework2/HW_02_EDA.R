# # # # # # # # # # # # # # # # 
# First Name : Avirat         #
# Last Name : Belekar         #
# CWID : 10454332             #
# Course : CS513- B           #
# Purpose : HW_02_EDA         #
# Email:abelekar@stevens.edu  #
# # # # # # # # # # # # # # # # 

# import breast_cancer dataset and store it in dataset variable
dataset<-read.csv('/Users/aviratbelekar/Desktop/spring/kdd/breast-cancer-wisconsin.data.csv')

#Summarize each column
summary(dataset)

#Identifying missing values
dataset[dataset == '?']<- NA
is.na(dataset)

#Replacing the missing values with the “mean” of the column.
new_data<-transform(dataset, F6 = as.integer(F6))
for(i in 1:ncol(new_data)){
  new_data[is.na(new_data[,i]), i] <- mean(new_data[,i], na.rm = TRUE)
}  

# Displaying the frequency table of “Class” vs. F6
myTable <- table(new_data$Class, new_data$F6)
ftable(myTable)

# Displaying the scatter plot of F1 to F6, one pair at a time
plot(new_data[2:7], main = "Scatter Plot from F1 to F6",pch = 10,col=c("red","black"))

#Show histogram box plot for columns F7 to F9
boxplot(new_data[8:10], main = "Box Plot for Columns F7 to F9",pch = 10,col=c("red","black"))

# Delete all the objects from your R- environment.
rm(list = ls())

# import breast_cancer dataset and store it in dataset variable
dataset<-read.csv('/Users/aviratbelekar/Desktop/spring/kdd/breast-cancer-wisconsin.data.csv')

# Replace all ? with NA
dataset[dataset == '?']<- NA

#Remove Rows with NA
new_data<-na.omit(dataset)

