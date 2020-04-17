# # # # # # # # # # # # # # # #
# First Name : Avirat         #
# Last Name : Belekar         #
# CWID : 10454332             #
# Course : CS513- B           #
# Purpose : HW_Midterm_Exam   #
# Email:abelekar@stevens.edu  #
# # # # # # # # # # # # # # # #

# Question 2
rm(list= ls())
setwd("~/Desktop/spring/kdd/Assignments")
dataset = read.csv('COVID19_v3.csv')

# Summarizing each column
summary(dataset)

# Identifying missing values
is.na(dataset)

# Displaying frequency table of infected vs martialstatus
frequency_table<-table(dataset$Infected , dataset$MaritalStatus)
frequency_table

# Creating a new data frame consisting of only Age MaritalStatus and Month at Hospital
new_data <- data.frame(dataset$Age, dataset$MaritalStatus, dataset$MonthAtHospital)

#Displaying the scatter plot of “Age”, “MaritalStatus” and “MonthAtHospital”, one pair at a time
plot(new_data[1:3], main = "Scatter Plot", pch = 10,col = c("red","black","blue"))

#Displaying the Box plot of “Age”, “MaritalStatus” and “MonthAtHospital”
boxplot(new_data[1:3], main = "Box Plot", pch = 10,col = c("red","black","blue"))

