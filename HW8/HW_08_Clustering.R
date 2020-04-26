# # # # # # # # # # # # # # # # 
# First Name : Avirat         #
# Last Name : Belekar         #
# CWID : 10454332             #
# Course : CS513- B           #
# Purpose : HW_08_Clustering  #
# Email:abelekar@stevens.edu  #
# # # # # # # # # # # # # # # #

#Clear all previous variables
rm(list = ls())

#Set working directory 
setwd("~/Desktop/spring/kdd/Assignments/Homework")

#Import breastcancer dataset
dataset<-read.csv(file = 'wisc_bc_ContinuousVar.csv')

# Displaying the summary of the dataset
summary(dataset)
table(dataset$diagnosis)

#To factor the data set
dataset<-na.omit(dataset)
dataset<-dataset[-1]
dataset_dist<-dist(dataset[,-1])

#Implementing Hclust Algorithm
hclust_results<-hclust(dataset_dist)
plot(hclust_results)
hclust_2<-cutree(hclust_results,2)
conf_mat<-table(hclust_2,dataset[,1])
accuracy<-sum(diag(conf_mat))/nrow(dataset) * 100
accuracy


# Question 2
# Clearing all previous variables
rm(list = ls())

#Set working directory 
setwd("~/Desktop/spring/kdd/Assignments/Homework")

#Import breastcancer dataset
dataset<-read.csv(file = 'wisc_bc_ContinuousVar.csv')

# Displaying the summary of the dataset
summary(dataset)
table(dataset$diagnosis)

#To factor the data set
dataset<-na.omit(dataset)
dataset<-dataset[-1]

# Implement K means algorithm
kmeans_2<- kmeans(dataset[,-1],2,nstart = 10)
kmeans_2$cluster
conf_mat<-table(kmeans_2$cluster,dataset[,1])
accuracy<-sum(diag(conf_mat))/nrow(dataset) * 100
accuracy

