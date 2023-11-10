# R example for multivariate data

# Load mammal brain weight data set
sch.df <-read.csv("C:/Users/firas/OneDrive/Desktop/BIOL4062/3_Multivar_distances/Data/Schoenemann.csv",header=T)
summary(sch.df)

# Create a new data frame which consists of log-transformed data
lsch <- data.frame(lFat = log(sch.df$Fat), lCNS = log(sch.df$CNS), lBone = log(sch.df$Bone),
                   lMass = log(sch.df$Mass), lMuscle = log(sch.df$Muscle), Order = sch.df$Order)

summary(lsch)

# Do a 3d plot (using base R, not ggplot2 at present)
library(lattice)
cloud(lCNS ~ lMuscle*lBone, data=lsch,groups=Order,ylab="Bone", xlab="Muscle",zlab="CNS")
cloud(lCNS ~ lMuscle*lBone, data=lsch,groups=Order, pch = 10, type = "p", ylab="log(Bone)",xlab="log(Muscle)",zlab="log(CNS)",auto.key=TRUE) #adds key


# Quickly aggregate mean values by order
mean_vals = aggregate(lsch[,1:5],list(lsch$Order),FUN=mean)
#selecting all the rows and apply mean value by order
#calculate Euclidean distance only after you dd this

# Tidyverse alternative
library(tidyverse)
lsch %>% group_by(Order) %>% summarise_all(mean) 


# Calculate Euclidean distances between species and orders
#we selected 1-5 culumns, if adding 1:x before the coma, try to get the rows.
#make sure you select the numeric columns only for the Euclidean space
dist(lsch[,1:5], method="euclidean")
dist(mean_vals[,2:6], method="euclidean")
dist(mean_vals[,-1], method = "euclidean")
#-1 means drop this value. Do the analysis on everything except this

# Mahalanobis distances for Lynx (first row/unit of data)
mahalanobis(lsch[,-6], as.matrix(lsch[1,-6]), cov(lsch[,-6])) 

# Mahalanobis distances from first order (Carnivora) to others
mahalanobis(mean_vals[, -1], as.matrix(mean_vals[1,-1]), cov(mean_vals[,-1])) 


# All Mahalanobis distances among orders
mahv<-matrix(0, length(mean_vals[,1]), length(mean_vals[,1]))

# Version using looping
for (jj in 1:length(mean_vals[,1])) {
  mahv[jj ,] <- mahalanobis(mean_vals[, -1], as.matrix(mean_vals[jj,-1]), cov(mean_vals[,-1])) 
  }
data.frame(Order = mean_vals[,1],mahv)

# Saving manipulated data
save(lsch,file="C:/Users/firas/OneDrive/Desktop/BIOL4062/3_Multivar_distances/Output/schoenemann.Rdata")
