# Example-of-k-means-in-R
```R
#Set working directory path to whatever folder you like. 
setwd("C:\\Users\\Rebecca Merrett\\Documents\\r-workspace\\RebeccaSpace")

#Read in ARFF file: http://archive.ics.uci.edu/ml/datasets/Chronic_Kidney_Disease
#Use 32 bit version of R if you are on Windows as Java works for this. Need to have Java downloaded.
require(RWeka)
ChronicKidneyDisease <- read.arff("chronic_kidney_disease.arff")
head(ChronicKidneyDisease)

#Remove categorical variables as k means is used for numeric data only. Even if you convert categorical to binary(0,1 - 1 meaning 'yes', 0 meaning 'no', for example), it might be hard for the algorithm to measure distance.
#There are ways to work with categorical data for clustering but I'll show you next time, folks ;-)
DropColumns <- names(ChronicKidneyDisease) %in% c("sg", "al", "su", "rbc", "pc", "pcc", "ba", "htn", "dm", "cad", "appet", "pe", "ane", "class")
ChronicKidneyDiseaseNumeric <- ChronicKidneyDisease[!DropColumns]
head(ChronicKidneyDiseaseNumeric)

#Check for missing values. I think k means doesn't bode well with missing values when clustering records (rows).
#If there is a good portion of data missing spread throughout and don't have many records (400 not great), probably can't afford to delete all the rows that have missing value(s).
#If there is only one or few records where majority of values are missing (more than 70%, for example), then that one record probably not worth imputing possible bias/estimated values, so could delete.
ChronicKidneyDiseaseNumeric[(rowMeans(is.na(ChronicKidneyDiseaseNumeric)) >0.7), ]
#Btw, the code above lets you know which row numbers have more than 70% missing values.
#Now that we've found there are just a few rows that have more than 70% missing values, we can go ahead and delete them as probably want to impute too buch bias/estimation for these records.
ChronicKidneyDiseaseNumericRemovedNA <- ChronicKidneyDiseaseNumeric[-which(rowMeans(is.na(ChronicKidneyDiseaseNumeric)) >0.7), ]
ChronicKidneyDiseaseNumericRemovedNA

#Check each variable's min, max, etc. If the min and max are quite far apart, then maybe replacing all missing values with mean or mode(most frequent) won't be anywhere accurate estimation.
summary(ChronicKidneyDiseaseNumericRemovedNA)
#Predictive mean matching(PMM) in the MICE package is a method for predicting plausible missing values.
library(mice)
ChronicKidneyDiseaseMICE <- mice(ChronicKidneyDiseaseNumericRemovedNA, m=5, maxit=50, meth='pmm', seed=500)
ChronicKidneyDiseaseDataReady <- complete(ChronicKidneyDiseaseMICE, 1)
#Check there are no missing values
ChronicKidneyDiseaseDataReady
sapply(ChronicKidneyDiseaseDataReady, function(x)sum(is.na(x)))

#Now let's get into k means, woo!
#In the original dataset, there are two classes - chronic kidney disease and not chronic kidney disease. Let's try clustering the ready data without the class into 2 groups.
#Set seed to make result reproducible.
set.seed(1234)
ChronicKidneyDiseaseK2 <- kmeans(x=ChronicKidneyDiseaseDataReady, centers=2)
ChronicKidneyDiseaseK2
#Plot the clustered results.
require(useful)
plot(ChronicKidneyDiseaseK2, data=ChronicKidneyDiseaseDataReady)

#Now let's see how chronic and not chronic classes fall into the 2 groups/clusters.
#Chronic and not chronic are represented by shapes. If each shape closely matches each colour, then kmeans has done a good job of diagnosing patients.

#First, need to create a cleaned dataset that includes the "class" variable. Similar process to before.
DropColumns2 <- names(ChronicKidneyDisease) %in% c("sg", "al", "su", "rbc", "pc", "pcc", "ba", "htn", "dm", "cad", "appet", "pe", "ane")
ChronicKidneyDiseaseNumericClass <- ChronicKidneyDisease[!DropColumns2]
head(ChronicKidneyDiseaseNumericClass)
#Class dataset has less rows with more than 70% missing values than the other dataset. So need to delete the same rows as other dataset, so they both have the same number of rows.
ChronicKidneyDiseaseNumericClassRemovedNA <- ChronicKidneyDiseaseNumericClass[-c(24, 68, 144, 166, 216, 233), ]
ChronicKidneyDiseaseNumericClassRemovedNA
ChronicKidneyDiseaseMICE2 <- mice(ChronicKidneyDiseaseNumericClassRemovedNA, m=5, maxit=50, meth='pmm', seed=500)
ChronicKidneyDiseaseDataReadyClass <- complete(ChronicKidneyDiseaseMICE2, 1)
ChronicKidneyDiseaseDataReadyClass
sapply(ChronicKidneyDiseaseDataReadyClass, function(x)sum(is.na(x)))

#Now see if classes fall into the 2 clusters.
plot(ChronicKidneyDiseaseK2, data=ChronicKidneyDiseaseDataReadyClass, class="class")
```