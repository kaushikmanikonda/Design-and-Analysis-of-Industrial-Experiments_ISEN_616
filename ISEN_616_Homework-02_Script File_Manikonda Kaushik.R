#Script Title: ISEN_616_Assignment-02_Script File_Manikonda Kaushik
#Script Purpose: To satisfy the requirements for ISEN 616, Homework-02

#displays any existing objects in the workspace
ls()

#Step to check which liabraries are loaded
search()

#Removes all existing objects and variable assignments. 
#Will be commented out in the final homework to avoid repetition during future executions
rm(list=ls())
ls()

#This command directs all graphic output to a pdf file
pdf("C:\\Users\\rajak\\Documents\\Spring_2021\\ISEN_616\\Homeworks\\HW_02/HW02_output_File.pdf")

#This line imports the above mentioned data from a .csv file into R workspace.
p2_data <- read.csv("C:\\Users\\rajak\\Documents\\Spring_2021\\ISEN_616\\Homeworks\\HW_02/HW_02_Data.csv", sep="")
str(p2_data)

#This line splits the dataset into device dataset and doctors dataset.
p2_deviceset <- p2_data[ ,c(1:4)]
str(p2_deviceset)
p2_doctorset <- p2_data[ ,c(5:8)]
str(p2_doctorset)

##The following lines prepare the above datasets for ANOVA tables.

#This set of code is preparing the device dataset
devset <- p2_deviceset[ ,c(1,2)]
colnames(devset)[2] <- "Readings"
str(devset)
library(dplyr)
dummy1 <- p2_deviceset[ ,c(1,3)]
dummy2 <- p2_deviceset[ ,c(1,4)]
colnames(dummy1)[2] <- "Readings"
colnames(dummy2)[2] <- "Readings"
str(dummy1)
str(dummy2)
devset <- rbind(devset,dummy1,dummy2)
str(devset)
devcol <- c(rep("Dev1",15),rep("Dev2",15),rep("Dev3",15))
devset <- cbind(devset,devcol)
devset$devcol <- as.factor(devset$devcol)
colnames(devset)[3] <- "Device"
str(devset)

#This set of code is preparing the doctor dataset
docset <- p2_doctorset[ ,c(1,2)]
colnames(docset)[2] <- "Readings"
colnames(docset)[1] <- "Day"
str(docset)
library(dplyr)
dummy1 <- p2_doctorset[ ,c(1,3)]
dummy2 <- p2_doctorset[ ,c(1,4)]
colnames(dummy1)[2] <- "Readings"
colnames(dummy2)[2] <- "Readings"
colnames(dummy1)[1] <- "Day"
colnames(dummy2)[1] <- "Day"
str(dummy1)
str(dummy2)
docset <- rbind(docset,dummy1,dummy2)
str(docset)
doccol <- c(rep("Doc1",15),rep("Doc2",15),rep("Doc3",15))
docset <- cbind(docset,doccol)
docset$doccol <- as.factor(docset$doccol)
colnames(docset)[3] <- "Doctor"
str(docset)


install.packages("ggpubr")

###A new electronic device for measuring blood pressure is introduced in the
#market. An experiment has been conducted to compare the precision (measurement
#valiation) of measurements taken by the new device with those
#taken by doctors with existing devices. Three devices are randomly selected
#from store and three doctors (with their own existing devices) are randomly
#chosen from all available doctors. One patient's blood pressure has been
#monitored for 15 days. On each day, the blood pressure was read by all
#three devices and three doctors. The readings are listed in Table 2.11.


##(a) Analyze the blood pressures measured by devices and by doctors separately
#using a one-way random effects model. Your work should include
#two ANOYA tables, one for devices and one for doctors. You should
#also include F tests and estimates of variance components.

summary(p2_deviceset)
library ("ggpubr")
boxplot(devset$Readings~devset$Device, col = c("#00AFBB", "#E7B800","#FC4E07"), order = c("Dev1", "Dev2", "Dev3"), ylab = "Readings", xlab = "Device")
one.way_device <- aov(devset$Readings~devset$Device,data=devset)
summary(one.way_device)

summary(p2_doctorset)
boxplot(docset$Readings~docset$Doctor, col = c("#00AFBB", "#E7B800","#FC4E07"), order = c("Doc1", "Doc2", "Doc3"), ylab = "Readings", xlab = "Doctor")
one.way_doctor <- aov(docset$Readings~docset$Doctor,data=docset)
summary(one.way_doctor)

TukeyHSD(one.way_doctor)


##Problem 17 in Chapter 2 with redefined questions as below. Please disregard 
##the questions in the textbook.


##Show the ANOVA table for each dataset. Conduct the F test and write your 
##conclusions and their implications.

##(b) What can you conclude from (a)?

#From the ANOVA table for the device dataset, the F-value is 0 (zero), and 
#he p-value (p >F) is 1. So, we can accept the null hypothesis with 100% confidence. 
#he difference in measurements from the three devices is insignificant.
#In other ords, there is no significant difference in the blood pressure 
#measurements, regardless of which of the three devices we use.
  
#From the ANOVA table for the doctor dataset, the F-value is 139.1, and the 
#p-value (p >F) is 2e-16. So, the plausibility value of the null hypothesis is 
#very small. Hence, we reject the null hypothesis: "There is no significant 
#difference in BP measurement from doctor to doctor." We conclude that there 
#is a significant doctor-to-doctor difference in blood pressure measurement 
#at level 0.001.

##(c) Find 95% confidence intervals for the mean blood pressure measured by
#devices and the mean blood pressure measured by doctors.

m <- mean(p2_deviceset$Dev1)
sdev <- sd(p2_deviceset$Dev1)
Dev1_0.95CI <- c((m-(1.96*sdev)),(m+(1.96*sdev)))
Dev1_0.95CI

m <- mean(p2_deviceset$Dev2)
sdev <- sd(p2_deviceset$Dev2)
Dev2_0.95CI <- c((m-(1.96*sdev)),(m+(1.96*sdev)))
Dev2_0.95CI

m <- mean(p2_deviceset$Dev3)
sdev <- sd(p2_deviceset$Dev3)
Dev3_0.95CI <- c((m-(1.96*sdev)),(m+(1.96*sdev)))
Dev3_0.95CI


m <- mean(p2_doctorset$Doc1)
sdev <- sd(p2_doctorset$Doc1)
Doc1_0.95CI <- c((m-(1.96*sdev)),(m+(1.96*sdev)))
Doc1_0.95CI

m <- mean(p2_doctorset$Doc2)
sdev <- sd(p2_doctorset$Doc2)
Doc2_0.95CI <- c((m-(1.96*sdev)),(m+(1.96*sdev)))
Doc2_0.95CI

m <- mean(p2_doctorset$Doc3)
sdev <- sd(p2_doctorset$Doc3)
Doc3_0.95CI <- c((m-(1.96*sdev)),(m+(1.96*sdev)))
Doc3_0.95CI





###The bioactivity of four different drugs A, B, C, D for treating a particular
#illness was compared in a study and the following ANOVA table was given
#for the data:

#(a) Describe a proper design of the experiment to allow valid inferences to
#be made from the data.

#The given ANOVA table suggests a randomized model. So, to allow valid inferences,
#the drugs must have been administered to subjects at random. The individual 
#patients must have also been independent of each other. The normality of the 
#errors assumption must also be satisfied to allow valid inferences from this 
#ANOVA table.


#(b) Use an F test to test at the 0.01 level the null hypothesis that the
#four treatments have the same bioactivity. Compute the p value of the
#observed F statistic.

SSTr <- 64.42
SSE <- 62.12
n <- 30
k <- 3
F <- (SSTr/(k))/(SSE/(n-k-1))
F
#The next line gives the p-value corresponding to this F value.
pf(F,3,26,lower.tail=FALSE)

#The p-value (0.000297) is less than 0.01 level, and hence, we can 
#reject the null hypothesis. The variance ratio of 8.987 is significant 
#at the 0.01 level. So, the difference in the bioactivity levels from 
#Drugs A,B,C,& D is significant at the 0.01 level.

#(c) The treatment averages are as follows: YA = 66.10 (7 samples), Y 8 =
#65.75 (8 samples), Yc = 62.63 (9 samples), YD = 63.85 (6 samples).
#Use the Tukey method to perfOim multiple comparisons of the four
#treatments at the 0.01 level.


#*(d) It turns out that A and B are brand-name drugs and C and Dare
#generic drugs. To compare brand-name versus generic drugs, the
#contrast ! <y A + Y B) - t <y c + Y D) is computed. Obtain the p value
#of the computed contrast and test its significance at the 0.01 level.
#Comment on the difference between brand-name and generic drugs.

pt(4.7025, 19, lower.tail = FALSE)

graphics.off()
