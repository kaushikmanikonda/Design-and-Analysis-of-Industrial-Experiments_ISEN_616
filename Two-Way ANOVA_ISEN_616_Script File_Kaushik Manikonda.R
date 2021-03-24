#Script Title: ISEN_616_Assignment-03_Script File_Manikonda Kaushik
#Script Purpose: To satisfy the requirements for ISEN 616, Homework-03

#displays any existing objects in the workspace
ls()

#Step to check which liabraries are loaded
search()

#Removes all existing objects and variable assignments. 
#Will be commented out in the final homework to avoid repetition during future executions
rm(list=ls())
ls()


###1. Problem 8 in Chapter 3.
#8. A chemical reaction experiment was carried out with the objective of comparing
#if a new catalyst B would give higher yields than the old catalyst A. The experiment 
#was run on six different batches of raw material which were known to be quite 
#different from one another. Each batch was divided into two portions to which A or B 
#was applied at random. The data collected are given in Table 3.43.


##(a) Explain the experimental design.
#Objective: To compare two catalysts A & B to see if B would give a higher yield 
#than A. We can also restate this as, “To compare catalysts A&B to determine if 
#there is a significant difference in performance between the two.” 
#Experimental Design: Six batches of raw materials were collected that are known to be quite 
#different from each other. So, the “batch” (6 batches) is the blocking variable. 
#However, we are not very interested in the differences among these six batches. 
#We are only interested in the performance differences between catalyst A &B. 
#Each of these 6 different batches of raw materials (units) were put through a 
#pair of catalysts (treatments) A & B. So, this is a pair-wise comparison experiment. 
#It is a paired comparison because each batch (or unit) is divided and randomly 
#assigned to go through a pair of catalysts (treatments) A & B. Since the batches 
#are divided and the randomly assigned between A&B, the randomization principle of 
#experimental design is also satisfied here.


##(b) Carry out the appropriate t test.
library ("ggpubr")
p8data <- read.csv("C:\\Users\\rajak\\Documents\\Spring_2021\\ISEN_616\\Homeworks\\HW_03\\HW-03_P8 Data.csv")
str(p8data)
p8data$Batch <- as.factor(p8data$Batch)
p8data$catalyst <- as.factor(p8data$catalyst)
str(p8data)
ANOVA <- aov(p8data$response~p8data$Batch+p8data$catalyst,data=p8data)
summary(ANOVA)

##t-test
di <- p8data$response[7:12]-p8data$response[1:6]
di
mean(di);sd(di)
tpaired <- sqrt(length(p8data$Batch)/2)*mean(di)/sd(di)
tpaired
qt(0.95,5)
qt(0.975,5)
#tpaired (=2.645751) > tcritical (=2.015048), so, the yields from A&B are 
#significantly different at significance level 0.05.

##(c) Construct a 95% confidence interval for the difference between catalysts A and B.
TukeyHSD(ANOVA,"p8data$catalyst",ordered=TRUE, conf.level = 0.95)




###Problem 13 in Chapter 3. You only need to do show the ANOVA table and 
#conduct the F test for this problem.
#13.For the composite experiment of Section 2.3, the original paper by 
#Mazumdar and Hoa (1995) reported a second factor, tape speed. Table 3.44 
#shows the three replicates for each level of laser power corresponding to 
#tape speed of 6.42, 13.0, and 27.0 mis, respectively. The levels of tape 
#speed are roughly evenly spaced on the log scale, so that linear and quadratic
#effects can be entertained for the second quantitative factor. Analyze the
#experiment as a two-way layout with a single replicate, including model
#building, parameter estimation, ANOVA and residual analysis.

##Importing data and preparing it for analysis.
p13data <- read.csv("C:\\Users\\rajak\\Documents\\Spring_2021\\ISEN_616\\Homeworks\\HW_03\\HW-03_P13 Data.csv")
str(p13data)
p13data$Tape_speed <- as.factor(p13data$Tape_speed)
p13data$Laser_power <- as.factor(p13data$Laser_power)
str(p13data)

##TWO-WAY ANOVA
p13anova1 <- aov(p13data$Response~p13data$Tape_speed+p13data$Laser_power,data=p13data)
summary(p13anova1)

#Both tape speed and laser power have significant difference among their respective
#levels at a significance level of 0.05. At a significance level of 0.01, only laser
#power has a significant difference in response among its levels.

##Trial ANOVA Table
p13anova2 <- aov(p13data$Response~p13data$Tape_speed+p13data$Laser_power+p13data$Tape_speed:p13data$Laser_power,data=p13data)
summary(p13anova2)
