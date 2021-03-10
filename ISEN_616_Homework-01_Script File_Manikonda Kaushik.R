#Script Title: ISEN_616_Assignment-01_Script File_Manikonda Kaushik
#Script Purpose: To satisfy the requirements for ISEN 616, Homework-01

#displays any existing objects in the workspace
ls()

#Step to check which liabraries are loaded
search()

#Removes all existing objects and variable assignments. 
#Will be commented out in the final homework to avoid repetition during future executions
rm(list=ls())
ls()

#This command directs all graphic output to a pdf file
pdf("C:\\Users\\rajak\\Documents\\Spring_2021\\ISEN_616\\Homeworks\\HW_01/HW01_Output_File.pdf")



### Question_01 Textbook Pg. 37, Problem 9:

#In the winter, a plastic rain gauge cannot be used to collect precipitation 
#data because it will freeze and crack. As a way to record snowfall, weather 
#observers were instructed to collect the snow in a metal standard 2.5 can, 
#allow the snow to melt indoors, pour it into a plastic rain gauge, and then 
#record the measurement. An estimate of the snowfall is then obtained by 
#multiplying the measurement by 0.44 (The factor 0.44 was theoretically derived 
#as the ratio of the surface area of the rectangular opening of the rain gauge 
#and of the circular metal can.) One observer questioned the validity of the 
#0.44 factor for estimating snowfall. Over one summer, the observer recorded 
#the following rainfall data collected in the rain gauge and in the standard 
#2.5 can, both of which were mounted next to each other at the same height. 
#The data (courtesy of Masaru Hamada) appear in Table 1.10, where the first 
#column is the amount of rain (in inches) collected in the standard 2.5 can 
#(x) and the second column is the amount of rain (in inches) collected in the 
#rain gauge (y).

#This line imports the above mentioned data from a .csv file into R workspace.
rain_data <- read.csv("C:\\Users\\rajak\\Documents\\Spring_2021\\ISEN_616\\Homeworks\\HW_01/Problem_01.csv", sep="")

#This line examines the structure of the data in the new object "rain_data"
str(rain_data)
length(rain_data$x)
length(rain_data$y)

par(mfrow=c(2,2))

#a) Plot the residuals yi – 0.44xi for the data. Do you observe any systematic 
#pattern to question the validity of the formula y = 0.44x?
attach(rain_data)
#proposed_lm <- (y - 0.44*x)
#residuals <- resid(proposed_lm)
plot(x, y-0.44*x)
abline(h=0, col="blue")
plot(y, y-0.44*x)
abline(h=0, col="blue")

#Yes, from the plots, we observe a clear skew to the positive y-direction. This
#implies that the model is underestimating the value more than it is overestimating.
#So, y = 0.44x is slightly underestimating the values of y, and hence has questionable
#validity.


#b) Use regression analysis to analyze the data in Table 1.10 by assuming a 
#general ß0 (i.e. an intercept term) and ß0 = 0 (i.e. regression line through 
#the origin.) How well do the two models fit the data? Is the intercept term significant?

lm.fit <- lm(y~x, data=rain_data)
summary(lm.fit)
residuals <- resid(lm.fit)
plot(x, residuals, ylab="Residuals", xlab="x", main="Resid vs. x with intercept") 
abline(h=0, col="blue")
par(mfrow=c(2,2))
plot(lm.fit, main="Model with Intercept")

lm.fit_2 <- lm(y~x+0, data=rain_data)
summary(lm.fit_2)
residuals_2 <- resid(lm.fit_2)
plot(x, residuals_2, ylab="Residuals", xlab="x", main="Resid vs. x without intercept")
abline(h=0, col="blue")
par(mfrow=c(2,2))
plot(lm.fit_2, main="Model without Intercept")

#The two linear models with normal intercept, and a 0 intercept fit the data very well.
#In fact, these two models fit the data better than the originally proposed y= 0.44x.
#The normal Q-Q plot for the normal linear model is much more symmetric than the QQ plot
#for the 0 intercept linear model, and the origial model. The intercept has a p-value of 
#0.00491, and a relatively low standard error of 0.01221. So, we can conclude that the 
#intercept term is moderately significant.



#c) Because of evaporation during the summer and the can being made of metal, 
#the formula y = 0.44x may not fit the rainfall data collected in the summer. 
#An argument can be made that supports the model with an intercept. Is this 
#supported by your analyses in a) and b)?

#Yes, the results for a) and b) support the model with an intercept. The
#residual plots for the model with an intercept is much more symmetric than
#the proposed 0.44x model and the new 0 intercept model. The intercept has a 
#p-value of 0.00491, and a relatively low standard error of 0.01221. So, we 
#can conclude that the intercept term is moderately significant.






### Question_02 Textbook Pg. 40, Problem 13:

#The data in Table 1.13 is from 1980 U.S. Census Undercount (Ericksen et al. 1989). 
#There are 66 rows and 10 columns. The first column is the place where the data is 
#collected. There are eight predictors:
#1. Minority: minority percentage.
#2. Crime: rate of serious crimes per 1000 population.
#3. Poverty: percentage poor.
#4. Language: percentage having difficulty speaking or writing English.
#5. Highschool: percentage age 25 or older who had not finished high school.
#6. Housing: percentage of housing in small, multi-unit buildings.
#7. City: a factor with two levels: “city” (major city), “state” (state remainder).
#8. Conventional: percentage of households counted by conventional personal enumeration.
#The response is undercount (in terms of percentage). Use regression to investigate the 
#relationship between undercount and the eight predictors.

#This line imports the above mentioned data from a .csv file into R workspace.
census <- read.csv("C:\\Users\\rajak\\Documents\\Spring_2021\\ISEN_616\\Homeworks\\HW_01/Problem_02_census.csv", sep="")

#This line examines the structure of the data in the new object "rain_data"
str(census)
census$City <- as.factor(census$City)
census$City
census$Minority
census$Crime
census$Poverty
census[31,]


#a) Perform regression analysis using all the predictors except city. Show the 
#regression residuals. Which predictors seem to be important? Draw the residual 
#plot against the fitted value. What can you conclude from this plot?

census.lm <- lm(census$Undercount~census$Minority+census$Crime+census$Poverty+census$Language+census$Highschool+census$Housing+census$Conventional, data=census)
summary(census.lm)
resid(census.lm)
par(mfrow=c(2,2))
plot(census.lm, main="Problem_2_Linear Model")

trial.lm <- lm(census$Undercount~., data = census[,!(names(census) %in% c('City','Place'))])
summary(trial.lm)
resid(trial.lm)
par(mfrow=c(2,2))
plot(census.lm, main="Problem_2_Linear Model")

#Language seems to be the most important predictor, followed by Highschool, and Crime.
#However, the standard error for Crime is comparable to it's coefficient value. Hence,
#we can not conclude that Crime is a reliable predictor. The residual vs. fitted plot
#shows a fairly symmetric pattern around the baseline. So, we can conclude that the
#linear model produced here fits the given dataset well.



#b) Explain how the variable “city” differs from the others.

class(census$City)
levels(census$City)
#The variable "City" is a factor with two levels "city" and "state", whie
#all other variables are either numeric/integer, or character variables.


#c) Use both best subset regression and stepwise regression to select variables 
#from all the predictors (excluding the variable “city”). Compare your final models 
#obtained by the two methods.
install.packages("leaps")
library(leaps)

##### Best Subset Selection

regfit.full = regsubsets(census$Undercount~.,data=census[,!(names(census) %in% c('City','Place'))], nvmax = 30, really.big = F)
#summary(regfit.full)
reg.summary = summary(regfit.full)
reg.summary

par(mfrow=c(2,2))
#Figure 1
plot(reg.summary$rss,xlab="Number of predictors",ylab="RSS",main="Best Subset Selection", type="l")
#Figure 2
plot(reg.summary$adjr2,xlab="Number of predictors",ylab="Adjusted RSq",main="Best Subset Selection",type="l")
which.max(reg.summary$adjr2)
#points(7,reg.summary$adjr2[7], col="red",cex=2,pch=20)
#Figure 3
plot(reg.summary$cp,xlab="Number of predictors",ylab="Cp",main="Best Subset Selection",type='l')
which.min(reg.summary$cp)
#points(5,reg.summary$cp[5],col="red",cex=2,pch=20)
#Figure 4
plot(reg.summary$bic,xlab="Number of predictors",ylab="BIC",main="Best Subset Selection",type='l')
which.min(reg.summary$bic)
#points(5,reg.summary$bic[5],col="red",cex=2,pch=20)
##print the coefficient estimates of the 8-predictor model
coef(regfit.full,6)
par(mfrow=c(1,1))


###Backwards Subset Selection
regfit.bwd=regsubsets(census$Undercount~.,data=census[,!(names(census) %in% c('City','Place'))],nvmax=30,method="backward")

#####Using CV Methods to select the best subset from all backward subsets.
#Validation Set CV: Randomly split data into a training set and a test set

set.seed(3)
train=sample(c(TRUE,FALSE), nrow(census), rep=TRUE)
test=(!train)

##Perform backward subset selection
census_dummy <-census[train,]
regfit.bwd=regsubsets(census_dummy$Undercount~.,data=census_dummy[,c((2:7),9)],nvmax=30,method="backward")

##building an "X" matrix from test data
census_dummy <-census[test,]
test.mat=model.matrix(census_dummy$Undercount~.,data=census_dummy[,c((2:7),9)])

###Compute test MSE of all the models
val.errors=rep(NA,6)
for(i in 1:6){
coefi=coef(regfit.bwd,id=i)
pred=test.mat[,names(coefi)]%*%coefi
val.errors[i]=mean((census$Undercount[test]-pred)^2)
}
val.errors

###Find the best model
min(val.errors)
which.min(val.errors)
coef(regfit.bwd,which.min(val.errors))


###Forward Subset Selection
regfit.bwd=regsubsets(census$Undercount~.,data=census[,!(names(census) %in% c('City','Place'))],nvmax=100,method="forward")

#####Using CV Methods to select the best subset from all forward subsets.
#Validation Set CV: Randomly split data into a training set and a test set

set.seed(3)
train=sample(c(TRUE,FALSE), nrow(census), rep=TRUE)
test=(!train)

##Perform backward subset selection
census_dummy <-census[train,]
regfit.bwd=regsubsets(census_dummy$Undercount~.,data=census_dummy[,c((2:7),9)],nvmax=100,method="forward")

##building an "X" matrix from test data
census_dummy <-census[test,]
test.mat=model.matrix(census_dummy$Undercount~.,data=census_dummy[,c((2:7),9)])

###Compute test MSE of all the models
val.errors=rep(NA,6)
for(i in 1:6){
coefi=coef(regfit.bwd,id=i)
pred=test.mat[,names(coefi)]%*%coefi
val.errors[i]=mean((census$Undercount[test]-pred)^2)
}
val.errors

###Find the best model
min(val.errors)
which.min(val.errors)
coef(regfit.bwd,which.min(val.errors))

#Stepwise subset selection methods choose a 3-predictor model with Language, Crime, and
#Conventional as the most important predictors. While the best subset selection method
#chose the 6-predictor model, with Language, Highschool, and Crime as the most important
#predictors, followed by Housing, Conventional, and Minority.

#Comparing these results to the multiple linear regression model, Language, Highschool, and
#Crime are the most commonly chosen "important predictors". Of these, Language emerges as the
#most dominant predictor across the board from all methods.

graphics.off()