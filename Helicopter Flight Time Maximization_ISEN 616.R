#Script Title: ISEN616_Project Script File_Kaushik Manikonda
#Script Purpose: To satisfy the requirements for ISEN 616, Final Project

#displays any existing objects in the workspace
ls()

#Step to check which liabraries are loaded
search()

#Removes all existing objects and variable assignments. 
#Will be commented out in the final homework to avoid repetition during future executions
rm(list=ls())
ls()


#This command directs all graphic output to a pdf file
#pdf("C:\\Users\\rajak\\Documents\\Spring_2021\\ISEN_616\\Project/ISEN616_output_File.pdf")

# The following line imports the model matrix and planning matrix data from .csv files
moddata <- read.csv("C:\\Users\\rajak\\Documents\\Spring_2021\\ISEN_616\\Project\\Model_Matrix.csv")
regdata <- read.csv("C:\\Users\\rajak\\Documents\\Spring_2021\\ISEN_616\\Project\\Planning_Matrix.csv")

str(moddata);str(regdata)
regdata
moddata


###Model Matrix Data Preparation for Half Normal Plot
str(moddata)
moddata$wl <- as.factor(moddata$wl)
moddata$ww <- as.factor(moddata$ww)
moddata$bl <- as.factor(moddata$bl)
moddata$bw <- as.factor(moddata$bw)
moddata$mbl <- as.factor(moddata$mbl)
moddata$ft <- as.factor(moddata$ft)
moddata$X7 <- as.factor(moddata$X7)
moddata$X8 <- as.factor(moddata$X8)
moddata$X9 <- as.factor(moddata$X9)
moddata$X10 <- as.factor(moddata$X10)
moddata$X11 <- as.factor(moddata$X11)
str(moddata)



### Half Normal Plots for main effects and interactions 7-11.
dummy <- moddata
str(dummy)
attach(dummy)

## Calculating the main effects and interactions for Location.

ME_wl <- (mean(Ravg[wl=="1"])-mean(Ravg[wl=="-1"]))
ME_ww <- (mean(Ravg[ww=="1"])-mean(Ravg[ww=="-1"]))
ME_bl <- (mean(Ravg[bl=="1"])-mean(Ravg[bl=="-1"]))
ME_bw <- (mean(Ravg[bw=="1"])-mean(Ravg[bw=="-1"]))
ME_mbl <- (mean(Ravg[mbl=="1"])-mean(Ravg[mbl=="-1"]))
ME_ft <- (mean(Ravg[ft=="1"])-mean(Ravg[ft=="-1"]))
ME_X7 <- (mean(Ravg[X7=="1"])-mean(Ravg[X7=="-1"]))
ME_X8 <- (mean(Ravg[X8=="1"])-mean(Ravg[X8=="-1"]))
ME_X9 <- (mean(Ravg[X9=="1"])-mean(Ravg[X9=="-1"]))
ME_X10 <- (mean(Ravg[X10=="1"])-mean(Ravg[X10=="-1"]))
ME_X11 <- (mean(Ravg[X11=="1"])-mean(Ravg[X11=="-1"]))
detach(dummy)


### Location Half Normla plot for all 11 factors (6 ME + 5 INT).

c1 <- c(ME_wl,ME_ww,ME_bl,ME_bw,ME_mbl,ME_ft,ME_X7,ME_X8,ME_X9,ME_X10,ME_X11)
c1a <- colnames(dummy)[2:12]
c1b <- data.frame(c1a,c1)
c1b

c2 <- abs(c1)
c2a <- c2[order(c2)]
c2a
c2b <- c1a[order(c2)]
c2b

c2c <- c(1:11)
c3 <- 0.5+(0.5*(c2c-0.5)/11)
c3
c4 <- qnorm(c3)
c4 <- c4[order(c4)]
M1 <- data.frame(c4,c2a,c2b)
M1

plot(M1[,c(1,2)],ylab="Absolute Effects",xlab="Normal Quantiles")
text(M1,labels=M1$c2b,pos=2)


### Location Half Normla plot for only the 6 main effects
v1 <- c(ME_wl,ME_ww,ME_bl,ME_bw,ME_mbl,ME_ft)
v1a <- colnames(dummy)[2:7]
v1b <- data.frame(v1a,v1)
v1b

v2 <- abs(v1)
v2a <- v2[order(v2)]
v2a
v2b <- v1a[order(v2)]
v2b

v2c <- c(1:6)
v3 <- 0.5+(0.5*(v2c-0.5)/6)
v3
v4 <- qnorm(v3)
v4 <- v4[order(v4)]
N1 <- data.frame(v4,v2a,v2b)
N1

plot(N1[,c(1,2)],ylab="Absolute Effects",xlab="Normal Quantiles")
text(N1,labels=N1$v2b,pos=2)



attach(dummy)

## Calculating the main effects and interactions for Dispersion.

ME_wl <- (mean(lns2[wl=="1"])-mean(lns2[wl=="-1"]))
ME_ww <- (mean(lns2[ww=="1"])-mean(lns2[ww=="-1"]))
ME_bl <- (mean(lns2[bl=="1"])-mean(lns2[bl=="-1"]))
ME_bw <- (mean(lns2[bw=="1"])-mean(lns2[bw=="-1"]))
ME_mbl <- (mean(lns2[mbl=="1"])-mean(lns2[mbl=="-1"]))
ME_ft <- (mean(lns2[ft=="1"])-mean(lns2[ft=="-1"]))
ME_X7 <- (mean(lns2[X7=="1"])-mean(lns2[X7=="-1"]))
ME_X8 <- (mean(lns2[X8=="1"])-mean(lns2[X8=="-1"]))
ME_X9 <- (mean(lns2[X9=="1"])-mean(lns2[X9=="-1"]))
ME_X10 <- (mean(lns2[X10=="1"])-mean(lns2[X10=="-1"]))
ME_X11 <- (mean(lns2[X11=="1"])-mean(lns2[X11=="-1"]))
detach(dummy)


### Dispersion Half Normla plot for all 11 factors (6 ME + 5 INT).

c1 <- c(ME_wl,ME_ww,ME_bl,ME_bw,ME_mbl,ME_ft,ME_X7,ME_X8,ME_X9,ME_X10,ME_X11)
c1a <- colnames(dummy)[2:12]
c1b <- data.frame(c1a,c1)
c1b

c2 <- abs(c1)
c2a <- c2[order(c2)]
c2a
c2b <- c1a[order(c2)]
c2b

c2c <- c(1:11)
c3 <- 0.5+(0.5*(c2c-0.5)/11)
c3
c4 <- qnorm(c3)
c4 <- c4[order(c4)]
M1 <- data.frame(c4,c2a,c2b)
M1

plot(M1[,c(1,2)],ylab="Absolute Effects",xlab="Normal Quantiles")
text(M1,labels=M1$c2b,pos=2)


### Dispersion Half Normla plot for only the 6 main effects
v1 <- c(ME_wl,ME_ww,ME_bl,ME_bw,ME_mbl,ME_ft)
v1a <- colnames(dummy)[2:7]
v1b <- data.frame(v1a,v1)
v1b

v2 <- abs(v1)
v2a <- v2[order(v2)]
v2a
v2b <- v1a[order(v2)]
v2b

v2c <- c(1:6)
v3 <- 0.5+(0.5*(v2c-0.5)/6)
v3
v4 <- qnorm(v3)
v4 <- v4[order(v4)]
N1 <- data.frame(v4,v2a,v2b)
N1

plot(N1[,c(1,2)],ylab="Absolute Effects",xlab="Normal Quantiles")
text(N1,labels=N1$v2b,pos=2)



### Hamada-Wu Analysis Strategy (Step-wise regression) for location (Ravg).
#install.packages("olsrr")
library("olsrr")
dummy2 <- regdata
dummy2
attach(dummy2)

## Step - 01 of Hamada - Wu

m1a <- lm(Ravg~wl+(wl*ww)+(wl*bl)+(wl*bw)+(wl*mbl)+(wl*ft),data=dummy2)
ols_step_both_p(m1a)
#Best model here includes wl,bl,wl*ft,& ww

m1b <- lm(Ravg~ww+(wl*ww)+(ww*bl)+(ww*bw)+(ww*mbl)+(ww*ft),data=dummy2)
ols_step_both_p(m1b)
#Best model here includes wl,ww*bl,& ft

m1c <- lm(Ravg~bl+(wl*bl)+(ww*bl)+(bl*bw)+(bl*mbl)+(bl*ft),data=dummy2)
ols_step_both_p(m1c)
#Best model here includes wl, bl*ww, & ft

m1d <- lm(Ravg~bw+(wl*bw)+(bw*bl)+(bw*ww)+(bw*mbl)+(bw*ft),data=dummy2)
ols_step_both_p(m1d)
#Best model here includes wl & bl

m1e <- lm(Ravg~mbl+(wl*mbl)+(mbl*bl)+(mbl*bw)+(ww*mbl)+(mbl*ft),data=dummy2)
ols_step_both_p(m1e)
#Best model here includes wl, bl, mbl*ft, & ww

m1f <- lm(Ravg~ft+(wl*ft)+(ft*bl)+(ft*bw)+(ft*mbl)+(ww*ft),data=dummy2)
ols_step_both_p(m1f)
#Best model here includes wl, bl, ft*mbl, ft*bw.


### The comprehensive list of 2-fi for step-02 includes wl*ft, ww*bl,
### mbl*ft, ft*bw.
### Significant ME from Step-01 include wl,bl,ft,ww

## Step - 02 of Hamada - Wu
m2a <- lm(Ravg~wl+ww+bl+bw+mbl+ft+wl*ft+ww*bl+mbl*ft+ft*bw,data=dummy2)
ols_step_both_p(m2a)

### Step-02 identified wl, bw, ww*bl, ft*bw, & mbl*ft as the significant effects.

## Step - 03 of Hamada - Wu
m3a <- lm(Ravg~wl+bw+ww*bl+ft*bw+mbl*ft+wl*ww+wl*bl+wl*bw+wl*mbl+ww*ft+mbl*ww+ww*bw+bl*mbl+bl*bw+bl*ft,data=dummy2)
ols_step_both_p(m3a)

### Step-03 identified wl,bw, ww*bl, ft*mbl, wl*bw as the important factors.



### Iteration -02 Steps 02 & 03

## Step - 02 of Hamada - Wu
m2b <- lm(Ravg~wl+ww+bl+bw+mbl+ft+ww*bl+mbl*ft+wl*bw,data=dummy2)
ols_step_both_p(m2b,details=TRUE)

### Step-02 identified wl, bw, ww*bl, wl*bw, & mbl*ft as the significant effects.

## Step - 03 of Hamada - Wu
m3b <- lm(Ravg~wl+bw+ww*bl+wl*bw+mbl*ft+wl*ww+wl*bl+wl*mbl+ww*bw+bl*bw+bw*ft+bw*mbl,data=dummy2)
ols_step_both_p(m3b,details=TRUE)

### Step-03 in Iteration-02 identified wl,ft, ww*bl, mbl as the important factors.



### Iteration -03 Steps 02 & 03

## Step - 02 of Hamada - Wu
m2b <- lm(Ravg~wl+ww+bl+bw+mbl+ft+ww*bl,data=dummy2)
ols_step_both_p(m2b,details=TRUE)

### Step-02 identified wl, ww*bl, ft, & mbl as the significant effects.

## Step - 03 of Hamada - Wu
m3b <- lm(Ravg~wl+ft+mbl+ww*bl+wl*bw+wl*mbl+wl*ft+wl*ww+ft*ww+ft*bl+ft*bw+ft*mbl+mbl*bl+mbl*bw+mbl*ww+mbl*ft,data=dummy2)
ols_step_both_p(m3b,details=TRUE)

### Step-03 in Iteration-02 identified wl,ft, ww*bl, mbl as the important factors.

### The model stops changing here, so I am stopping iterations betweeb 2&3 here.
### The final list of significant factors is wl, ft, mbl, ww*bl.
### The final model is yavg = 2.230 + 0.188wl - 0.079ft - 0.151mbl - 0.062 ww*bl
### So, the ideal factor levels for flight time maximization are 
### wing length (wl) = 4.5, No folded tip (ft = -1), middle body lengt = 1 inch
### mbl -ve. And finally any combination of ww and bl such that ww * bl is -ve.
### Validation: The helicopter made with these above specifications had the 
### following flight times y1 = 2.90 sec, y2 = 2.77 sec, and y3 = 2.78



### Hamada-Wu Analysis Strategy (Step-wise regression) for dispersion (lns2).
#install.packages("olsrr")
library("olsrr")
dummy2 <- regdata
dummy2
attach(dummy2)

## Step - 01 of Hamada - Wu

m1a <- lm(lns2~wl+(wl*ww)+(wl*bl)+(wl*bw)+(wl*mbl)+(wl*ft),data=dummy2)
ols_step_both_p(m1a)
#Best model here includes wl*bw,bl, & ft.

m1b <- lm(lns2~ww+(wl*ww)+(ww*bl)+(ww*bw)+(ww*mbl)+(ww*ft),data=dummy2)
ols_step_both_p(m1b)
#Best model here includes bl

m1c <- lm(lns2~bl+(wl*bl)+(ww*bl)+(bl*bw)+(bl*mbl)+(bl*ft),data=dummy2)
ols_step_both_p(m1c)
#Best model here includes bl*bw, & ft

m1d <- lm(lns2~bw+(wl*bw)+(bw*bl)+(bw*ww)+(bw*mbl)+(bw*ft),data=dummy2)
ols_step_both_p(m1d)
#Best model here includes bw*wl, bl, ft

m1e <- lm(lns2~mbl+(wl*mbl)+(mbl*bl)+(mbl*bw)+(ww*mbl)+(mbl*ft),data=dummy2)
ols_step_both_p(m1e)
#Best model here includes bl

m1f <- lm(lns2~ft+(wl*ft)+(ft*bl)+(ft*bw)+(ft*mbl)+(ww*ft),data=dummy2)
ols_step_both_p(m1f)
#Best model here includes bl

### The comprehensive list of 2-fi for step-02 includes wl*bw, & bl*bw
### Significant ME from Step-01 include bl,ft

## Step - 02 of Hamada - Wu
m2a <- lm(lns2~wl+ww+bl+bw+mbl+ft+bl*wl+bl*bw+bl*ww+bl*ft+bl*mbl+ft*wl+ft*bw+ft*mbl+ft*ww,data=dummy2)
ols_step_both_p(m2a,details=TRUE)

### Step-02 identified ft, bl*bw as the significant effects.

## Step - 03 of Hamada - Wu
m3a <- lm(lns2~ft+bl+ww*bl+ft*bw+mbl*ft+wl*bl+ww*ft+bl*mbl+bl*bw+bl*ft+ft*wl,data=dummy2)
ols_step_both_p(m3a,details=TRUE)

### Step-03 identified ft, bl*bw as the important factors.



### Iteration -02 Steps 02 & 03

## Step - 02 of Hamada - Wu
m2b <- lm(lns2~wl+ww+bl+bw+mbl+ft+ww*bl+mbl*ft+wl*bw,data=dummy2)
ols_step_both_p(m2b,details=TRUE)

### Step-02 identified wl*bw, bl, ft as the significant effects.

## Step - 03 of Hamada - Wu
m3a <- lm(lns2~ft+bl+ww*bl+ft*bw+mbl*ft+wl*bl+ww*ft+bl*mbl+bl*bw+bl*ft+ft*wl,data=dummy2)
ols_step_both_p(m3a,details=TRUE)

### Step-03 identified ft, bl*bw as the important factors.

### The regression model for dispersion is
### lns2 = -7.560 +0.246 ft + 0.282 bl*bw

graphics.off()