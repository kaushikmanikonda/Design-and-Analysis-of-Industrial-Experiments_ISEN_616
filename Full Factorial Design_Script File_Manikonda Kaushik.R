#Script Title: ISEN_616_Assignment-04_Script File_Manikonda Kaushik
#Script Purpose: To satisfy the requirements for ISEN 616, Homework-04

#displays any existing objects in the workspace
ls()

#Step to check which liabraries are loaded
search()

#Removes all existing objects and variable assignments. 
#Will be commented out in the final homework to avoid repetition during future executions
rm(list=ls())
ls()


#This command directs all graphic output to a pdf file
pdf("C:\\Users\\rajak\\Documents\\Spring_2021\\ISEN_616\\Homeworks\\HW_04/HW04_output_File.pdf")

# The following line imports the data for problems 11, and 17 from .csv files
p11data <- read.csv("C:\\Users\\rajak\\Documents\\Spring_2021\\ISEN_616\\Homeworks\\HW_04\\P11_data.csv")
p17data <- read.csv("C:\\Users\\rajak\\Documents\\Spring_2021\\ISEN_616\\Homeworks\\HW_04\\P17_data.csv")


#Data Preparation for Problem #11.
str(p11data)
p11data$Flasher <- as.factor(p11data$Flasher)
p11data$lever_inertia <- as.factor(p11data$lever_inertia)
p11data$Task <- as.factor(p11data$Task)
str(p11data)

#Data Preparation for Problem #11.
str(p17data)


###Problem - 11

###A 23 experiment was performed by an industrial engineer and the results
#are given in Table 4.16. Half of the experiment was done on one day and
#the remaining half on the next day.

dummy <- p11data
attach(dummy)

#(a) Compute the main effect of task.
ME_task <- (mean(respense_time[Task=="Z"])-mean(respense_time[Task=="Y"]))
ME_task
detach(dummy)

#(b) Use a half-normal plot to detect significant factOlial effects. Compare
#your finding with those based on the IER version of Lenth's method at
#an appropriate a level.
p11data2 <- read.csv("C:\\Users\\rajak\\Documents\\Spring_2021\\ISEN_616\\Homeworks\\HW_04\\P11_2_data.csv")
str(p11data2)
p11data2$Flasher_A <- as.factor(p11data2$Flasher_A)
p11data2$lever_inertia_B <- as.factor(p11data2$lever_inertia_B)
p11data2$Task_C <- as.factor(p11data2$Task_C)
p11data2$AB <- as.factor(p11data2$AB)
p11data2$BC <- as.factor(p11data2$BC)
p11data2$AC <- as.factor(p11data2$AC)
p11data2$ABC <- as.factor(p11data2$ABC)
str(p11data2)
attach(p11data2)
ME_FlasherA <- (mean(respense_time[Flasher_A=="1"])-mean(respense_time[Flasher_A=="-1"]))
ME_leverB <- (mean(respense_time[lever_inertia_B=="1"])-mean(respense_time[lever_inertia_B=="-1"]))
ME_taskC <- (mean(respense_time[Task_C=="1"])-mean(respense_time[Task_C=="-1"]))
ME_AB <- (mean(respense_time[AB=="1"])-mean(respense_time[AB=="-1"]))
ME_BC <- (mean(respense_time[BC=="1"])-mean(respense_time[BC=="-1"]))
ME_AC <- (mean(respense_time[AC=="1"])-mean(respense_time[AC=="-1"]))
ME_ABC <- (mean(respense_time[ABC=="1"])-mean(respense_time[ABC=="-1"]))
detach(p11data2)
c1 <- c(ME_FlasherA,ME_leverB,ME_taskC,ME_AB,ME_BC,ME_AC,ME_ABC)
c1
c1 <- abs(c1)
c1 <- c1[order(c1)]
c1
c2 <- c(1:7)
c22 <- c("AC","lever","Flasher","AB","BC","ABC","Task")
c3 <- 0.5+(0.5*(c2-0.5)/7)
c3
c4 <- qnorm(c3)
c4 <- c4[order(c4)]
M1 <- data.frame(c4,c1)
M1
M2 <- cbind(c22,M1)
M2
M3 <- M2[,c(2,3)]
M3
plot(M3,ylab="Absolute Effects",xlab="Normal Quantiles")
text(M3,labels=M2$c22,pos=2)

### Task is the most important factor, and none of the interaction terms
# or other main effects even come close to Task.


#(c) With the timing mechanism operating properly, the standard deviation
#of the readings of average response time is known to be about 1 (q = 1).
#Subsequent to carrying out this experiment, however, it was discovered
#that the timing device, although it had been operating properly on the
#first day, had not been operating properly on the second day. On that
#second day, the standard deviation was q = 4. Given this information,
#what is the variance of the main effect of task computed in part (a)
#above?




###Problem - 17

###A metal alloy is used to make components for aircraft engines. Cracking
#is a potentially serious problem in the final part, as it can lead to a 
#nonrecoverable failure. The objective of the experiment was to identify the
#key factors and possibly their interactions which have effects on cracks.
#Four factors were considered: pouring temperature (A), titanium content
#(B), heat treatment method (C), and the amount of grain refiner used (0).
#A 24 experiment was conducted and the response of interest was the length
#of crack (in mmx 10-2). Each trial condition was replicated twice. The data
#are given in Table 4.1S.

#(a) Which main effects are significant for crack length? Are there any
#significant interactions?
dummy3 <- p17data
attach(dummy3)
ME_A <- (mean(avg_y[A==1])-mean(avg_y[A==-1]))
ME_B <- (mean(avg_y[B==1])-mean(avg_y[B==-1]))
ME_C <- (mean(avg_y[C==1])-mean(avg_y[C==-1]))
ME_D <- (mean(avg_y[D==1])-mean(avg_y[D==-1]))
ME_AB <- (mean(avg_y[AB==1])-mean(avg_y[AB==-1]))
ME_AC <- (mean(avg_y[AC==1])-mean(avg_y[AC==-1]))
ME_AD <- (mean(avg_y[AD==1])-mean(avg_y[AD==-1]))
ME_BC <- (mean(avg_y[BC==1])-mean(avg_y[BC==-1]))
ME_BD <- (mean(avg_y[BD==1])-mean(avg_y[BD==-1]))
ME_CD <- (mean(avg_y[CD==1])-mean(avg_y[CD==-1]))
ME_ABC <- (mean(avg_y[ABC==1])-mean(avg_y[ABC==-1]))
ME_ACD <- (mean(avg_y[ACD==1])-mean(avg_y[ACD==-1]))
ME_BCD <- (mean(avg_y[BCD==1])-mean(avg_y[BCD==-1]))
ME_ABD <- (mean(avg_y[ABD==1])-mean(avg_y[ABD==-1]))
ME_ABCD <- (mean(avg_y[ABCD==1])-mean(avg_y[ABCD==-1]))
detach(dummy3)

v1 <- c(ME_A,ME_B,ME_C,ME_D,ME_AB,ME_AC,ME_AD,ME_BC,ME_BD,ME_CD,ME_ABC,ME_ACD,ME_BCD,ME_ABD,ME_ABCD)
v1 <- abs(v1)
v2 <- v1[order(v1)]
v2
v3 <- c(1:15)
v4 <- 0.5+(0.5*(v3-0.5)/15)
v5 <- qnorm(v4)
v5 <- v5[order(v5)]
v6 <- c("A","B","C","D","AB","AC","AD","BC","BD","CD","ABC","ACD","BCD","ABD","ABCD")
v7 <- v6[order(v1)]
N1 <- data.frame(v7,v5,v2)
names(N1) <- c("Effect","normal_quantiles","absolute_effects")
N1

plot(N1$normal_quantiles,N1$absolute_effects,ylab="Absolute Effects",xlab="Normal Quantiles",main="Location Effects")
text(N1$normal_quantiles,N1$absolute_effects,labels=N1$Effect,pos=2)

###From the half-normal plot, B &C are the most significant main effects for crack length.
#Yes, BD, and BCD are the most important interactions.

attach(dummy3)
lvar_A <- (mean(lnS2[A==1])-mean(lnS2[A==-1]))
lvar_B <- (mean(lnS2[B==1])-mean(lnS2[B==-1]))
lvar_C <- (mean(lnS2[C==1])-mean(lnS2[C==-1]))
lvar_D <- (mean(lnS2[D==1])-mean(lnS2[D==-1]))
lvar_AB <- (mean(lnS2[AB==1])-mean(lnS2[AB==-1]))
lvar_AC <- (mean(lnS2[AC==1])-mean(lnS2[AC==-1]))
lvar_AD <- (mean(lnS2[AD==1])-mean(lnS2[AD==-1]))
lvar_BC <- (mean(lnS2[BC==1])-mean(lnS2[BC==-1]))
lvar_BD <- (mean(lnS2[BD==1])-mean(lnS2[BD==-1]))
lvar_CD <- (mean(lnS2[CD==1])-mean(lnS2[CD==-1]))
lvar_ABC <- (mean(lnS2[ABC==1])-mean(lnS2[ABC==-1]))
lvar_ACD <- (mean(lnS2[ACD==1])-mean(lnS2[ACD==-1]))
lvar_BCD <- (mean(lnS2[BCD==1])-mean(lnS2[BCD==-1]))
lvar_ABD <- (mean(lnS2[ABD==1])-mean(lnS2[ABD==-1]))
lvar_ABCD <- (mean(lnS2[ABCD==1])-mean(lnS2[ABCD==-1]))
detach(dummy3)

v8 <- c(lvar_A,lvar_B,lvar_C,lvar_D,lvar_AB,lvar_AC,lvar_AD,lvar_BC,lvar_BD,lvar_CD,lvar_ABC,lvar_ACD,lvar_BCD,lvar_ABD,lvar_ABCD)
v8 <- abs(v8)
v9 <- v8[order(v8)]
v9
v10 <- c("A","B","C","D","AB","AC","AD","BC","BD","CD","ABC","ACD","BCD","ABD","ABCD")
v11 <- v10[order(v8)]
log_variance <- v9
N2 <- data.frame(v11,v5,log_variance)
N2

#par(mfrow=c(2,1))

plot(N2$v5,N2$log_variance,ylab="Absolute Effects",xlab="Normal Quantiles",main="Dispersion Effects")
text(N2$v5,N2$log_variance,labels=N2$v11,pos=2)


#(b) What factors affect the variability of the crack length?

###From the dispersion effects plot, AB & CD affect the variability of the 
#crack length the most.


#(c) Find the optimum process conditions to minimize mean crack length.

##From the two half-normal plots, BD is significant for avg(y), and
# AB is significant for log(S2). So, BD is an adjustment factor. 

graphics.off()