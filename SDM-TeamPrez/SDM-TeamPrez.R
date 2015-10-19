data <- read.csv("~/Studies/Courses/USF/Statistical Data Mining/In-Class Presentation/2.WeeklyTeamPrez new/2.WeeklyTeamPrez/airline data.csv")
View(airline.data)

plot(data)

install.packages("car")
library(car)

names(data)


#Explanatory Models
#Why some routs have higher Fare?

scatterplot.matrix(FARE~S_CITY+E_CITY+COUPON+NEW+VACATION+DISTANCE|FARE,data=data)
plot(FARE~S_CITY+E_CITY+COUPON+NEW+VACATION+DISTANCE,data=data)
pairs(FARE~S_CITY+E_CITY+COUPON+NEW+VACATION+DISTANCE,data=data)
scatterplot.matrix(FARE~COUPON+VACATION+DISTANCE|FARE,data=data)

install.packages("FactoMineR")
library(FactoMineR)
pca<-PCA(data[,-1:-4])
pca$var

boxplot(data[],notch = T,horizontal = T)

install.packages("corrplot")
library(corrplot)
cor(data[,-1:-4])
corrplot(cor(data[,-1:-4]),method="circle")
corrplot(cor(data[,-1:-4]),method="ellipse")
corrplot.mixed(cor(data[,-1:-4]),lower="ellipse",upper="number")
##Correlation Matrix Plots is an effective technique visually show the correlations between Variables

par(mar=c(4,4,0,0))
plot(as.factor(data[,"E_CITY"]))
plot(data[,"S_CITY"],data[,"FARE"])
plot(data[,"DISTANCE"],data[,"FARE"])
plot(data[,"S_POP"],data[,"FARE"])
plot(data[,"E_POP"],data[,"FARE"])
plot(data[,"COUPON"],data[,"FARE"])
boxplot(data[,"COUPON"]~data[,"SW"]) #No apparent difference between SW~COUPON
plot(data[,"VACATION"],data[,"FARE"])
boxplot(data[,"FARE"]~data[,"VACATION"],notch=T) #Vacation Destinations have lower avg fare
boxplot(data[,"FARE"]~data[,"SW"],notch=T) #South West serviced locations have lower avg fares
plot(data[,"DISTANCE"],data[,"SW"])
boxplot(data[,"DISTANCE"]~data[,"SW"],notch=T) #South West serviced locations have lower avg distance
boxplot(data[,"COUPON"]~data[,"SW"])

########
#T-Tests
t.test <- with(data,t.test(as.numeric(FARE),as.numeric(DISTANCE)))
t.test

t.test2 <- with(data,t.test(as.numeric(FARE)~as.factor(SW)))
t.test2

t.test3 <- with(data,t.test(as.numeric(FARE),as.numeric(COUPON)))
t.test3

t.test4 <- with(data,t.test(as.numeric(FARE),as.numeric(NEW)))
t.test4

#######
#ANOVA
aova <- with(data,aov(as.numeric(FARE)~as.numeric(DISTANCE)))
summary(anova)
plot(anova)

anova1 <- with(data,aov(as.numeric(FARE)~as.numeric(DISTANCE)+as.factor(SW))) #Blocked on SW
summary(anova1)
plot(anova1)

anova2 <- with(data,aov(as.numeric(FARE)~as.numeric(COUPON)+as.factor(SW))) #Blocked on SW
summary(anova2)
plot(anova2)


#MANOVA
#ANCOVA
#MANCOVA
########################################################
#Linear Regression (Variable Selection, Model Selection)
lm.fit <- lm(as.numeric(FARE)~as.factor(S_CODE)+as.factor(S_CITY)+as.factor(E_CODE)+as.factor(E_CITY)+as.numeric(COUPON)+as.factor(NEW)+as.factor(VACATION)+as.factor(SW)+as.numeric(HI)+as.numeric(S_INCOME)+as.numeric(E_INCOME)+as.numeric(S_POP)+as.numeric(E_POP)+as.factor(SLOT)+as.factor(GATE)+as.numeric(DISTANCE)+as.numeric(PAX),data=data)
summary(lm.fit) #R2 86.21%
plot(lm.fit)
influencePlot(lm.fit)
library(MASS)
step <- stepAIC(lm.fit,direction = "both") #AIC=4363.76****
step2 <- stepAIC(lm.fit,direction = "forward") #AIC=4382.22
step2 <- stepAIC(lm.fit,direction = "backward") #AIC=4363.76****



      ##New Model with highest AIC variables
lm.fit2 <- lm(as.numeric(FARE) ~ as.factor(S_CITY) + as.factor(E_CITY) + as.numeric(COUPON) + 
                as.factor(NEW) + as.factor(VACATION) + as.factor(SW) + as.numeric(HI) + 
                as.numeric(DISTANCE) + as.numeric(PAX) ,data=data)
summary(lm.fit2)#R2 86.45% **not much increase
plot(lm.fit2)
influencePlot(lm.fit2)
AIC(lm.fit2) #6176.324




#############################################################
#Linear Regression (Interaction Terms and Stepwise Selection)
##Visualizations
library(ggplot2)
##Distance and SW interaction
gp <- ggplot(data=data, aes(x=DISTANCE, y=FARE, colour=factor(SW))) 
gp + geom_point() + stat_smooth(method="lm")
##Distance and SW interaction
gp <- ggplot(data=data, aes(x=DISTANCE, y=FARE*FARE, colour=factor(SW))) 
gp + geom_point() + stat_smooth(method="lm")
##Distance and SW interaction
gp <- ggplot(data=data, aes(x=DISTANCE, y=log(FARE), colour=factor(SW))) 
gp + geom_point() + stat_smooth(method="lm")
##Coupon and SW interaction
gp <- ggplot(data=data, aes(x=COUPON, y=FARE, colour=factor(SW))) 
gp + geom_point() + stat_smooth(method="lm")
##Coupon and SW interaction
gp <- ggplot(data=data, aes(x=DISTANCE, y=FARE, colour=factor(VACATION))) 
gp + geom_point() + stat_smooth(method="lm")

##All interaction Terms!!!
ilm.fit <- lm(FARE~.+.*.,data=data)
summary(ilm.fit) #R2 100% hahaha
summary(ilm.fit)$coefficients

##Distance & Coupon
ilm.fit2 <- lm(as.numeric(FARE) ~ as.factor(S_CODE) + as.factor(S_CITY) + as.factor(E_CODE) + 
                 as.factor(E_CITY) + as.numeric(COUPON) + as.factor(NEW) + as.factor(VACATION) + 
                 as.factor(SW) + as.numeric(HI) + as.numeric(S_INCOME) + as.numeric(E_INCOME) + 
                 as.numeric(S_POP) + as.numeric(E_POP) + as.factor(SLOT) + as.factor(GATE) + 
                 as.numeric(DISTANCE) + as.numeric(PAX) + (as.numeric(DISTANCE) * as.numeric(COUPON)), 
               data=data)
summary(ilm.fit2) #R2 86.60% increase

##DISTANCE * SW
ilm.fit3 <- lm(as.numeric(FARE) ~ as.factor(S_CODE) + as.factor(S_CITY) + as.factor(E_CODE) + 
                 as.factor(E_CITY) + as.numeric(COUPON) + as.factor(NEW) + as.factor(VACATION) + 
                 as.factor(SW) + as.numeric(HI) + as.numeric(S_INCOME) + as.numeric(E_INCOME) + 
                 as.numeric(S_POP) + as.numeric(E_POP) + as.factor(SLOT) + as.factor(GATE) + 
                 as.numeric(DISTANCE) + as.numeric(PAX) + (as.numeric(DISTANCE) * as.numeric(COUPON)) +
                 (as.numeric(DISTANCE)*as.factor(SW)), 
               data=data)
summary(ilm.fit3) #R2 86.64% increase


##NEW * SW
ilm.fit4 <- lm(as.numeric(FARE) ~ as.factor(S_CODE) + as.factor(S_CITY) + as.factor(E_CODE) + 
                 as.factor(E_CITY) + as.numeric(COUPON) + as.factor(NEW) + as.factor(VACATION) + 
                 as.factor(SW) + as.numeric(HI) + as.numeric(S_INCOME) + as.numeric(E_INCOME) + 
                 as.numeric(S_POP) + as.numeric(E_POP) + as.factor(SLOT) + as.factor(GATE) + 
                 as.numeric(DISTANCE) + as.numeric(PAX) + (as.numeric(DISTANCE) * as.numeric(COUPON)) +
                 (as.numeric(DISTANCE)*as.factor(SW)) + (as.factor(NEW)*as.factor(SW)),
               data=data)
summary(ilm.fit4) #R2 86.71% increase
plot(ilm.fit4) # good fit
AIC(ilm.fit4) #6174.615

## Adding all relevant interaction terms to the model
## Potential interaction variables are DISTANCE, SLOT, HI, SW, VACATION, NEW, COUPON
## Finding all good interactions by using stepwise selection on AIC by individual variables
ilm.fit5 <- lm(as.numeric(FARE)~as.numeric(DISTANCE)*.,data=data)
plot(ilm.fit5) #good fit
AIC(ilm.fit5) #5931
stepAIC(ilm.fit5, direction = "both") #AIC 4081.87

ilm.fit6 <- lm(as.numeric(FARE)~as.factor(SLOT)*.,data=data)
plot(ilm.fit6) #good fit
stepAIC(ilm.fit6, direction = "both") #AIC 4358.43

ilm.fit6 <- lm(as.numeric(FARE)~as.numeric(HI)*.,data=data)
plot(ilm.fit6) #good fit
stepAIC(ilm.fit6, direction = "both") #AIC 4273.05

ilm.fit6 <- lm(as.numeric(FARE)~as.numeric(SW)*.,data=data)
plot(ilm.fit6) 
stepAIC(ilm.fit6, direction = "both") #AIC 4155.2

ilm.fit6 <- lm(as.numeric(FARE)~as.numeric(VACATION)*.,data=data)
plot(ilm.fit6) #good fit
stepAIC(ilm.fit6, direction = "both") #AIC 4178.12

ilm.fit6 <- lm(as.numeric(FARE)~as.numeric(NEW)*.,data=data)
plot(ilm.fit6)
stepAIC(ilm.fit6, direction = "both") #AIC 4355.1

ilm.fit6 <- lm(as.numeric(FARE)~as.numeric(COUPON)*.,data=data)
plot(ilm.fit6) #good fit
stepAIC(ilm.fit6, direction = "both") #AIC 4188.3

ilm.fit7 <- lm(as.numeric(FARE) ~ as.factor(S_CODE) + as.factor(S_CITY) + as.factor(E_CODE) + 
                 as.factor(E_CITY) + as.numeric(COUPON) + as.factor(NEW) + as.factor(VACATION) + 
                 as.factor(SW) + as.numeric(HI) + as.numeric(S_INCOME) + as.numeric(E_INCOME) + 
                 as.numeric(S_POP) + as.numeric(E_POP) + as.factor(SLOT) + as.factor(GATE) + 
                 as.numeric(DISTANCE) + as.numeric(PAX) + 
                 as.numeric(DISTANCE):S_CITY + as.numeric(DISTANCE):E_CITY + 
                 as.numeric(DISTANCE):COUPON + as.numeric(DISTANCE):HI + as.numeric(DISTANCE):PAX + 
                 as.numeric(SLOT):PAX + as.numeric(SLOT):DISTANCE + as.numeric(SLOT):VACATION +
                 as.numeric(HI):S_CITY + as.numeric(HI):E_CITY + as.numeric(HI):VACATION + as.numeric(HI):SW + 
                 as.numeric(HI):HI + as.numeric(HI):PAX +
                 as.numeric(SW):S_CITY + as.numeric(SW):E_CITY + as.numeric(SW):COUPON + as.numeric(SW):VACATION + 
                 as.numeric(SW):DISTANCE + as.numeric(SW):PAX +
                 as.numeric(VACATION):S_CITY + as.numeric(VACATION):E_CODE + as.numeric(VACATION):COUPON + 
                 as.numeric(VACATION):SW + as.numeric(VACATION):HI + as.numeric(VACATION):DISTANCE + 
                 as.numeric(VACATION):PAX +
                 as.numeric(NEW):S_CODE + as.numeric(NEW):HI + as.numeric(NEW):PAX + as.numeric(NEW):VACATION +
                 as.numeric(COUPON):S_CITY + as.numeric(COUPON):E_CITY + as.numeric(COUPON):VACATION + 
                 as.numeric(COUPON):SW + as.numeric(COUPON):HI + as.numeric(COUPON):DISTANCE, data=data)
summary(ilm.fit7) #R2 98.91% !!
plot(ilm.fit7)
AIC(ilm.fit7) #AIC 4578.465

#interaction terms + squared variables
istep1 <- stepAIC(ilm.fit7,direction = "both") #AIC 2725.83
ilm.fit8 <- lm(as.numeric(FARE) ~ as.factor(S_CITY) + as.factor(E_CITY) + as.numeric(COUPON) + 
                 as.factor(NEW) + as.numeric(HI) + as.numeric(DISTANCE) + 
                 as.numeric(PAX) + as.numeric(DISTANCE):S_CITY + as.numeric(DISTANCE):E_CITY + 
                 as.numeric(DISTANCE):HI + as.numeric(DISTANCE):PAX + as.numeric(HI):HI + 
                 as.numeric(HI):PAX + as.numeric(HI):S_CITY + as.numeric(HI):E_CITY + 
                 as.numeric(HI):VACATION + S_CITY:as.numeric(SW) + E_CITY:as.numeric(SW) + 
                 COUPON:as.numeric(SW) + VACATION:as.numeric(SW) + as.numeric(SW):DISTANCE + 
                 S_CITY:as.numeric(VACATION) + COUPON:as.numeric(VACATION) + 
                 DISTANCE:as.numeric(VACATION) + PAX:as.numeric(VACATION) + 
                 HI:as.numeric(NEW) + as.numeric(COUPON):S_CITY + as.numeric(COUPON):E_CITY + 
                 as.numeric(COUPON):HI,
               data = data)
summary(ilm.fit8) #R2 0.9903
plot(ilm.fit8)
AIC(ilm.fit8) #4538.392

#interaction terms + squared variables + Log(FARE)
ilm.fit8 <- lm(as.numeric(log(FARE)) ~ as.factor(S_CITY) + as.factor(E_CITY) + as.numeric(COUPON) + 
                 as.factor(NEW) + as.numeric(HI) + as.numeric(DISTANCE) + 
                 as.numeric(PAX) + as.numeric(DISTANCE):S_CITY + as.numeric(DISTANCE):E_CITY + 
                 as.numeric(DISTANCE):HI + as.numeric(DISTANCE):PAX + as.numeric(HI):HI + 
                 as.numeric(HI):PAX + as.numeric(HI):S_CITY + as.numeric(HI):E_CITY + 
                 as.numeric(HI):VACATION + S_CITY:as.numeric(SW) + E_CITY:as.numeric(SW) + 
                 COUPON:as.numeric(SW) + VACATION:as.numeric(SW) + as.numeric(SW):DISTANCE + 
                 S_CITY:as.numeric(VACATION) + COUPON:as.numeric(VACATION) + 
                 DISTANCE:as.numeric(VACATION) + PAX:as.numeric(VACATION) + 
                 HI:as.numeric(NEW) + as.numeric(COUPON):S_CITY + as.numeric(COUPON):E_CITY + 
                 as.numeric(COUPON):HI,
               data = data)
summary(ilm.fit8) #R2 0.9912 higher
plot(ilm.fit8)
AIC(ilm.fit8) #-1931.995

#interaction terms
ilm.fit8 <- lm(as.numeric(FARE) ~ as.factor(S_CITY) + as.factor(E_CITY) + as.numeric(COUPON) + 
                 as.factor(NEW) + as.numeric(HI) + as.numeric(DISTANCE) + 
                 as.numeric(PAX) + 
                 as.numeric(DISTANCE):S_CITY + as.numeric(DISTANCE):E_CITY + 
                 as.numeric(DISTANCE):HI + as.numeric(DISTANCE):PAX + 
                 as.numeric(HI):PAX + as.numeric(HI):S_CITY + as.numeric(HI):E_CITY + 
                 as.numeric(HI):VACATION + 
                 S_CITY:as.numeric(SW) + E_CITY:as.numeric(SW) + 
                 COUPON:as.numeric(SW) + VACATION:as.numeric(SW) + as.numeric(SW):DISTANCE + 
                 S_CITY:as.numeric(VACATION) + COUPON:as.numeric(VACATION) + 
                 DISTANCE:as.numeric(VACATION) + PAX:as.numeric(VACATION) + 
                 HI:as.numeric(NEW) + 
                 as.numeric(COUPON):S_CITY + as.numeric(COUPON):E_CITY + 
                 as.numeric(COUPON):HI,
               data = data)
summary(ilm.fit8) #R2 0.9901
plot(ilm.fit8)
AIC(ilm.fit8) #4553.261 Lower

#Removing Sw and NEW interaction terms
ilm.fit8 <- lm(as.numeric(FARE) ~ as.factor(S_CITY) + as.factor(E_CITY) + as.numeric(COUPON) + 
                 as.factor(NEW) + as.numeric(HI) + as.numeric(DISTANCE) + 
                 as.numeric(PAX) + 
                 as.numeric(DISTANCE):S_CITY + as.numeric(DISTANCE):E_CITY + 
                 as.numeric(DISTANCE):HI + as.numeric(DISTANCE):PAX + 
                 as.numeric(HI):PAX + as.numeric(HI):S_CITY + as.numeric(HI):E_CITY + 
                 as.numeric(HI):VACATION + 
                 #S_CITY:as.numeric(SW) + E_CITY:as.numeric(SW) + 
                 #COUPON:as.numeric(SW) + VACATION:as.numeric(SW) + as.numeric(SW):DISTANCE + 
                 S_CITY:as.numeric(VACATION) + COUPON:as.numeric(VACATION) + 
                 DISTANCE:as.numeric(VACATION) + PAX:as.numeric(VACATION) + 
                 #HI:as.numeric(NEW) + 
                 as.numeric(COUPON):S_CITY + as.numeric(COUPON):E_CITY + 
                 as.numeric(COUPON):HI,
               data = data)
summary(ilm.fit8) #R2 0.96.86
plot(ilm.fit8)
AIC(ilm.fit8) #5314.338 HIgher

#Removing SW NEW interaction terms and adding log FARE
ilm.fit8 <- lm(as.numeric(log(FARE)) ~ as.factor(S_CITY) + as.factor(E_CITY) + as.numeric(COUPON) + 
                 as.factor(NEW) + as.numeric(HI) + as.numeric(DISTANCE) + 
                 as.numeric(PAX) + 
                 as.numeric(DISTANCE):S_CITY + as.numeric(DISTANCE):E_CITY + 
                 as.numeric(DISTANCE):HI + as.numeric(DISTANCE):PAX + 
                 as.numeric(HI):PAX + as.numeric(HI):S_CITY + as.numeric(HI):E_CITY + 
                 as.numeric(HI):VACATION + 
                 #S_CITY:as.numeric(SW) + E_CITY:as.numeric(SW) + 
                 #COUPON:as.numeric(SW) + VACATION:as.numeric(SW) + as.numeric(SW):DISTANCE + 
                 S_CITY:as.numeric(VACATION) + COUPON:as.numeric(VACATION) + 
                 DISTANCE:as.numeric(VACATION) + PAX:as.numeric(VACATION) + 
                 #HI:as.numeric(NEW) + 
                 as.numeric(COUPON):S_CITY + as.numeric(COUPON):E_CITY + 
                 as.numeric(COUPON):HI,
               data = data)
summary(ilm.fit8) #R2 0.97
plot(ilm.fit8)
AIC(ilm.fit8) #-1125 Higher

#adding log COUPON 
ilm.fit8 <- lm(as.numeric((FARE)) ~ as.factor(S_CITY) + as.factor(E_CITY) + log(as.numeric(COUPON)) + 
                 as.factor(NEW) + as.numeric(HI) + as.numeric(DISTANCE) + 
                 as.numeric(PAX) + 
                 as.numeric(DISTANCE):S_CITY + as.numeric(DISTANCE):E_CITY + 
                 as.numeric(DISTANCE):HI + as.numeric(DISTANCE):PAX + 
                 as.numeric(HI):PAX + as.numeric(HI):S_CITY + as.numeric(HI):E_CITY + 
                 as.numeric(HI):VACATION + 
                 S_CITY:as.numeric(SW) + E_CITY:as.numeric(SW) + 
                 COUPON:as.numeric(SW) + VACATION:as.numeric(SW) + as.numeric(SW):DISTANCE + 
                 S_CITY:as.numeric(VACATION) + COUPON:as.numeric(VACATION) + 
                 DISTANCE:as.numeric(VACATION) + PAX:as.numeric(VACATION) + 
                 HI:as.numeric(NEW) + 
                 as.numeric(COUPON):S_CITY + as.numeric(COUPON):E_CITY + 
                 as.numeric(COUPON):HI,
               data = data)
summary(ilm.fit8) #R2 0.9903
plot(ilm.fit8)
AIC(ilm.fit8) #4534.745 Lowest


#Removing All CITY interaction variables
ilm.fit8 <- lm(as.numeric((FARE)) ~ as.factor(S_CITY) + as.factor(E_CITY) + log(as.numeric(COUPON)) + 
                 as.factor(NEW) + as.numeric(HI) + as.numeric(DISTANCE) + 
                 as.numeric(PAX) + 
                 #as.numeric(DISTANCE):S_CITY + as.numeric(DISTANCE):E_CITY + 
                 as.numeric(DISTANCE):HI + as.numeric(DISTANCE):PAX + 
                 as.numeric(HI):PAX + 
                 #as.numeric(HI):S_CITY + as.numeric(HI):E_CITY + 
                 as.numeric(HI):VACATION + 
                 #S_CITY:as.numeric(SW) + E_CITY:as.numeric(SW) + 
                 COUPON:as.numeric(SW) + VACATION:as.numeric(SW) + as.numeric(SW):DISTANCE + 
                 S_CITY:as.numeric(VACATION) + COUPON:as.numeric(VACATION) + 
                 DISTANCE:as.numeric(VACATION) + PAX:as.numeric(VACATION) + 
                 HI:as.numeric(NEW) + 
                 #as.numeric(COUPON):S_CITY + as.numeric(COUPON):E_CITY + 
                 as.numeric(COUPON):HI,
               data = data)
summary(ilm.fit8) #R2 0.9123
plot(ilm.fit8) #Very good
AIC(ilm.fit8) #5925.274 Lowest of Good fit till now!

#Adding S_CITY one by one
ilm.fit8 <- lm(as.numeric((FARE)) ~ as.factor(S_CITY) + as.factor(E_CITY) + log(as.numeric(COUPON)) + 
                 as.factor(NEW) + as.numeric(HI) + as.numeric(DISTANCE) + 
                 as.numeric(PAX) + 
                 #as.numeric(DISTANCE):S_CITY + as.numeric(DISTANCE):E_CITY + 
                 as.numeric(DISTANCE):HI + as.numeric(DISTANCE):PAX + 
                 as.numeric(HI):PAX + 
                 #as.numeric(HI):S_CITY + as.numeric(HI):E_CITY + 
                 as.numeric(HI):VACATION + 
                 #S_CITY:as.numeric(SW) + E_CITY:as.numeric(SW) + 
                 COUPON:as.numeric(SW) + VACATION:as.numeric(SW) + as.numeric(SW):DISTANCE + 
                 S_CITY:as.numeric(VACATION) + COUPON:as.numeric(VACATION) + 
                 DISTANCE:as.numeric(VACATION) + PAX:as.numeric(VACATION) + 
                 HI:as.numeric(NEW) + 
                 as.numeric(COUPON):S_CITY + as.numeric(COUPON):E_CITY + 
                 as.numeric(COUPON):HI,
               data = data)
summary(ilm.fit8) #R2 0.9123
plot(ilm.fit8)
AIC(ilm.fit8) #5925.274 Lower

#Exponential Models
ilm.fit8 <- lm(as.numeric(log(FARE)) ~ as.factor(S_CITY) + as.factor(E_CITY) + (as.numeric(COUPON)) + 
                 as.factor(NEW) + as.numeric(HI) + as.numeric(DISTANCE) + 
                 as.numeric(PAX) + 
                 #as.numeric(DISTANCE):S_CITY + as.numeric(DISTANCE):E_CITY + 
                 as.numeric(DISTANCE):HI + as.numeric(DISTANCE):PAX + 
                 as.numeric(HI):PAX + 
                 #as.numeric(HI):S_CITY + as.numeric(HI):E_CITY + 
                 as.numeric(HI):VACATION + 
                 #S_CITY:as.numeric(SW) + E_CITY:as.numeric(SW) + 
                 COUPON:as.numeric(SW) + VACATION:as.numeric(SW) + as.numeric(SW):DISTANCE + 
                 S_CITY:as.numeric(VACATION) + COUPON:as.numeric(VACATION) + 
                 DISTANCE:as.numeric(VACATION) + PAX:as.numeric(VACATION) + 
                 HI:as.numeric(NEW) + 
                 #as.numeric(COUPON):S_CITY + as.numeric(COUPON):E_CITY + 
                 as.numeric(COUPON):HI,
               data = data)
summary(ilm.fit8) #R2 0.9328 Higher
plot(ilm.fit8) #Very good
AIC(ilm.fit8) #-655.0511 Low

#Exponential Models with log COUPON
ilm.fit8 <- lm(as.numeric(log(FARE)) ~ as.factor(S_CITY) + as.factor(E_CITY) + log(as.numeric(COUPON)) + 
                 as.factor(NEW) + as.numeric(HI) + as.numeric(DISTANCE) + 
                 as.numeric(PAX) + 
                 #as.numeric(DISTANCE):S_CITY + as.numeric(DISTANCE):E_CITY + 
                 as.numeric(DISTANCE):HI + as.numeric(DISTANCE):PAX + 
                 as.numeric(HI):PAX + 
                 #as.numeric(HI):S_CITY + as.numeric(HI):E_CITY + 
                 as.numeric(HI):VACATION + 
                 #S_CITY:as.numeric(SW) + E_CITY:as.numeric(SW) + 
                 COUPON:as.numeric(SW) + VACATION:as.numeric(SW) + as.numeric(SW):DISTANCE + 
                 S_CITY:as.numeric(VACATION) + COUPON:as.numeric(VACATION) + 
                 DISTANCE:as.numeric(VACATION) + PAX:as.numeric(VACATION) + 
                 HI:as.numeric(NEW) + 
                 #as.numeric(COUPON):S_CITY + as.numeric(COUPON):E_CITY + 
                 as.numeric(COUPON):HI,
               data = data)
summary(ilm.fit8) #R2 0.9342 Higher
plot(ilm.fit8) #Ext. good
AIC(ilm.fit8) #-668.6591 Low


#Exponential Model with log COUPON DISTANCE HI
ilm.fit8 <- lm(as.numeric(log(FARE)) ~ as.factor(S_CITY) + as.factor(E_CITY) + log(as.numeric(COUPON)) + 
                 as.factor(NEW) + log(as.numeric(HI)) + log(as.numeric(DISTANCE)) + 
                 as.numeric(PAX) + 
                 #as.numeric(DISTANCE):S_CITY + as.numeric(DISTANCE):E_CITY + 
                 as.numeric(DISTANCE):HI + as.numeric(DISTANCE):PAX + 
                 as.numeric(HI):PAX + 
                 #as.numeric(HI):S_CITY + as.numeric(HI):E_CITY + 
                 as.numeric(HI):VACATION + 
                 #S_CITY:as.numeric(SW) + E_CITY:as.numeric(SW) + 
                 COUPON:as.numeric(SW) + VACATION:as.numeric(SW) + as.numeric(SW):DISTANCE + 
                 S_CITY:as.numeric(VACATION) + COUPON:as.numeric(VACATION) + 
                 DISTANCE:as.numeric(VACATION) + PAX:as.numeric(VACATION) + 
                 HI:as.numeric(NEW) + 
                 #as.numeric(COUPON):S_CITY + as.numeric(COUPON):E_CITY + 
                 as.numeric(COUPON):HI,
               data = data)
summary(ilm.fit8) #R2 0.9371 Higher
plot(ilm.fit8) #Ext. good
AIC(ilm.fit8) #-697.9255 Lowest till now!

#Exponential Model with log COUPON DISTANCE HI and log in Interactions
ilm.fit8 <- lm(as.numeric(log(FARE)) ~ as.factor(S_CITY) + as.factor(E_CITY) + log(as.numeric(COUPON)) + 
                 as.factor(NEW) + log(as.numeric(HI)) + log(as.numeric(DISTANCE)) + 
                 as.numeric(PAX) + 
                 #as.numeric(DISTANCE):S_CITY + as.numeric(DISTANCE):E_CITY + 
                 log(as.numeric(DISTANCE)):log(HI) + log(as.numeric(DISTANCE)):PAX + 
                 log(as.numeric(DISTANCE)):PAX + 
                 #as.numeric(HI):S_CITY + as.numeric(HI):E_CITY + 
                 log(as.numeric(HI)):VACATION + 
                 #S_CITY:as.numeric(SW) + E_CITY:as.numeric(SW) + 
                 log(COUPON):as.numeric(SW) + VACATION:as.numeric(SW) + as.numeric(SW):log(DISTANCE) + 
                 S_CITY:as.numeric(VACATION) + log(COUPON):as.numeric(VACATION) + 
                 log(DISTANCE):as.numeric(VACATION) + PAX:as.numeric(VACATION) + 
                 log(HI):as.numeric(NEW) + 
                 #as.numeric(COUPON):S_CITY + as.numeric(COUPON):E_CITY + 
                 log(as.numeric(COUPON)):log(HI),
               data = data)
summary(ilm.fit8) #R2 0.9253 Higher
plot(ilm.fit8) #good
AIC(ilm.fit8) #-588.2157 higher

#Exponential Model with log COUPON DISTANCE HI and only one log interaction per interaction
ilm.fit8 <- lm(as.numeric(log(FARE)) ~ as.factor(S_CITY) + as.factor(E_CITY) + log(as.numeric(COUPON)) + 
                 as.factor(NEW) + log(as.numeric(HI)) + log(as.numeric(DISTANCE)) + 
                 as.numeric(PAX) + 
                 #as.numeric(DISTANCE):S_CITY + as.numeric(DISTANCE):E_CITY + 
                 log(as.numeric(DISTANCE)):(HI) + log(as.numeric(DISTANCE)):PAX + 
                 log(as.numeric(DISTANCE)):PAX + 
                 #as.numeric(HI):S_CITY + as.numeric(HI):E_CITY + 
                 log(as.numeric(HI)):VACATION + 
                 #S_CITY:as.numeric(SW) + E_CITY:as.numeric(SW) + 
                 log(COUPON):as.numeric(SW) + VACATION:as.numeric(SW) + as.numeric(SW):log(DISTANCE) + 
                 S_CITY:as.numeric(VACATION) + log(COUPON):as.numeric(VACATION) + 
                 log(DISTANCE):as.numeric(VACATION) + PAX:as.numeric(VACATION) + 
                 log(HI):as.numeric(NEW) + 
                 #as.numeric(COUPON):S_CITY + as.numeric(COUPON):E_CITY + 
                 log(as.numeric(COUPON)):(HI),
               data = data)
summary(ilm.fit8) #R2 0.9242 Lower
plot(ilm.fit8) #good
AIC(ilm.fit8) #-578.8088 Higher!

#Exponential Model with log COUPON DISTANCE HI and only one log interaction per interaction
ilm.fit8 <- lm(as.numeric(log(FARE)) ~ as.factor(S_CITY) + as.factor(E_CITY) + log(as.numeric(COUPON)) + 
                 as.factor(NEW) + log(as.numeric(HI)) + log(as.numeric(DISTANCE)) + 
                 as.numeric(PAX) + 
                 #as.numeric(DISTANCE):S_CITY + as.numeric(DISTANCE):E_CITY + 
                 (as.numeric(DISTANCE)):(HI) + (as.numeric(DISTANCE)):PAX + 
                 (as.numeric(DISTANCE)):PAX + 
                 #as.numeric(HI):S_CITY + as.numeric(HI):E_CITY + 
                 log(as.numeric(HI)):VACATION + 
                 #S_CITY:as.numeric(SW) + E_CITY:as.numeric(SW) + 
                 log(COUPON):as.numeric(SW) + VACATION:as.numeric(SW) + as.numeric(SW):log(DISTANCE) + 
                 S_CITY:as.numeric(VACATION) + log(COUPON):as.numeric(VACATION) + 
                 log(DISTANCE):as.numeric(VACATION) + PAX:as.numeric(VACATION) + 
                 log(HI):as.numeric(NEW) + 
                 #as.numeric(COUPON):S_CITY + as.numeric(COUPON):E_CITY + 
                 log(as.numeric(COUPON)):(HI),
               data = data)
summary(ilm.fit8) #R2 0.9242 Lower
#plot(ilm.fit8) #good
AIC(ilm.fit8) #-610.6472 Lower but still higher tahn best model!

#Exponential Model with log COUPON DISTANCE HI and only log HI in interactions
ilm.fit8 <- lm(as.numeric(log(FARE)) ~ as.factor(S_CITY) + as.factor(E_CITY) + log(as.numeric(COUPON)) + 
                 as.factor(NEW) + log(as.numeric(HI)) + log(as.numeric(DISTANCE)) + 
                 as.numeric(PAX) + 
                 #as.numeric(DISTANCE):S_CITY + as.numeric(DISTANCE):E_CITY + 
                 (as.numeric(DISTANCE)):(HI) + (as.numeric(DISTANCE)):PAX + 
                 (as.numeric(DISTANCE)):PAX + 
                 #as.numeric(HI):S_CITY + as.numeric(HI):E_CITY + 
                 (as.numeric(HI)):VACATION + 
                 #S_CITY:as.numeric(SW) + E_CITY:as.numeric(SW) + 
                 log(COUPON):as.numeric(SW) + VACATION:as.numeric(SW) + as.numeric(SW):(DISTANCE) + 
                 S_CITY:as.numeric(VACATION) + log(COUPON):as.numeric(VACATION) + 
                 (DISTANCE):as.numeric(VACATION) + PAX:as.numeric(VACATION) + 
                 (HI):as.numeric(NEW) + 
                 #as.numeric(COUPON):S_CITY + as.numeric(COUPON):E_CITY + 
                 log(as.numeric(COUPON)):(HI),
               data = data)
summary(ilm.fit8) #R2 0.9031 Higher
#plot(ilm.fit8) #good
AIC(ilm.fit8) #not good!

#Exponential Model with log COUPON HI
ilm.fit8 <- lm(as.numeric(log(FARE)) ~ as.factor(S_CITY) + as.factor(E_CITY) + log(as.numeric(COUPON)) + 
                 as.factor(NEW) + log(as.numeric(HI)) + (as.numeric(DISTANCE)) + 
                 as.numeric(PAX) + 
                 #as.numeric(DISTANCE):S_CITY + as.numeric(DISTANCE):E_CITY + 
                 as.numeric(DISTANCE):HI + as.numeric(DISTANCE):PAX + 
                 as.numeric(HI):PAX + 
                 #as.numeric(HI):S_CITY + as.numeric(HI):E_CITY + 
                 as.numeric(HI):VACATION + 
                 #S_CITY:as.numeric(SW) + E_CITY:as.numeric(SW) + 
                 COUPON:as.numeric(SW) + VACATION:as.numeric(SW) + as.numeric(SW):DISTANCE + 
                 S_CITY:as.numeric(VACATION) + COUPON:as.numeric(VACATION) + 
                 DISTANCE:as.numeric(VACATION) + PAX:as.numeric(VACATION) + 
                 HI:as.numeric(NEW) + 
                 #as.numeric(COUPON):S_CITY + as.numeric(COUPON):E_CITY + 
                 as.numeric(COUPON):HI,
               data = data)
summary(ilm.fit8) #R2 0.9381 Highest!!
#plot(ilm.fit8) #Ext. good
AIC(ilm.fit8) #-707.5091 Lowest of Good fit Exponential models!



#Exponential Model with log COUPON DISTANCE
ilm.fit8 <- lm(as.numeric(log(FARE)) ~ as.factor(S_CITY) + as.factor(E_CITY) + log(as.numeric(COUPON)) + 
                 as.factor(NEW) + (as.numeric(HI)) + log(as.numeric(DISTANCE)) + 
                 as.numeric(PAX) + as.numeric(DISTANCE):HI + as.numeric(DISTANCE):PAX + 
                 as.numeric(HI):PAX + as.numeric(HI):VACATION + 
                 COUPON:as.numeric(SW) + VACATION:as.numeric(SW) + as.numeric(SW):DISTANCE + 
                 S_CITY:as.numeric(VACATION) + COUPON:as.numeric(VACATION) + 
                 DISTANCE:as.numeric(VACATION) + PAX:as.numeric(VACATION) + HI:as.numeric(NEW) + as.numeric(COUPON):HI,
               data = data)
summary(ilm.fit8) #R2 0.9356 Lower!!
plot(ilm.fit8) #Ext. good
AIC(ilm.fit8) #-682.408 higher!
BIC(ilm.fit8) #57.67614


#Stepwise selection on best model
istep2 <- stepAIC(ilm.fit8,direction = "both")
ilm.fit8 <- lm(as.numeric(log(FARE)) ~ as.factor(S_CITY) + as.factor(E_CITY) + 
                 log(as.numeric(COUPON)) + as.factor(NEW) + log(as.numeric(HI)) + 
                 as.numeric(DISTANCE) + as.numeric(PAX) + as.numeric(DISTANCE):PAX + 
                 PAX:as.numeric(HI) + as.numeric(HI):VACATION + COUPON:as.numeric(SW) + 
                 VACATION:as.numeric(SW) + as.numeric(SW):DISTANCE + S_CITY:as.numeric(VACATION) + 
                 COUPON:as.numeric(VACATION) + PAX:as.numeric(VACATION) + 
                 HI:as.numeric(NEW) + HI:as.numeric(COUPON),
               data = data)
summary(ilm.fit8) #R2 0.9383 Highest!!
plot(ilm.fit8) #Ext. good !!!!
AIC(ilm.fit8) #-710.6806 Lowest of Good fit Exponential models!
BIC(ilm.fit8) #20.48689 Lowest!!!!

#interaction Terms visualizations
library(ggplot2)
gp <- ggplot(data=data, aes(x=(HI), y=log(FARE), colour=factor(VACATION))) 
gp + geom_point() + stat_smooth(method="lm")

gp <- ggplot(data=data, aes(x=(HI), y=log(FARE), colour=factor(NEW))) 
gp + geom_point() + stat_smooth(method="lm")

gp <- ggplot(data=data, aes(x=COUPON, y=log(FARE), colour=factor(SW))) 
gp + geom_point() + stat_smooth(method="lm")

gp <- ggplot(data=data, aes(x=COUPON, y=log(FARE), colour=factor(VACATION))) 
gp + geom_point() + stat_smooth(method="lm")

gp <- ggplot(data=data, aes(x=DISTANCE, y=log(FARE), colour=factor(SW))) 
gp + geom_point() + stat_smooth(method="lm")

gp <- ggplot(data=data, aes(x=PAX, y=log(FARE), colour=factor(VACATION))) 
gp + geom_point() + stat_smooth(method="lm")


###############
#Decision Tree
library(rpart)
tree.fit <- rpart(as.numeric(FARE)~as.factor(S_CODE)+as.factor(S_CITY)+as.factor(E_CODE)+
                    as.factor(E_CITY)+as.numeric(COUPON)+as.factor(NEW)+as.factor(VACATION)+
                    as.factor(SW)+as.numeric(HI)+as.numeric(S_INCOME)+as.numeric(E_INCOME)+
                    as.numeric(S_POP)+as.numeric(E_POP)+as.factor(SLOT)+as.factor(GATE)+
                    as.numeric(DISTANCE)+as.numeric(PAX),data=data)
print(tree.fit)
plot(tree.fit)
text(tree.fit)
plotcp(tree.fit) # Cross-Validation plot


############################
#Conditional Inference Tree
library(party)
ctree.fit <- ctree(as.numeric(FARE)~as.factor(S_CODE)+as.factor(S_CITY)+as.factor(E_CODE)+
                     as.factor(E_CITY)+as.numeric(COUPON)+as.factor(NEW)+as.factor(VACATION)+
                     as.factor(SW)+as.numeric(HI)+as.numeric(S_INCOME)+as.numeric(E_INCOME)+
                     as.numeric(S_POP)+as.numeric(E_POP)+as.factor(SLOT)+as.factor(GATE)+
                     as.numeric(DISTANCE)+as.numeric(PAX),data=data, controls = ctree_control(mincriterion = 0.95,minsplit = 20,maxdepth = 5))
plot(ctree.fit)



##############################################################################################################
##############################################################################################################
##############################################################################################################

#Predictive Models
#Predict Fare on a given route


#








