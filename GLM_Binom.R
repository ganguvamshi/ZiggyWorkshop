#set the working directory & read the data
#On a Windows computer: adjust blah blah blah
#setwd("c:/blah blah blah")

#For a Mac:
setwd("/Users/ziggy/Documents/R/SCELSE")

#Import the data from a tab delimited ascii file
TF <- read.table(file = "Trifur.txt", 
                 header = TRUE,
                 dec = ".")

#dec = '.'   means that the point is used for decimals. 
#Change to   dec = ","   if required.
########################################################################

#Trifur is a fish parasite. We are interested in modeling the probability
#of the parasite being present as a function of the total length (LT), weight,
#sex of the ind. and area (high, med, low contamination) where the fish were caught.



########################################################################
#To see what is in the object TF, type:
names(TF)
str(TF)
# 'data.frame':	155 obs. of  6 variables:
# $ ID    : int  107 108 109 111 112 113 114 115 116 117 ...
# $ Area  : int  1 1 1 1 1 1 1 1 1 1 ...
# $ LT    : num  31.5 34.5 39 28.5 36 34.5 34 32.5 31.5 33 ...
# $ Weight: int  380 470 696 248 500 508 478 400 468 478 ...
# $ Sex   : int  1 2 2 2 2 2 2 1 2 1 ...
# $ Trifur: int  0 0 0 0 0 0 0 0 0 0 ...




########################################################################
#Housekeeping

library(lattice)  #For fancy multipanel graphs

########################################################################





########################################################################
#Data exploration
#A Outliers in Y /Outliers in X
#B Collinearity X
#C Relationships Y vs X
#D Spatial/temporal aspects of sampling design (not relevant here)
#E Interactions (is the quality of the data good enough to include them?)
#F Zero inflation Y
#G Are categorical covariates balanced?


##############################################
#A Outliers
dotchart(TF$Trifur) #just useful to check data is 0s or 1s

dotplot(as.matrix(TF[,c("LT", "Weight")]), 
        groups=FALSE,
        strip = strip.custom(bg = 'white',
                             par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col=1, cex  = 0.5, pch = 16,
        xlab = "Value of the variable",
        ylab = "Order of the data from text file")


#B. Collinearity
pairs(TF[,c("LT", "Weight")])
cor(TF[,c("LT", "Weight")])


#C. Relationships
plot(x = TF$LT, y = TF$Trifur)
boxplot(LT ~ Trifur, data = TF)
boxplot(Weight ~ Trifur, data = TF)

#E. Interactions
coplot(Trifur ~ LT | factor(Sex) * factor(Area),
       data = TF)  #here, not good to fit a 3-way interaction because 
#it may be significant simply because of 1 fish in 1 combination
#dataset not good for that
coplot(Trifur~LT | factor(Area), data = TF )
coplot(Trifur~LT | factor(Sex), data = TF )


#Any missing values?
colSums(is.na(TF))
#########################################################


table(TF$Sex, TF$Area, TF$Trifur)



#########################################################
#Start analysis
M1 <- glm(Trifur ~ factor(Sex) + LT + factor(Area) +
            factor(Sex) : LT + 
            factor(Sex) : factor(Area),
          family=binomial,
          data = TF)

# some have only 1/2 replicates, so three way interactions are omitted.

summary(M1) #IF get a WARNING message, Google Quasiseparation (maybe all males have the parasites, not good for model)         

#Model selection                           
drop1(M1, test="Chi")


#Remove non-significant interactions
M2 <- glm(Trifur ~ factor(Sex) + LT + factor(Area), 
          family = binomial,
          data = TF)
drop1(M2, test="Chi")
summary(M2)
###########################################


#binary data cannot be overdispersed in general, so not check this
# although for poisson, mean=variance, but still there could be some overdispersion (variance is greater than mean)
# so we check for overdispersion for poisson.if overdispersed use neg binomial.
##########################################
#Model validation:
par(mfrow = c(2, 2))
plot(M2)

par(mfrow = c(1, 1))
plot(M2, which = c(4))

E2 <- resid(M2, type = "pearson")
plot(x = TF$LT, y = E2)
abline(h = 0, v= 0)

plot(x = TF$Sex, y = E2)
abline(h = 0, v= 0)

plot(x = TF$Area, y = E2)
abline(h = 0, v= 0)
######################################################



######################################################
#Model interpretation
summary(M2)

#Coefficients:
#               Estimate Std. Error z value Pr(>|z|)
#(Intercept)   -13.72804    2.96265  -4.634 3.59e-06 ***
#factor(Sex)2   -1.85502    0.61524  -3.015 0.002569 **
#LT              0.34245    0.08807   3.888 0.000101 ***
#factor(Area)2   4.12838    0.82733   4.990 6.04e-07 ***
#factor(Area)3   4.11067    0.85067   4.832 1.35e-06 ***

#Par_i ~ Bin(pi_i)
#i = 1, ..., 155
#Par_i is 1 if fish i has Trifur presence, 0 otherwise
#E(Par_i) = pi_i
#var(Par_i) = pi_i * (1 - pi_i)
#logit(pi_i) = alpha + Sex_i + LT_i + Area_i

#         exp(alpha+Sex_i + LT_i + Area_i)
#pi_i = --------------------------------------
#        1+ exp(alpha+Sex_i + LT_i + Area_i)

# #Equation for: Sex = 1, Area = 1
# eta = -13.7  + 0.34 * LT
# pi = exp(eta) / (1+exp(eta))

# #Equation for: Sex = 1, Area = 2
# eta = -13.7 + 4.12   + 0.34 * LT
# pi = exp(eta)/ (1+exp(eta))

# #Equation for: Sex = 1, Area = 3
# eta = -13.7 + 4.11   + 0.34 * LT
# pi = exp(eta)/ (1+exp(eta))

# #Equation for: Sex = 2, Area = 1
# eta = -13.7  -1.85 +   0.34 * LT
# pi = exp(eta)/ (1+exp(eta))

# #Equation for: Sex = 2, Area = 2
# eta = -13.7  -1.85 + 4.12 +   0.34 * LT
# pi = exp(eta)/ (1+exp(eta))

# #Equation for: Sex = 2, Area = 3
# eta = -13.7  -1.85 + 4.11 +   0.34 * LT
# pi = exp(eta)/ (1+exp(eta))



range(TF$LT)
#V: never extrapolate, always stay within the range
# create some fake data sets
MyData1.1<-data.frame(LT=seq(24,42), Sex=1, Area=1)
MyData1.2<-data.frame(LT=seq(24,42), Sex=1, Area=2)
MyData1.3<-data.frame(LT=seq(24,42), Sex=1, Area=3)
MyData2.1<-data.frame(LT=seq(24,42), Sex=2, Area=1)
MyData2.2<-data.frame(LT=seq(24,42), Sex=2, Area=2)
MyData2.3<-data.frame(LT=seq(24,42), Sex=2, Area=3)

P.11<-predict(M2, newdata=MyData1.1, type="response") #reponse already does the e^
P.12<-predict(M2, newdata=MyData1.2, type="response")
P.13<-predict(M2, newdata=MyData1.3, type="response")
P.21<-predict(M2, newdata=MyData2.1, type="response")
P.22<-predict(M2, newdata=MyData2.2, type="response")
P.23<-predict(M2, newdata=MyData2.3, type ="response")
#V: if we want stand errors for confidence Intervals, we need type="link" also

#win.graph()   or    quartz() for Mac
plot(x=TF$LT, y=TF$Trifur,
     xlab = "Length",
     ylab = "Probability of parasites presence")
lines(MyData1.1$LT, P.11,col=1,lwd=3)
lines(MyData1.2$LT, P.12,col=1,lwd=3,lty=2)
lines(MyData1.3$LT, P.13,col=1,lwd=3,lty=3)
lines(MyData2.1$LT, P.21,col=2,lwd=3)
lines(MyData2.2$LT, P.22,col=2,lwd=3, lty =2 )
lines(MyData2.3$LT, P.23,col=2,lwd=3, lty =3)

legend("topleft",
       legend = c("S1/A1","S1/A2","S1/A3","S2/A1","S2/A2","S2/A3"),
       col = c(1,1,1,2,2,2),
       lty = c(1,2,3,1,2,3),
       cex = 0.7,
       lwd = c(3,3,3,3,3,3))

text(40, 0.25, "CI ??")
#######################################################

#to add SE or CI need to get fitted values and SE using "link"
#BUT then need to bring the values back to original scale using
#the appropriate link function (for Poisson and NegBinom = exp())

P.11<-predict(M2, newdata=MyData1.1, type="link", se = TRUE)
# type = "link" doesnt do exponential convertion
# we need to do it manually while plotting 

lines(MyData1.1$LT, exp(P.11$fit) / (1 + exp(P.11$fit)),col=1,lwd=3)
lines(MyData1.1$LT, exp(P.11$fit + 1.96 * P.11$se.fit) / 
        (1 + exp(P.11$fit + 1.96 * P.11$se.fit)),                        
      col=1)

lines(MyData1.1$LT, exp(P.11$fit - 1.96 * P.11$se.fit) / 
        (1 + exp(P.11$fit - 1.96 * P.11$se.fit)),                        
      col=1)


#and the same for other Area/Sex combinations..







