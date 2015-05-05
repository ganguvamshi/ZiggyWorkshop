## GLMM Poisson_Neg Binomial
## GLMS are bad when random effect size is low (2), May be 3 is fine
#Set the working directory and import the data

setwd("/Users/vamshidhar/Documents/OneDrive/github/ZiggyWorkshop")
KW <- read.table(file = "pollen.txt", 
                 header = TRUE, 
                 dec = ".")

names(KW)
#[1]  "Time"      "Hive"      "Treatment" "Dandelion"
str(KW)
 
#Model the number of Dandelion pollen as a function
#of time, treatment (3 food sources), the interaction between time
#and treatment, and a hive effect.
###################################################################




###################################################################
#Load packages and library files
library(lattice)  #Needed for multi-panel graphs
library(lme4)
 
########################################################


########################################################
#Housekeeping
KW$fHive <- factor(KW$Hive)

########################################################



########################################################
#Data exploration

xyplot(Dandelion ~ Time | Treatment, 
       xlab = list("Time (days)", cex = 1.5),
       ylab = list("Number of dandelion pollen grains", cex = 1.5),
       data = KW, layout = c(3,1),
       groups = Hive,
       type = "l", col = 1,
       strip = strip.custom(bg = 'white',
                            par.strip.text = list(cex = 1.2)),
       scales = list(alternating = T,
                     x = list(relation = "same"),
                     y = list(relation = "same"))
)

#V : lines represent different hives. There is some drastic diff between hives (random effect)
######################################################





#######################################################
#By design of the study use hive as random effect
#Step 1: Apply a Poisson GLMM
##############################

M1 <- glmer(Dandelion ~ Time * Treatment + (1|fHive),
            data = KW, 
            family = poisson) # random effect is within (), poisson becaue of counts
print(summary(M1), digits = 2, signif.stars=FALSE)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: poisson  ( log )
# Formula: Dandelion ~ Time * Treatment + (1 | fHive)
# Data: KW
# 
# AIC      BIC   logLik deviance df.resid 
# 1035.6   1050.3   -510.8   1021.6       53 
# 
# Scaled residuals: 
#   Min     1Q Median     3Q    Max 
# -6.85  -1.91  -0.12   1.98   7.88 
# 
# Random effects:
#   Groups Name        Variance Std.Dev.
# fHive  (Intercept) 1        1             ## weird that variance and std.dev is 1
# Number of obs: 60, groups:  fHive, 15
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)              3.232      0.463     7.0    3e-12
# Time                    -0.045      0.026    -1.8    0.080
# TreatmentProtein         2.043      0.650     3.1    0.002
# TreatmentSyrup           1.804      0.650     2.8    0.005
# Time:TreatmentProtein   -0.360      0.035   -10.4   <2e-16
# Time:TreatmentSyrup     -0.074      0.030    -2.4    0.015
# 
# Correlation of Fixed Effects:
#   (Intr) Time   TrtmnP TrtmnS Tm:TrP
# Time        -0.137                            
# TretmntPrtn -0.712  0.097                     
# TretmntSyrp -0.712  0.098  0.507              
# Tm:TrtmntPr  0.102 -0.747 -0.120 -0.073       
# Tm:TrtmntSy  0.117 -0.852 -0.083 -0.113  0.636

#Check for overdispersion
E1 <- resid(M1, type = "pearson")
N  <- nrow(KW)
p  <- length(fixef(M1)) + 1  # +1 because of 1 Random effect 
Overdispersion <- sum(E1^2) / (N - p)
Overdispersion

#Why do we have overdispersion?
  #A. Outliers?                  ==> Remove them?
  #B. Missing covariates or interactions?  ==> Go back or..add a latent variable 
  #C. Zero inflation?            ==> ZIP
  #D. Large variance?            ==> NB
  #E. Correlation?               ==> GLMM
  #F. Non-linear patterns        ==> GAM(M) 
  #G. Wrong link function        ==> Change it 


#Standard steps to address some of these points:
#
#Fitted values
F1 <- fitted(M1)

plot(x = F1,
     y = E1,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0)
#No outliers

#C. Zero inflation
plot(table(KW$Dandelion),
     type = "h")     
#No zero inflation!


#D. Large variance?
dotchart(KW$Dandelion)
#That looks like a negative binomial distribution!   ###Large range of data


#E. Correlation
#Do we have the spatial position of the hives? No

#F. Non-linear patterns   
#
plot(x = KW$Time,
     y = E1)
abline(h = 0, lty = 2)
#Too few unique time points for smoothers.  ### Could fit time as factor if non linear pattern


#Approach to deal with overdispersion: NB GLMM

#This function is still under development!
M2A <- glmer.nb(Dandelion ~ Time * Treatment + (1|fHive), 
               data = KW)
summary(M2A)
drop1(M2A, test = "Chi")  #Doesn't work in my R version
                          


#Alternativaly...use this:
#Install glmmADMB: 
#Go to the web-page: 
#install.packages("glmmADMB", repos="http://r-forge.r-project.org", type="source")
#Go for the second option in the web-page


library("glmmADMB")

M2 <- glmmadmb(Dandelion ~ Time * Treatment, 
             random =~ 1|fHive,              #note that we are using a different package
             family="nbinom", data=KW)       #and the syntax has changed
summary(M2)

E2 <- resid(M2, type = "pearson")

#Overdisp from glmm adnb
p <- length(fixef(M2)) + 1 + 1  #6 betas, 1 sigma, 1 k
Overdispersion2 <-sum(E2^2) / (N - p)
Overdispersion2
# look under dispersed. but looks fine 

#Step 2: Is everthing significant?
summary(M2)
drop1(M2, test = "Chi")  #Ah...but you need sub-models with the same k!
                         #Trouble.
                      ### You are not using the same k in both models, therefore its not just the interaction you are testing
#Zuur et al. (2012a): If not the same k, then p value slightly wrong                            
# Not only testing for interaction, but testing for also dispersion (k) so need to be careful

#Drop the interaction                         
M3 <- glmmadmb(Dandelion ~ Time + Treatment, 
             random =~ 1|fHive, 
             family="nbinom", data=KW)
summary(M3)
drop1(M3, test = "Chi")    ### here SE with same values... not good

#Drop Treatment?
M4 <- glmmadmb(Dandelion ~ Time, 
             random =~ 1|fHive, 
             family="nbinom", data=KW)
summary(M4)
drop1(M4, test = "Chi")
# if p values are aroud the threshold (0.05) give it a check as p values are just estimates not exact values

############################################
#Step 3: Explain what it all means
summary(M4)
#Task: Write down the estimated model
#

#Sketch the fitted values
betas   <- fixef(M4)  #betas
Covbeta <- vcov(M4)   #covariance matrix
alpha   <-  M4$alpha  #= k = size
k       <- alpha      #(this may be better to avoid confusion) 
#And the random effects
#a       <-  ranef(M4)$fHive[,'(Intercept)']
#Re      <- as.numeric(as.factor(KW$fHive))

#Now we can calculate fitted values
# (predict(type="link")) doing it manually
MyData <- data.frame(Time = c(1,2,3,4))
X <- model.matrix(~ Time, data = MyData)
MyData$eta <- X %*% betas # estimated values
MyData$SE <- sqrt(diag(X %*% Covbeta %*% t(X)))
MyData$etaRes <- exp(MyData$eta) # transform back to original (type="response')
MyData$lowSE <- exp(MyData$eta - 1.96 * MyData$SE) #(se=TRUE)
MyData$highSE <- exp(MyData$eta + 1.96 * MyData$SE) # (se=TRUE)

MyData

plot(x = KW$Time, 
     y = KW$Dandelion,
     xlab = "Time",
     ylab = "Dandelion")
lines(x = MyData$Time,
      y = MyData$etaRes,
      lwd = 2)

lines(x = MyData$Time,
      y = MyData$highSE,
      lty = 2)

lines(x = MyData$Time,
      y = MyData$lowSE,
      lty = 2)