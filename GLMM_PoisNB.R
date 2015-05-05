## GLMM Poisson_Neg Binomial

#Set the working directory and import the data

setwd("/Users/ziggy/Documents/R/SCELSE")
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
######################################################





#######################################################
#By design of the study use hive as random effect
#Step 1: Apply a Poisson GLMM
##############################

M1 <- glmer(Dandelion ~ Time * Treatment + (1|fHive),
            data = KW, 
            family = poisson)
print(summary(M1), digits = 2, signif.stars=FALSE)

#Check for overdispersion
E1 <- resid(M1, type = "pearson")
N  <- nrow(KW)
p  <- length(fixef(M1)) + 1
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
#Go to the web-page: http://glmmadmb.r-forge.r-project.org/
#Go for the second option in the web-page


library("glmmADMB")

M2 <- glmmadmb(Dandelion ~ Time * Treatment, 
             random =~ 1|fHive,              #note that we are using a different package
             family="nbinom", data=KW)       #and the syntax has changed
summary(M2)

E2 <- resid(M2, type = "pearson")

#Overdisp from glmm adnb
p <- 6 + 1 + 1  #6 betas, 1 sigma, 1 k
Overdispersion2 <-sum(E2^2) / (N - p)
Overdispersion2



#Step 2: Is everthing significant?
summary(M2)
drop1(M2, test = "Chi")  #Ah...but you need sub-models with the same k!
                         #Trouble.
                      ### You are not using the same k in both models, therefore its not just the interaction you are testing
#Zuur et al. (2012a): If not the same k, then p value slightly wrong                            
    
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
MyData <- data.frame(Time = c(1,2,3,4))
X <- model.matrix(~ Time, data = MyData)
MyData$eta <- X %*% betas

MyData$SE <- sqrt(diag(X %*% Covbeta %*% t(X)))
MyData

plot(x = KW$Time, 
     y = KW$Dandelion,
     xlab = "Time",
     ylab = "Dandelion")
lines(x = MyData$Time,
      y = exp(MyData$eta),
      lwd = 2)

lines(x = MyData$Time,
      y = exp(MyData$eta + 1.96 * MyData$SE),
      lty = 2)

lines(x = MyData$Time,
      y = exp(MyData$eta - 1.96 * MyData$SE),
      lty = 2)
      
      

      
