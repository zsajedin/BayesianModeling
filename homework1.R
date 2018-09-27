rm(list=ls(all=TRUE))  # clear all variables
graphics.off()  # clear all graphics

# load data file
PWdata<-read.csv(file="ProspectiveMemoryEyeTrackingHW1.csv",header=TRUE,stringsAsFactors=TRUE)

# load the brms library
library(brms)
model1 = brm(Monitoring ~ Condition , data = PWdata, iter = 2000, warmup = 200, chains = 3, thin = 2 )

print(resid(model1))


# print out summary of model
print(summary(model1))

# Check on model convergence and posteriors
plot(model1)

# Plot fitting line
plot(marginal_effects(model1), points = TRUE)


# Compute HDPIs
library(broom)
library(coda)

print( tidyMCMC(model1$fit, conf.int = TRUE, conf.method = "HPDinterval") )


post<-posterior_samples(model1)

dev.new()
plot(density(post$b_ConditionPM))

# check probability that difference is less than zero
cat("Probability that group mean difference is greater than zero is: ", sum(post$b_ConditionPM > 0) / length(post$b_ConditionPM))



#2nd way of calcuating the probability of no effect
#cond1Mean = post$b_Intercept
#cond2Mean = post$b_ConditionPM + post$b_Intercept
#difConds = cond2Mean - cond1Mean
#plot(density(difConds))

# Compute posterior probability that b_NumDistractors is less than 0
#probLess0 <-length(difConds[difConds > 0])/length(difConds) 
#cat("Probability b_NumberDistractors is less than 0 = ", probLess0, "\n")


#--------------------

# Compute the highst posterior density interval (HPDI) for each parameter
# Load up some useful libraries for analyzing posterior distribution
library(broom)
library(coda)

print( tidyMCMC(model1$fit, conf.int = TRUE, conf.method = "HPDinterval") )


#Null Model
model2 = brm(Monitoring ~ 1 , data = PWdata, iter = 2000, warmup = 200, chains = 3, thin = 2 )

# print out summary of model
print(summary(model2))

# Check on model convergence and posteriors
plot(model2)

print(loo(model1, model2))
 
