library("rjags")
source("src/clean.R")
library("caret")
library("ggplot2")
library("ggthemr")
library("bayesplot")
library("gtools")
library("dplyr")
ggthemr("light")
library(latex2exp)

df = get.titanic.data()



dummy = dummyVars(" ~ .", data = df, fullRank = TRUE)
df = data.frame(predict(dummy, newdata = df))


mod_string = " model {

 # Data
 for (i in 1:length(y)) {
    y[i] ~ dbern(p[i])
  
    logit(p[i]) = b0 + (b[1] * Age[i]) + (b[2]  * SibSp[i]) + 
                (b[3] * Parch[i]) + (b[4] * Pclass.2nd.Class[i]) + 
                (b[5] * Pclass.3rd.Class[i]) + (b[6] * Sex.Female[i]) + 
                (b[7] * Embarked.Cherbourg[i]) + (b[8] * Embarked.Queenstown[i]) 
 }
   # Priors
   b0 ~ dnorm(0.0, 1.0/25)
    
    for (j in 1:8) {
      b[j] ~ dnorm(0.0, 1.0/25) # has variance 1.0
    }
}"

set.seed(102)
data_jags = list(
  y = df$Survived.Dead,
  Age = df$Age,
  SibSp = df$SibSp,
  Parch = df$Parch,
  Pclass.2nd.Class = df$Pclass.2nd.Class,
  Pclass.3rd.Class = df$Pclass.3rd.Class,
  Sex.Female = df$Sex.Female,
  Embarked.Cherbourg = df$Embarked.Cherbourg,
  Embarked.Queenstown = df$Embarked.Queenstown
)

params = c("b0", "b")

mod = jags.model(
  textConnection(mod_string), 
  data = data_jags,  
  n.chains = 3
)

update(mod, 4e3)

mod_sim = coda.samples(
  model=mod,
  variable.names = params,
  n.iter=5e4
)

mod_csim = as.mcmc(do.call(rbind, mod_sim))

# Diagnostics
#plot(mod_sim)
gelman.diag(mod_sim)
effectiveSize(mod_sim)


# Inference on Parameters
posterior = aperm(as.array(mod_sim), c(1,3,2))
dimnames(posterior)[[2]] = c("1", "2", "3")
dimnames(posterior)[[3]] = c("Age", 
                             "SibSp", 
                             "Parch", 
                             "2nd Economic Class (Indicator)", 
                             "3rd Economic Class (Indicator)",
                             "Female (Indicator)", 
                             "Embarked Cherbourg (Indicator)", 
                             "Embarked Queenstown (Indicator)",
                             "Intercept")


mcmc_intervals(posterior, prob = 0.50, prob_outer = 0.95) + 
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5) + 
  labs(title = "Posterior Density of Model Parameters", subtitle = "95% posterior density plot. The thick line represents 50% of the area.") + 
  xlab("Coeffcient magnitude") 

mcmc_areas(posterior, prob = 0.50, prob_outer = 0.70) + 
  geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5) + 
  labs(title = "Posterior Density of Model Parameters", subtitle = "95% posterior density plot. The shaded area represents 50% of the area.") + 
  xlab("Coeffcient magnitude") 

dic = dic.samples(mod, n.iter = 5e3)

get.predictive.distribution <- function(mod_csim, age, sipsp, parch, pClassSecond, 
                                    pClassThird, sex, embarkedCherbourg, embarkedQueenstown) {
  
  logit_p = mod_csim[, "b0"] + mod_csim[, c(1:8)] %*% c(age, sipsp, parch, pClassSecond, pClassThird, 
                                              sex, embarkedCherbourg, embarkedQueenstown)
  
  # Probability of dying for the given data vector
  p = inv.logit(logit_p)
  p
}
