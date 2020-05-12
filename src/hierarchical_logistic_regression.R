library("rjags")
source("src/clean.R")
library("caret")
library("ggplot2")
library("ggthemr")
library("bayesplot")
library("gtools")
library("dplyr")
ggthemr("light")

df_no_one_hot = get.titanic.data()
preProc <- preProcess(df_no_one_hot)
df_no_one_hot <- predict(preProc, df_no_one_hot)

dummy = dummyVars(" ~ .", data = df_no_one_hot, fullRank = TRUE)
df = data.frame(predict(dummy, newdata = df_no_one_hot))


mod_string = " model {

 # Data
 for (i in 1:length(y)) {
    y[i] ~ dbern(p[i])
  
    logit(p[i]) = alpha[grp[i]] + (b[1] * Age[i]) + (b[2]  * SibSp[i]) + 
                (b[3] * Parch[i]) + (b[4] * Pclass.2nd.Class[i]) + 
                (b[5] * Pclass.3rd.Class[i]) + (b[6] * Sex.Female[i]) + (b[7] * Embarked.Cherbourg[i]) + 
                (b[8] * Embarked.Queenstown[i])
 }
  
  # Priors
   
  for(j in 1:3) {
    alpha[j] ~ dnorm(mu, prec_tau)
  }
  
  mu ~ dnorm(0.0, 1.0/25)
  prec_tau ~ dgamma(1.0/2.0, 1 *10.0/2.0)
  tau = sqrt(1.0/ prec_tau)
  
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
  grp = as.numeric(df_no_one_hot$Embarked),
  
  Embarked.Cherbourg = df$Embarked.Cherbourg,
  Embarked.Queenstown = df$Embarked.Queenstown
)

params = c("alpha", "b", "mu", "tau")

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
#dimnames(posterior)[[3]] = c("intercept", "b_Age", "b_SibSp", "b_Parch", "b_Pclass.Second", "b_Pclass.Third",
#                            "b_Sex.Female")


mcmc_intervals(posterior) + geom_vline(xintercept=0, linetype="dashed", color = "black", size=0.5)


dic = dic.samples(mod, n.iter = 5e3)
dic
