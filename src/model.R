library("rjags")
source("src/clean.R")
source("src/utils.R")
library("caret")
library(splitstackshape)

df = get.cervical.data("data")
str(df)

# Test train split
split_proportion = 0.1
out = stratified(df, c("biopsy"), split_proportion, bothSets = TRUE)
valid = out[[1]]
train = out[[2]]


preProcValues <- preProcess(train, method = c("center", "scale"))


trainTransformed <- predict(preProcValues, train)
validTransformed <- predict(preProcValues, valid)

mod_string = " model {

 # Data
 for (i in 1:length(y)) {
    y[i] ~ dbern(p[i])
  
    logit(p[i]) = b0 + (b[1] * age[i]) + (b[2]  * num_pregnancies[i]) + 
                (b[3] * stds[i]) + (b[4] * iud[i]) + 
                (b[5] * hormonal_contraceptives[i]) + (b[6] * num_stds_diag[i])  
 }
   # Priors
   b0 ~ dnorm(0.0, 1.0/25)
    
    for (j in 1:6) {
      b[j] ~ dnorm(0.0, 1.0/25) # has variance 1.0
    }
}"

set.seed(102)
data_jags = list(
  y = as.numeric(trainTransformed$smokes == "Cancer"),
  age = trainTransformed$age,
  num_pregnancies = trainTransformed$num_pregnancies,
  num_stds_diag = trainTransformed$num_stds_diag,
  stds = as.numeric(trainTransformed$stds == "Has STDs"),
  iud = as.numeric(trainTransformed$iud == "Has IUD"),
  hormonal_contraceptives = as.numeric(trainTransformed$hormonal_contraceptives == "Contraceptives")
)

params = c("b0", "b")

mod = jags.model(
  textConnection(mod_string), 
  data = data_jags,  
  n.chains = 3
)

update(mod, 1e3)

mod_sim = coda.samples(
  model=mod,
  variable.names = params,
  n.iter=5e3
)

# Diagnostics
plot(mod_sim)
gelman.diag(mod_sim)
effectiveSize(mod_sim)
