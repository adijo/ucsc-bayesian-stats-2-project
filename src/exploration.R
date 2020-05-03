library("ggplot2")
library("ggthemr")
ggthemr("fresh")


source("src/clean.R")
source("src/utils.R")

df = get.cervical.data("data")
str(df)


### Distribution of target variable

# Simple plot
ggplot(df, aes(x=biopsy)) + 
  geom_bar(aes(y = (..count..) / sum(..count..))) + 
  labs(y = "Percentage (%)",
       title="Distribution")


ggplot(df, aes(x=smokes, y=age, fill=biopsy)) + geom_boxplot() 

ggplot(df, aes(x=hormonal_contraceptives, y=age, fill=biopsy)) + geom_boxplot() 

# Potentially useful
ggplot(df, aes(x=hormonal_contraceptives, y=age, fill=biopsy)) + geom_boxplot()  + facet_grid(.~smokes)  

ggplot(df, aes(x=num_stds_diag, y=age, fill=biopsy)) + geom_boxplot()  + facet_grid(.~smokes) + labs(x = "Number of Diagnosed STDs", y="Age")

# Potentialy useful
ggplot(df, aes(x=num_pregnancies,y=hormonal_contraceptives_yrs, color=biopsy)) + geom_point()  + facet_grid(iud~smokes)


# Potentially useful
ggplot(df, aes(x=age, y=hormonal_contraceptives_yrs, color=biopsy)) + geom_point()  + facet_grid(stds~smokes)

# Potentially useful: Need to find what explains high values for Hormonal contraceptives for healthy patients.
ggplot(df, aes(x=iud_yrs, y=hormonal_contraceptives_yrs, color=biopsy)) + geom_point() 


 
# The following plot does a decent job of separating stuff, but there are still patients with high number of IUD years that don't have
# the cancer. What can explain a high number of IUD years?
ggplot(df, aes(x=iud_yrs, y=hormonal_contraceptives_yrs, color=biopsy)) + geom_point() + facet_grid(smokes~.)

#Potentially useful
ggplot(df, aes(x=hormonal_contraceptives_yrs, y=num_pregnancies, color=biopsy)) + geom_point()  + facet_grid(stds~.)

ggplot(df, aes(x=num_pregnancies, y=hormonal_contraceptives_yrs, color=biopsy)) + geom_point()  + facet_grid(iud~smokes)

ggplot(df, aes(x=last_time_std_diag, y=age, fill=biopsy)) + geom_boxplot()  + facet_grid(stds~.)  # interesting



