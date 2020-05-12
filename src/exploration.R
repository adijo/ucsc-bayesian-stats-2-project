library("ggplot2")
library("ggthemr")
ggthemr("light")
source("src/clean.R")


df = get.titanic.data()

summary(df)


ageGroups = c("Young", "Young Adult", "Adult", "Old")
df$AgeBinned = cut(df$Age, 4, labels = ageGroups)

for(ageGroup in ageGroups){
  print(nrow(filter(df, AgeBinned == ageGroup & Survived == "Survived")) / nrow(filter(df, AgeBinned == ageGroup)))
}

# Appears that more females died.
ggplot(df, aes(x=Survived, fill=Sex)) + 
  geom_bar() + 
  xlab("Survival Status") + 
  ylab("Count") + 
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = 0.5)) + 
  scale_x_discrete(labels = c("Survived" = "Survived (n = 424)", "Dead" = "Dead (n = 288)")) + 
  labs(title = "Survival Rate by Gender", subtitle = "Males have a higher rate of survival compared to females")
  

ggplot(df, aes(x=Age, y=Parch + SibSp, color=Survived, shape=Sex)) + 
  geom_point(alpha=1/50) + 
  facet_grid(Pclass~Embarked) + 
  geom_jitter(height=0.5, width=0.5) + 
  ylab("Family size  (Sum of Parch and SibSp)") + 
  xlab("Age (years)") +  
  guides(shape = guide_legend(override.aes=list(color="black"))) +
  labs(title = "Survival Status Across Dimensions", 
       subtitle = "Chosen features show a fair predictive ability as the two survival classes appear as reasonably distinct points")
