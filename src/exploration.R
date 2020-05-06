library("ggplot2")
library("ggthemr")
ggthemr("fresh")
source("src/clean.R")


df = get.titanic.data()

# Appears that more females died.
ggplot(df, aes(x=Survived, fill=Sex)) + geom_bar()

# In first class, the ones that survived were overwhelmingly men
ggplot(df, aes(x=Survived, fill=Sex)) + geom_bar() + facet_grid(.~Pclass)

# For people who died, the distributions between male and female look similar
# For people who survived, the men seem to be older than females
ggplot(df, aes(x=Survived, y=Age, fill=Sex)) + geom_boxplot()


# Higher values of Parch combined with low values of Sibsp seem to indicate better survival
# Similarly, higher values of SipSp with lower values of parch survived more.
ggplot(df, aes(x=Parch, y=SibSp, color=Survived))  + geom_point()


ggplot(df, aes(x=SibSp, y=Parch, color=Survived)) + geom_point() + facet_grid(.~Embarked)
