
get.titanic.data = function() {
  
  # Drop unimportant columns (for now)
  df <- subset(titanic::titanic_train, select = -c(PassengerId, Name, Cabin, Fare, Ticket))
 
   # Omit NA's
  df = na.omit(df, target.colnames = c("Age", "Embarked"))
  
  # Convert to factors
  df$Survived = factor(df$Survived, levels = c(0, 1), labels = c("Survived", "Dead"))
  df$Sex = factor(df$Sex, levels = c("male", "female"), labels = c("Male", "Female"))
  df$Embarked = factor(df$Embarked, levels = c("S", "C", "Q"), labels = c("Southampton", "Cherbourg", "Queenstown"))
  df$Pclass = factor(df$Pclass, levels = c(1, 2, 3), labels = c("1st Class", "2nd Class", "3rd Class"))
 
  na.omit(df)
}
