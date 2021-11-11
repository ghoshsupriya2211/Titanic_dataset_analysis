setwd("C:\\Supriya_docs\\OneDrive - GrayMars IoT Solutions Pvt Ltd\\Supriya\\Rprog_samples\\KAGGLE\\Titanic")
# Read train data
titanic_train <- read.csv("train.csv",stringsAsFactors = FALSE)
# Read test data
titanic_test <- read.csv("test.csv",stringsAsFactors = FALSE)
# Details and summary of train and test data
str(titanic_train)
summary(titanic_train)
str(titanic_test)
summary(titanic_test)

# Sex vs Survived plot
ggplot(titanic_train, aes(Sex, fill = factor(Survived))) + 
  geom_bar(stat = "count")+ xlab("Sex") + ylab("Count") + ggtitle("Sex vs Survived")
table(titanic_train$Survived,titanic_train$Sex)
# Female survival rate
Female_survival <- 233/(81+233) # approx 74% of female survived
# Male survival rate
Male_survival <- 109/(468+109) # approx 19% of male survived
prop.table(table(titanic_train$Survived,titanic_train$Sex)) # approx 26% female &  12% male survived  
table(titanic_train$Sex =="female")
prop.table(table(titanic_train$Sex =="female")) # Out of total passengers, approx 35% is female &
# 65% is male

# Pclass vs Sex vs Survived plot
ggplot(titanic_train, aes(Pclass, fill = factor(Survived))) + 
  geom_bar(stat = "count")+ xlab("Pclass") + facet_grid(.~Sex)+ ylab("Count") +
 ggtitle("Pclass vs Sex vs Survived")




prop.table(table(titanic_train$Survived, titanic_train$Embarked))
prop.table(table(titanic_train$Survived, titanic_train$Pclass))
prop.table(table(titanic_train$Survived, titanic_train$SibSp))
prop.table(table(titanic_train$Survived, titanic_train$Parch))
sum(is.na(which(titanic_train$Age < mean(titanic_train$Age,na.rm = TRUE))))
mean(titanic_train$Age,na.rm = TRUE)
titanic_train$Age[titanic_train$Age > 30]
table(titanic_train$Survived[titanic_train$Survived==1], titanic_train$Age[titanic_train$Age > 30])
prop.table(table((titanic_train$Age >=20) & (titanic_train$Survived ==1)))
prop.table(table((titanic_train$Age < 1) & (titanic_train$Survived ==1)))
table(titanic_train$Age < 1)
