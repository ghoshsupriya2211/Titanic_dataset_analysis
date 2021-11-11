
library(ggplot2)
library(mice)
library(dplyr)
# Read train data
titanic_train <- read.csv("train.csv",stringsAsFactors = FALSE)
# Read test data
titanic_test <- read.csv("test.csv",stringsAsFactors = FALSE)
# Details and summary of train and test data
str(titanic_train)
summary(titanic_train)
str(titanic_test)
summary(titanic_test)
titanic_test$Survived <- NA
# combine train and test dataset for common analysis
Full_titanic <- rbind(titanic_train,titanic_test)
str(Full_titanic)
summary(Full_titanic)

#Categorical Features Plots

# Survived plot
ggplot(Full_titanic, aes(Survived, fill = factor(Survived))) + 
  geom_bar(stat = "count")+ xlab("Survived") + ylab("Count") + ggtitle("Survived")

table(Full_titanic$Survived)
prop.table(table(Full_titanic$Survived)) # overall approx 38% of people survived 

# Sex vs Survived plot
ggplot(Full_titanic, aes(Sex, fill = factor(Survived))) + 
  geom_bar(stat = "count")+ xlab("Sex") + ylab("Count") + ggtitle("Sex vs Survived")

table(Full_titanic$Survived,Full_titanic$Sex)
prop.table(table(Full_titanic$Survived,Full_titanic$Sex)) # approx 26% of female survived and
# 12% of male survived out of total

# Pclass vs Survived plot
ggplot(Full_titanic, aes(Pclass, fill = factor(Survived))) + 
  geom_bar(stat = "count")+ xlab("Pclass") + ylab("Count") + ggtitle("Pclass vs Survived")

table(Full_titanic$Survived, Full_titanic$Pclass)
prop.table(table(Full_titanic$Survived, Full_titanic$Pclass)) # approx 15% survived in class 1, 
# 10% in class 2 and 13% in class 3

# Pclass vs Sex vs Survived plot
ggplot(Full_titanic, aes(Pclass, fill = factor(Survived))) + 
  geom_bar(stat = "count")+ xlab("Pclass") + facet_grid(.~Sex)+ ylab("Count") +
  ggtitle("Pclass vs Sex vs Survived")

table(Full_titanic$Survived,Full_titanic$Pclass,Full_titanic$Sex)
prop.table(table(Full_titanic$Survived,Full_titanic$Pclass,Full_titanic$Sex)) #10% of class 1 female,
# 8% of class 2 and 8% of class 3 female survived.
# 5% of class 1, 2 % of class 2 and 5% of class 3 male survived

table(Full_titanic$Survived[Full_titanic$Sex=="female"],Full_titanic$Pclass[Full_titanic$Sex=="female"])
prop.table(table(Full_titanic$Survived[Full_titanic$Sex=="female"],
                 Full_titanic$Pclass[Full_titanic$Sex=="female"])) # out of females 29% class 1,22% class 2 
# and 23% class 3 survived

table(Full_titanic$Survived[Full_titanic$Sex=="male"],Full_titanic$Pclass[Full_titanic$Sex=="male"])
prop.table(table(Full_titanic$Survived[Full_titanic$Sex=="male"],
                 Full_titanic$Pclass[Full_titanic$Sex=="male"])) # out of males 7% class 1,3% of class 2 
# and 8% of class 3 survived

table(Full_titanic$Survived[Full_titanic$Pclass=="1"],Full_titanic$Pclass[Full_titanic$Pclass=="1"])
prop.table(table(Full_titanic$Survived[Full_titanic$Pclass=="1"],
                 Full_titanic$Pclass[Full_titanic$Pclass=="1"])) # 63 % of class 1 passenger survived

table(Full_titanic$Survived[Full_titanic$Pclass=="2"],Full_titanic$Pclass[Full_titanic$Pclass=="2"])
prop.table(table(Full_titanic$Survived[Full_titanic$Pclass=="2"],
                 Full_titanic$Pclass[Full_titanic$Pclass=="2"])) # 47 % of class 2 passenger survived

table(Full_titanic$Survived[Full_titanic$Pclass=="3"],Full_titanic$Pclass[Full_titanic$Pclass=="3"])
prop.table(table(Full_titanic$Survived[Full_titanic$Pclass=="3"],
                 Full_titanic$Pclass[Full_titanic$Pclass=="3"])) # 24 % of class 2 passenger survived

#MISSING VALUE TREATMENT
# Identifying NA and blank in the dataset
# NA in fare column replaced with median value of the class
which(is.na(Full_titanic$Fare))
Full_titanic[1044,]
Full_titanic$Fare[(Full_titanic$Pclass ==3 & Full_titanic$Embarked =="S")]
Full_titanic$Fare[1044] <- median(Full_titanic[Full_titanic$Pclass == '3', ]$Fare, na.rm = TRUE)
# missing value in Embarked column replaced with closest value of embarked
# 2 missing datas for Embarked
Full_titanic[c(62, 830), 'Embarked']
Full_titanic$Embarked[c(62, 830)] <- 'S'
#NA in age column
summary(is.na(Full_titanic$Age))
summary(Full_titanic$Age)
#Mice Imputation for missing age data
MissAge <- mice(data = Full_titanic[,-c(1,10)],m=10,maxit=20,meth='pmm')
MissComplete <- complete(MissAge)
str(MissComplete)
summary(MissComplete$Age)
summary(Full_titanic$Age)
Full_titanic$Age <- MissComplete$Age
head(Full_titanic,1)
str(Full_titanic)

# Embarked vs Survived plot
ggplot(Full_titanic, aes(Embarked, fill = factor(Survived))) + 
  geom_bar(stat = "count")+ xlab("Embarked") + ylab("Count") + ggtitle("Embarked vs Survived")

table(Full_titanic$Survived,Full_titanic$Embarked)
prop.table(table((Full_titanic$Survived),Full_titanic$Embarked)) # 10% of "C" , 3% of "Q"
# and 24% of "S" onboard survived.

table(Full_titanic$Survived[Full_titanic$Sex == "female"],
      Full_titanic$Embarked[Full_titanic$Sex == "female"])
prop.table(table(Full_titanic$Survived[Full_titanic$Sex == "female"],
                 Full_titanic$Embarked[Full_titanic$Sex == "female"])) # 20% of female at "c",
# 8 % at "Q" and 45% at "S" survived
#Also i could see out of total females onboarded at "C", 88% survived,out of total females 
# onboarded at "Q", 75% survived and out of total females onboarded at "S", 69% survived 


table(Full_titanic$Survived[Full_titanic$Sex == "male"],
      Full_titanic$Embarked[Full_titanic$Sex == "male"])
prop.table(table(Full_titanic$Survived[Full_titanic$Sex == "male"],
                 Full_titanic$Embarked[Full_titanic$Sex == "male"])) # 5% of male at "c",
# 0.5 % at "Q" and 13% at "S" survived
# Similarly for males, out of total onboarded at "C", 31% survived,out of total onboarded at 
# "Q", 2.4% survived and out of total onboarded at "S", 17% survived

# Age vs Survived plot
ggplot(Full_titanic, aes(Age, fill = factor(Survived))) + geom_histogram(bins=30) + 
xlab("Age") + ggtitle("Age vs Survived")

#Sex vs Survived vs Age 
ggplot(Full_titanic, aes(Age, fill = factor(Survived))) + 
  geom_histogram(bins=30) + xlab("Age") + ylab("Count") + facet_grid(.~Sex)+
  ggtitle("Age vs Sex vs Survived")

sum(table(Full_titanic$Survived[Full_titanic$Sex =="female"],
          Full_titanic$Age[Full_titanic$Sex =="female"]))

sum(table(Full_titanic$Survived[Full_titanic$Sex =="female" & Full_titanic$Survived == "1"],
          Full_titanic$Age[Full_titanic$Sex =="female" & Full_titanic$Survived == "1"]))

#  Pclass vs Sex vs Age
ggplot(Full_titanic, aes(x = Age, y = Sex)) + 
  geom_jitter(aes(colour = factor(Survived))) + theme(legend.title = element_blank())+
  facet_wrap(~Pclass) + 
  labs(x = "Age", y = "Sex", title = "Pclass vs Sex vs Age vs Survived")+
  scale_fill_discrete(name = "Survived") + 
  scale_x_continuous(name="Age",limits=c(0, 81))

# Fare vs Pclass
ggplot(Full_titanic, aes(x = Fare, y = Pclass)) + 
  geom_jitter(aes(colour = factor(Survived))) + theme(legend.title = element_blank())+
  labs(x = "Age", y = "Pclass", title = "Fare vs Pclass")+
  scale_fill_discrete(name = "Survived") + 
  scale_x_continuous(name="Fare", limits=c(0, 270), breaks=c(0, 40, 80, 120, 160, 200, 240, 280))

# Data Processing and Further Exploratory Analysis
#Feature engineering
# New Variable : Title (From Name)
Full_titanic$Title <- gsub('(.*, )|(\\..*)', '', Full_titanic$Name)
# Titles by Sex
table(Full_titanic$Sex, Full_titanic$Title)
# Reassign rare titles
officer <- c('Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev')
royalty <- c('Dona', 'Lady', 'the Countess','Sir', 'Jonkheer')
# Reassign mlle, ms, and mme, and rare
Full_titanic$Title[Full_titanic$Title == 'Mlle'] <- 'Miss' 
Full_titanic$Title[Full_titanic$Title == 'Ms']  <- 'Miss'
Full_titanic$Title[Full_titanic$Title == 'Mme'] <- 'Mrs' 
Full_titanic$Title[Full_titanic$Title %in% royalty]  <- 'Royalty'
Full_titanic$Title[Full_titanic$Title %in% officer]  <- 'Officer'
Full_titanic$Surname <- sapply(Full_titanic$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])


#Title vs Survived plot
ggplot(Full_titanic, aes(Title,fill = factor(Survived))) + geom_bar(stat = "count") + 
xlab('Title') + ylab("Count") + scale_fill_discrete(name = " Survived") + 
  ggtitle("Title vs Survived")

# New Variable : Family Size (From Name, SibSp and Parch)
# Family Size
Full_titanic$Fsize <- Full_titanic$SibSp + Full_titanic$Parch + 1
ggplot(Full_titanic, aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) + xlab('Family Size') + ylab("Count") + 
  ggtitle("Family Size vs Survived")
# FsizeD
Full_titanic$FsizeD[Full_titanic$Fsize == 1] <- 'Alone'
Full_titanic$FsizeD[Full_titanic$Fsize <= 5 & Full_titanic$Fsize > 1] <- 'Small'
Full_titanic$FsizeD[Full_titanic$Fsize > 5] <- 'Big'
table(Full_titanic$Survived, Full_titanic$FsizeD)

mosaicplot(table(Full_titanic$FsizeD, Full_titanic$Survived), main='FsizeD vs Survived', 
           ylab="Survived",xlab="FsizeD",col = hcl(c(50, 120)),)

# New Variable : Child (From Age)
Full_titanic$Child[Full_titanic$Age <= 12] <- '1'
Full_titanic$Child[Full_titanic$Age > 12] <- '0'
# Age <= 12 (Child ) = 1 and Age > 12(Adult) = 0

ggplot(Full_titanic[Full_titanic$Child == 'Child', ], aes(Sex, fill = factor(Survived))) + 
  geom_bar(stat = "count") + xlab("Sex") + ylab("Count") + facet_wrap(~Pclass) +
  scale_fill_discrete(name = "Survived") +
  ggtitle("Child vs Sex vs Pclass vs Survived")

table(Full_titanic$Child, Full_titanic$Survived)

# New Variable : Mother (From Title)
Full_titanic$Mother = ifelse(Full_titanic$Title=="Mrs" & Full_titanic$Parch > 0, 1,0)

# New Variable : Single (From SibSp and Parch)
Full_titanic$Single = ifelse(Full_titanic$SibSp + Full_titanic$Parch + 1 == 1, 1,0) 
# People travelling alone

# New Variable : Ticket_Type (Unique and Duplicate)
duplicated(Full_titanic$Ticket)
DuplicateTickets <- Full_titanic[duplicated(Full_titanic$Ticket) | rev(duplicated(rev(Full_titanic$Ticket))),]
Full_titanic[duplicated(Full_titanic$Ticket)| rev(duplicated(rev(Full_titanic$Ticket))),]$Ticket
table(DuplicateTickets$Ticket)
nrow(DuplicateTickets)


!duplicated(Full_titanic$Ticket) & rev(!duplicated(rev(Full_titanic$Ticket)))
UniqueTickets <- Full_titanic[!duplicated(Full_titanic$Ticket) & rev(!duplicated(rev(Full_titanic$Ticket))),]
Full_titanic[!duplicated(Full_titanic$Ticket) & rev(!duplicated(rev(Full_titanic$Ticket))),]$Ticket
table(UniqueTickets$Ticket)
nrow(UniqueTickets)


Full_titanic$TicketType <- ifelse(Full_titanic$Ticket == 
  (Full_titanic$Ticket[match(Full_titanic$Ticket,
 UniqueTickets$Ticket)]), "Duplicate","Unique")
table(Full_titanic$TicketType == "Unique")
Full_titanic$TicketType[1] <- "Unique"

Full_titanic$TicketType[is.na(Full_titanic$TicketType)] <- "Duplicate"
table(Full_titanic$TicketType,Full_titanic$Survived)



# New Variable : Cabin Availability

x <- c("A","B","C","D","E","F","G","T")
cabinAllotted <- Full_titanic[grepl(paste(x, collapse = "|"), Full_titanic$Cabin),]
prop.table(table(cabinAllotted$Survived)) # 67% with cabin allotted survived and 33% died
nrow(cabinAllotted)

Nocabin <- Full_titanic[!grepl(paste(x, collapse = "|"), Full_titanic$Cabin),]
nrow(Nocabin)
prop.table(table(Nocabin$Survived)) # 30% with no cabin survived and 70% died
nrow(Nocabin)

Full_titanic$CabinAvailability <- ifelse((Full_titanic$Cabin == 
Full_titanic$Cabin[match(Full_titanic$Cabin,
                         cabinAllotted$Cabin)]), "No","Yes")

Full_titanic$CabinAvailability[is.na(Full_titanic$CabinAvailability)] <- "No"
table(Full_titanic$CabinAvailability)

# Encoding and conversion into numeric
library(plyr)
str(Full_titanic)
Full_titanic$Sex <- revalue(Full_titanic$Sex, 
        c("male" = 1, "female" = 2))
Full_titanic$Sex <- as.numeric(Full_titanic$Sex)
Full_titanic$FsizeD <- revalue(Full_titanic$FsizeD, 
                               c("Alone" = 1, "Small" = 2, "Big" = 3))
Full_titanic$FsizeD <- as.numeric(Full_titanic$FsizeD)
Full_titanic$Child <- as.numeric(Full_titanic$Child)
Full_titanic$TicketType <- revalue(Full_titanic$TicketType, 
                            c("Duplicate" = 1, "Unique" = 2))
Full_titanic$TicketType <- as.numeric(Full_titanic$TicketType)
Full_titanic$CabinAvailability <- revalue(Full_titanic$CabinAvailability, 
                                   c("Yes" = 1, "No" = 2))
Full_titanic$CabinAvailability <- as.numeric(Full_titanic$CabinAvailability)

Full_titanic$Embarked <- revalue(Full_titanic$Embarked, 
                              c("S" = 1, "Q" = 2, "C" = 3))
Full_titanic$Embarked <- as.numeric(Full_titanic$Embarked)

Full_titanic$Title <- revalue(Full_titanic$Title, 
                           c("Mr" = 1, "Master" = 2,"Officer" = 3, 
                             "Mrs" = 4,"Royalty" = 5,"Miss" = 6))
Full_titanic$Title <- as.numeric(Full_titanic$Title)
str(Full_titanic)
Titanic_FinalSet <- Full_titanic[,c("PassengerId","Survived","Pclass","Sex","Age","Embarked","Fare",
                                    "Title","FsizeD","Child","Mother","TicketType",
                                    "CabinAvailability")]
# Conversion int into factor as required for categorical features
Titanic_FinalSet$Child <- factor(Titanic_FinalSet$Child)
Titanic_FinalSet$Survived <- factor(Titanic_FinalSet$Survived)
Titanic_FinalSet$Pclass <- factor(Titanic_FinalSet$Pclass)
Titanic_FinalSet$Sex <- factor(Titanic_FinalSet$Sex)
Titanic_FinalSet$Embarked <- factor(Titanic_FinalSet$Embarked)
Titanic_FinalSet$Title <- factor(Titanic_FinalSet$Title)
Titanic_FinalSet$FsizeD <- factor(Titanic_FinalSet$FsizeD)
Titanic_FinalSet$Child <- factor(Titanic_FinalSet$Child)
Titanic_FinalSet$Mother <- factor(Titanic_FinalSet$Mother)
#Titanic_FinalSet$Single <- factor(Titanic_FinalSet$Single)
Titanic_FinalSet$TicketType <- factor(Titanic_FinalSet$TicketType)
Titanic_FinalSet$CabinAvailability <- factor(Titanic_FinalSet$CabinAvailability)
str(Titanic_FinalSet)

# Identifying correlation and developing Plot
library(corrplot)
matrix_corr_data <- cor(Titanic_FinalSet)
corrplot(matrix_corr_data,method="circle")

# Modeling with Random Forest
# Split dataset into train and test
train <- Titanic_FinalSet[1:891,]
test <- Titanic_FinalSet[892:1309,]

# random forest
library(caret)
library(randomForest)
library(ranger)
# set.seed(123)
# rf_model <- randomForest(Survived ~., data = train)

# set.seed(123)
# rf_model <- randomForest(Survived ~., data = train, importance = TRUE, ntree=2000)

#Creating the model
set.seed(123)
rf_model <- randomForest(Survived ~., data = train, method = "rf",
                      mtry = 5, importance = TRUE, ntree=2000)
plot(rf_model)
ntrees <- which.min(rf_model$err.rate[,1])

set.seed(123)
rf_model <- randomForest(Survived ~., data = train, method = "rf",
                         mtry = 5, importance = TRUE, ntree=ntrees)


z <- confusionMatrix(rf_model$predicted, train$Survived)

#Error rate when predicting death: 8.7%
# Error rate when predicting survival: 27.8%
# Accuracy is 83.95 % , Sensitivity : 0.9153, Specificity : 0.7222


# set.seed(123)
# my_control <- trainControl(method = "cv", number = 10)
# tgrid <- expand.grid(.mtry = c(3:10), .splitrule = "variance",.min.node.size = c(10,15,20))
# rf_model <- randomForest(Survived ~., data = train, method = "rf", trControl = my_control,  
#                          importance = TRUE, ntree=2000, 
#                          tuneGrid = tgrid)

# set.seed(123)
# repeatedCV <- trainControl(method = "repeatedcv",
#                            number = 5,
#                            repeats = 5)
# rf_grid <- expand.grid(mtry = seq(from = 2, to = ncol(rf_model) - 1, by = 1))
# rf_model <- randomForest(Survived ~., data = train,
#                     method = "rf", 
#                     trControl = repeatedCV, 
#                     importance = TRUE, 
#                     tuneGrid = rf_grid)

# fitting the model to test data
rf_model_test <- predict(rf_model, newdata = test, type = "class")
summary(rf_model_test)

# columns (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = rf_model_test)
table(solution$Survived)

# .csv
write.csv(solution, file = 'rf_model_sol.csv', row.names = F)


# # "tpr", "fpr"
# library(ROCR)
# rf_model_predict <- predict(rf_model, test, type="prob")
# predROC <- prediction(rf_model_predict[,2],test$Survived)
# perfROC <- performance(predROC, "tpr", "fpr")
# 
# plot(perfROC)
# abline(a=0, b=1)
# # Area under curve
# perfROC <- performance(predROC, "auc")
# perfROC@y.values[[1]]

# Error
plot(rf_model, ylim=c(0,0.36), main = 'RF_MODEL')
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

#variable importance
importance <- importance(rf_model)
?importance

varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# var imp
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Graph var importantes
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

set.seed(1)
install.packages("adabag")
library(adabag)
titanic.bag <- bagging(Survived ~ ., data = train)
confusionMatrix(as.factor(titanic.bag$class), train$Survived)

pred.bag <- predict(titanic.bag, test, type = "response")
pred.bag$

# bagging model
# columns (prediction)
solution1 <- data.frame(PassengerID = test$PassengerId, Survived = pred.bag$class)
table(solution1$Survived)

# .csv
write.csv(solution1, file = 'bagging_model_sol.csv', row.names = F)


















 





















