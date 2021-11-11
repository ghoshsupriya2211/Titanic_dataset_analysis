
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
#MISSING VALUE TREATMENT
# Identifying NA in the dataset
# NA in fare column replaced with median value of the class
which(is.na(Full_titanic$Fare))
Full_titanic[1044,]
Full_titanic$Fare[(Full_titanic$Pclass ==3 & Full_titanic$Embarked =="S")]
Full_titanic$Fare[1044] <- median(Full_titanic[Full_titanic$Pclass == '3', ]$Fare, na.rm = TRUE)
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

# Sex vs Survived plot
ggplot(titanic_train, aes(Sex, fill = factor(Survived))) + 
  geom_bar(stat = "count")+ xlab("Sex") + ylab("Count") + ggtitle("Sex vs Survived")

table(titanic_train$Survived,Full_titanic$Sex)

# Pclass vs Sex vs Survived plot
ggplot(titanic_train, aes(Pclass, fill = factor(Survived))) + 
  geom_bar(stat = "count")+ xlab("Pclass") + facet_grid(.~Sex)+ ylab("Count") +
 ggtitle("Pclass vs Sex vs Survived")

# Age vs Survived
ggplot(titanic_train, aes(Age, fill = factor(Survived))) + 
  geom_histogram(bins=30) + xlab("Age") + ggtitle("Age vs Survived")

# Age vs Sex vs Survived
ggplot(titanic_train, aes(Age, fill = factor(Survived))) + geom_histogram(bins=30) + 
  xlab("Age") + ylab("Count") + facet_grid(.~Sex)+ ggtitle("Age vs Sex vs Survived")

# Pclass vs Sex vs Age
ggplot(titanic_train, aes(x = Age, y = Sex)) + geom_jitter(aes(colour = factor(Survived))) + 
  theme(legend.title = element_blank())+ facet_wrap(~Pclass) + 
  labs(x = "Age", y = "Sex", title = "Pclass vs Sex vs Age vs Survived") +
scale_x_continuous(name="Age",limits=c(0, 81))

# Fare vs Pclass vs Survived
ggplot(titanic_train, aes(x = Fare, y = Pclass)) + geom_jitter(aes(colour = factor(Survived))) + 
  theme(legend.title = element_blank())+labs(x = "Age", y = "Pclass", title = "Fare vs Pclass")+
 scale_x_continuous(name="Fare", limits=c(0, 270), breaks=c(0, 40, 80, 120, 160, 200, 240, 280))

# Adding New Variable : Family Count (FCount = 1 + SibSp + Parch)
Full_titanic$FCount <- Full_titanic$SibSp + Full_titanic$Parch + 1
# plot(Family count vs survived)
  ggplot(Full_titanic, aes(x = FCount, fill = factor(Survived))) +
    geom_bar(stat='count', position='dodge') +
    scale_x_continuous(breaks=c(1:11)) +
    xlab('Family Count') + ylab("Count") + scale_fill_discrete(name = "Survived") + 
    ggtitle("Family Count vs Survived")
  
Full_titanic$FCountD[Full_titanic$FCount == 1] <- 0
Full_titanic$FCountD[Full_titanic$FCount < 5 & Full_titanic$FCount > 1] <- 1
Full_titanic$FCountD[Full_titanic$FCount > 4] <- 2

mosaicplot(table(Full_titanic$FCountD, Full_titanic$Survived), main='FCountD vs Survived', 
           ylab="Survived",xlab="FCountD",col = hcl(c(50, 120)),)

# 2 missing datas for Embarked
Full_titanic[c(62, 830), 'Embarked']
Full_titanic$Embarked[c(62, 830)] <- 'S'
# PClass vs Embarked vs Survived
ggplot(Full_titanic, aes(Embarked, fill = factor(Survived))) + 
  geom_bar(stat = "count") + xlab("Embarked") + ylab("Count") + facet_wrap(~Pclass) + 
  scale_fill_discrete(name = "Survived") + ggtitle("Embarked vs Pclass vs Survived")

# x <- Full_titanic$Cabin[Full_titanic$Cabin == "\"]
# which(sapply(Full_titanic$Cabin,1,function(x) any(grepl("A", x))))
# which(sapply(Full_titanic$Cabin, function(x) any(x = "A")))
x <- c("A","B","C","D","E","F","G","T")
cabinreturn <- Full_titanic[grepl(paste(x, collapse = "|"), Full_titanic$Cabin),]
prop.table(table(cabinreturn$Survived)) # 67% with cabin allotted survived and 33% died
nrow(cabinreturn)

Nocabin <- Full_titanic[!grepl(paste(x, collapse = "|"), Full_titanic$Cabin),]
nrow(Nocabin)
prop.table(table(Nocabin$Survived)) # 30% with no cabin survived and 70% died

sum(table(Full_titanic$Age[Full_titanic$Age <= 12]))
sum(table(Full_titanic$Age[Full_titanic$Age <= 12 & Full_titanic$Survived==1]))

sum(table(Full_titanic$Age[Full_titanic$Age > 12 & Full_titanic$Sex=="female"]))
sum(table(Full_titanic$Age[Full_titanic$Age > 12 & Full_titanic$Survived==1]))



duplicated(Full_titanic$Ticket)
notdistinct <- Full_titanic[duplicated(Full_titanic$Ticket)| rev(duplicated(rev(Full_titanic$Ticket))),]
nrow(notdistinct[notdistinct$Survived =="1",])
nrow(notdistinct)
sum(summary(as.factor(notdistinct$Ticket)))

!(duplicated(Full_titanic$Ticket))
distinct <- Full_titanic[!(duplicated(Full_titanic$Ticket)) & rev(!duplicated(rev(Full_titanic$Ticket))),]
nrow(distinct[distinct$Survived =="1",])
nrow(distinct)
#mother child
# single sibling












