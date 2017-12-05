##Survival on the basis of below categories
## 1. on basis of Pclass: chances are uper class was saved more i.e 1
## 2. on the basis of gender: Females survival chances are more
## 3. Age: Kids saves more
## 4. Family: Family persons save more

library(dplyr)
library(ggplot2)
library(caret)
#Read train csv
train<-read.csv("D:/Data Science/Titanic/Data Set/train.csv")
#Read test csv
test<-read.csv("D:/Data Science/Titanic/Data Set/test.csv")
#Add coloumn Survives in test dataset
test$Survived<-NA

# combine train and test dataset to a full_titanic dataset
full_titanic=rbind(train,test)

str(full_titanic)

#To find Na values in full_titanic
colSums(is.na(full_titanic))

#To find blank values in full_titanic
colSums(full_titanic=='')

#To get count of possible values in Embarked column
table(full_titanic$Embarked)

# assign most count value 'S' to blank values in Embarked
full_titanic$Embarked[full_titanic$Embarked=='']="S"

table(full_titanic$Embarked)


###Check the length and see how many varibles of them we can move to factor for our analysis

apply(full_titanic,2, function(x) length(unique(x)))

##Convert below in to factors
cols=c("Survived","Pclass","Sex","Embarked")
for(i in cols){
  full_titanic[,i]<- as.factor(full_titanic[,i])
}
  
str(full_titanic)

#Explotary analysis on PClass
ggplot(full_titanic[1:891,], aes(x= Pclass,fill=factor(Survived))) + 
  geom_bar(aes(fill=factor(Survived)))  + 
  ggtitle("Pclass vs Survival Rate")+
  xlab("Pclass") +
  ylab("Total count") +
  labs(fill= "Survived")

#Explorary analysis on basis of sex

ggplot(full_titanic[1:891,],aes(x=Sex, fill= factor(Survived)))+
  geom_bar(aes(fill=factor(Survived)))+
  ggtitle("Sex vs Survival Rate")+
  xlab("Sex") +
  ylab("Total count") +
  labs(fill= "Survived")

#3D analysis of Sex, Pclass and Survival Rate
ggplot(full_titanic[1:891,],aes(x=Sex, fill= factor(Survived)))+
  geom_bar(aes(fill=factor(Survived)))+
  facet_wrap(~Pclass)+
  ggtitle("3D view of Sex, Pclass and Survival Rate")+
  xlab("Sex") +
  ylab("Total count") +
  labs(fill= "Survived")

##So In the all the class female Survival rate is better than Men



##Exploratory Analysis on Title

#Extract title of Names
full_names<-full_titanic$Name

title <- gsub('(.*, )|(\\..*)', '', full_names)

full_titanic$title<-title

table(full_titanic$title)


# Group in big basket
full_titanic$title[full_titanic$title == 'Mlle']        <- 'Miss' 
full_titanic$title[full_titanic$title == 'Ms']          <- 'Miss'
full_titanic$title[full_titanic$title == 'Mme']         <- 'Mrs' 
full_titanic$title[full_titanic$title == 'Lady']          <- 'Miss'
full_titanic$title[full_titanic$title == 'Dona']          <- 'Miss'


full_titanic$title[full_titanic$title == 'Capt']        <- 'Officer' 
full_titanic$title[full_titanic$title == 'Col']        <- 'Officer' 
full_titanic$title[full_titanic$title == 'Major']   <- 'Officer'
full_titanic$title[full_titanic$title == 'Dr']   <- 'Officer'
full_titanic$title[full_titanic$title == 'Rev']   <- 'Officer'
full_titanic$title[full_titanic$title == 'Don']   <- 'Officer'
full_titanic$title[full_titanic$title == 'Sir']   <- 'Officer'
full_titanic$title[full_titanic$title == 'the Countess']   <- 'Officer'
full_titanic$title[full_titanic$title == 'Jonkheer']   <- 'Officer'
table(full_titanic$title)


#Survival Rate on the basis of titles

ggplot(full_titanic[1:891,],aes(x=title,fill= factor(Survived)))+
  geom_bar(aes(fill=factor(Survived)))+
  ggtitle("Titles vs Survival Rate")+
  xlab("Titles")+
  ylab("Total Count")+
  labs(fill="Survived")

### Visualize the 3-way of relationship of Title, Pclass, and Survival
ggplot(full_titanic[1:891,],aes(x=title,fill= factor(Survived)))+
  geom_bar(aes(fill=factor(Survived)))+
  facet_wrap(~Pclass)
  ggtitle("Titles vs Survival Rate")+
  xlab("Titles")+
  ylab("Total Count")+
  labs(fill="Survived")
  
##Master in 1st and 2nd class has 100% Survival where has Mrs and Miss having 90% chance of Survival in 1st and 2nd class 
##Since Title mostly depending on Age (except few cases), I will use title in place of age which has 263 missing observation
  
##Exploratory Analysis on Family
  
# Lets create a Family size using Sibsp and Parch

full_titanic$FamilySize<- full_titanic$SibSp +full_titanic$Parch +1

table(full_titanic$FamilySize)

full_titanic$FamilySized[full_titanic$FamilySize == 1]   <- 'Single'
full_titanic$FamilySized[full_titanic$FamilySize<5 & full_titanic$FamilySize>=2]<-'small'
full_titanic$FamilySized[full_titanic$FamilySize >=5]   <- 'Big'

full_titanic$FamilySized=as.factor(full_titanic$FamilySized)

table(full_titanic$FamilySized)


#Survival Rate on the basis of family size

ggplot(full_titanic[1:891,],aes(x=FamilySized,fill= factor(Survived)))+
  geom_bar(aes(fill=factor(Survived)))+
  ggtitle("Family Size vs Survival Rate")+
  xlab("Family Size")+
  ylab("Total Count")+
  labs(fill="Survived")


#3d analysis of survival rate on basis of title and family size
# why big size family has problem

ggplot(full_titanic[1:891,],aes(x=FamilySized,fill= factor(Survived)))+
  geom_bar(aes(fill=factor(Survived)))+
  facet_wrap(~title)+
  ggtitle("3d view of title, Family Size and Survival Rate")+
  xlab("Family Size")+
  ylab("Total Count")+
  ylim(0,300) +
  labs(fill="Survived")

###I am very surprised to see Single coming out to be bulk, however there is chance that, they could come with friends or servants
##I though to extract those unique number using same ticket number distributed.

##Engineer features based on all the passengers with the same ticket
ticket.unique <- rep(0, nrow(full_titanic))
tickets <- unique(full_titanic$Ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(full_titanic$Ticket == current.ticket)
  
  
  for (k in 1:length(party.indexes)) {
    ticket.unique[party.indexes[k]] <- length(party.indexes)
  }
}

full_titanic$ticket.unique <- ticket.unique

full_titanic$ticket.size[full_titanic$ticket.unique == 1]   <- 'Single'
full_titanic$ticket.size[full_titanic$ticket.unique < 5 & full_titanic$ticket.unique>= 2]   <- 'Small'
full_titanic$ticket.size[full_titanic$ticket.unique >= 5]   <- 'Big'


##Lets check the Ticket size through grpah
ggplot(full_titanic[1:891,],aes(x = ticket.size,fill=factor(Survived))) +
  geom_bar() +
  ggtitle("ticket.Size VS Survival")+
  xlab("ticket.size") +
  ylab("Total Count") +
  labs(fill = "Survived")


##Lets check the Ticket and title size through grpah
ggplot(full_titanic[1:891,], aes(x = ticket.size, fill = Survived)) +
  geom_bar() +
  facet_wrap(~title) + 
  ggtitle("3D View of Ticket, Title and Survival rate") +
  xlab("ticket.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

##We can't see huge diffrence b/w ticket size and Family Size, May be we will use any one of them which is contributing more

#Exploratory Analysis on Embarked

ggplot(full_titanic[1:819,],aes(x= Embarked, fill=factor(Survived)))+
  geom_bar(aes(fill=factor(Survived)))+
  ggtitle("Embarked vs Survival rate")+
  xlab("Embarked") +
  ylab("Total Count") +
  labs(fill = "Survived")

##Lets further divide the grpah by Pclass
ggplot(full_titanic[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass vs Embarked vs survival") +
  xlab("Embarked") +
  ylab("Total Count") +
  labs(fill = "Survived")

##Haha..I don't think there is a correlation between Survival rate and Embarked 

##There is a lot of Missing value in Cabin, i dont think its good idea to use that
##As mentioned earlier will use Title inplace of Age 
##Fare is definitelly correlate with Pclass..so i am not going to use that too

full_titanic$ticket.size <- as.factor(full_titanic$ticket.size)
full_titanic$title <- as.factor(full_titanic$title)

##From the Explortory anlysis part we have decided to use below variables for our model building 

##"Pclass", "title","Sex","Embarked","FamilySized","ticket.size"

##Any redaundant varible among above will drop in the course of analysis

str(full_titanic)

###lets prepare and keep data in the proper format

feauter1<-full_titanic[1:891,c("Pclass","title" ,"Sex","Embarked", "FamilySized","ticket.size","Survived" )]
#response <- as.factor(train$Survived)
#table(full_titanic[1:891,"Survived"]==train$Survived)

###For Cross validation purpose will keep 20% of data aside from my orginal train set
##This is just to check how well my data works for unseen data
library(caTools)

set.seed(500)
split<- sample.split(feauter1$Survived, SplitRatio = 2/3)
train_val<-subset(feauter1,split==TRUE)
test_val<-subset(feauter1,split==FALSE)

####check the proprtion of Survival rate in orginal training data, current traing and testing data
round(prop.table(table(train$Survived)*100),digits = 1)
round(prop.table(table(train_val$Survived)*100),digits = 1)
round(prop.table(table(test_val$Survived)*100),digits = 1)


#Predictive analysis and cross validation

##Decision Tree
library(rpart)
library(rpart.plot)

set.seed(1234)
Model_DT<- rpart(Survived~. , data=train_val,method = "class")
rpart.plot(Model_DT,extra =  3,fallen.leaves = T)

###Surprise, Check out the plot,  our Single tree model is using only Title, Pclass and Ticket.size and vomited rest
###Lets Predict train data and check the accuracy of single tree
PRE_TDT=predict(Model_DT,data=train_val,type="class")
confusionMatrix(PRE_TDT,train_val$Survived)


set.seed(1234)
cv.10 <- createMultiFolds(train_val$Survived, k = 10, times = 10)

# Control
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                     index = cv.10)

##Train the data
Model_CDT <- train(x = train_val[,-7], y = train_val[,7], method = "rpart", tuneLength = 30,
                   trControl = ctrl)


##Check the accurcay
##Accurcay using 10 fold cross validation of Single tree is 0.8139 
##Seems Overfitted earlier using Single tree, there our accurcay rate is 0.83

# check the variable imporatnce, is it the same as in Single tree?
rpart.plot(Model_CDT$finalModel,extra =  3,fallen.leaves = T)

##Yes, there is no change in the imporatnce of variable


###Lets cross validate the accurcay using data that kept aside for testing purpose
PRE_VDTS=predict(Model_CDT$finalModel,newdata=test_val,type="class")
confusionMatrix(PRE_VDTS,test_val$Survived)

###There it is, How exactly our train data and test data matches in accuracy


###RANDOM fOREST
library(randomForest)
set.seed(1234)
rf.1 <- randomForest(x = train_val[,-7],y=train_val[,7], importance = TRUE, ntree = 1000)
rf.1

varImpPlot(rf.1)

####Random Forest accurcay rate is 82.91 which is 1% better than the decison  tree
####Lets remove 2 redaundant varibles and do the modeling again
train_val1=train_val[,-4:-5]
test_val1=test_val[,-4:-5]

set.seed(1234)
rf.2 <- randomForest(x = train_val1[,-5],y=train_val1[,5], importance = TRUE, ntree = 1000)
rf.2

varImpPlot(rf.2)


###Can see the Magic now, increase in accuracy by just removing 2 varibles, accuracy now is 84.03 

##Even though random forest is so power full we accept the model only after cross validation


set.seed(2348)
cv10_1 <- createMultiFolds(train_val1[,5], k = 10, times = 10)

# Set up caret's trainControl object per above.
ctrl_1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv10_1)



set.seed(1234)
rf.5<- train(x = train_val1[,-5], y = train_val1[,5], method = "rf", tuneLength = 3,
             ntree = 1000, trControl =ctrl_1)

rf.5

varImpPlot(rf.5)

##Cross validation give us the accurcay rate of .8393

###Lets Predict the test data 

pr.rf=predict(rf.5,newdata = test_val1)

confusionMatrix(pr.rf,test_val1$Survived)


###Logistic Regression
contrasts(train_val1$Sex)
contrasts(train_val1$Pclass)


##Lets run Logistic regression model
log.mod <- glm(Survived ~ ., family = binomial(link=logit), 
               data = train_val1)
###Check the summary
summary(log.mod)

confint(log.mod)

train.probs <- predict(log.mod, data=train_val1,type =  "response")
table(train_val1$Survived,train.probs>0.5)

(395+204)/(395+204+70+45)

###Logistic regression predicted train data with accuracy rate of 0.83 

test.probs <- predict(log.mod, newdata=test_val1,type =  "response")
table(test_val1$Survived,test.probs>0.5)

(168+83)/(168+15+31+83)


