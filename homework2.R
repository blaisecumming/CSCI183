# Loading in the training data
train <- read.csv("train.csv", stringsAsFactors=FALSE)
# Loading in the test data (to be evaluated)
test <- read.csv("test.csv", stringsAsFactors=FALSE)

# First we will assume everyone died since more people died then did not, and try to make accurate assumptions about who lived using our training data

train$Child <- 0
train$Child[train$Age < 18] <- 1

# This will separate children from adults

#We will also try to sort using fare in three categories
train$Cheap <- 0
train$Cheap[train$Fare < 10] <- 1
train$Expensive <- 0
train$Expensive[train$Fare > 30] <- 1

#Those who paid under 10, and 10-30, and 30 or more


aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
#This will display the ratio of survived compared to all entries of the table sorted by 2 factors: Child, and Sex
#We are able to see that 75% of adult females survived, and 69% of children females did. So our original assumption will be all females survive to improve our results.



aggregate(Survived ~ Child + Sex + Cheap + Expensive, data=train, FUN=function(x) {sum(x)/length(x)})
#This will display the ratio of survived compared to all entries of the table sorted by 4 binary factors: Child, Sex, and Cheap/Expensive Fares

#We can observe that only women with the exception of young men made it in a majority. Using this we can compound our results.

#Let survival be based off being a women, or men who are under 18



test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1
test$Survived[test$Age < 18] <- 1

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "secondattempt.csv", row.names = FALSE)