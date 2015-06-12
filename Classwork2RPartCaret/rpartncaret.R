#Caret
test <- read.csv("C:/test.csv")
train <- read.csv("C:/train.csv")

#Impute NAs
test$Age[is.na(test$Age)] = mean(test$Age, na.rm=TRUE)
test$Fare[is.na(test$Fare)] = mean(test$Fare, na.rm=TRUE)


install.packages("rpart")
library(rpart)

install.packages("caret")
library(caret)

decision <- rpart(Survived ~ Sex + Age + Pclass + Fare + Parch, data=train)


# plot tree 
plot(decision, uniform=TRUE, 
     main="Regression Tree for Survival ")
text(decision, use.n=TRUE, all=TRUE, cex=.8)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 1)

gbmGrid <-  expand.grid(interaction.depth = c(1, 2, 3, 4),
                        n.trees = (1:200)*100,
                        shrinkage = c(0.1,0.05, 0.01, 0.005, 0.001),
                        n.minobsinnode = 50)

gbmFit1 <- train(Survived ~ Sex + Age + Pclass + Fare + Parch, data = train,
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = TRUE,
                 tuneGrid = gbmGrid)
gbmFit1

#Tuning parameter 'n.minobsinnode' was held constant at a value of 50
#RMSE was used to select the optimal model using  the smallest value.
#The final values used for the model were n.trees = 900, interaction.depth = 4, shrinkage = 0.05 and n.minobsinnode = 50. 

trellis.par.set(caretTheme())
plot(gbmFit1)