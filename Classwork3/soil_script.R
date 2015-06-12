soil <- read.csv("C:/soil.csv")
soil_m <- as.matrix(soil)

install.packages("glmnet")
library(glmnet)

results <- glmnet(soil_m, soil_m[,3579], family = "gaussian")
plot(results)
plot(results$lambda)
cvglmnet = cv.glmnet(soil_m[,1:3578], soil_m[,3579])

#based on the smallest lambda, we get the variables ranked by importance. since here we need top 10, I chose not to use the minimum. 
#The 15th largest lambda provides us with the top 11.
predictions = predict(cvglmnet$glmnet.fit,type="coef", s = lambda[15])
summary(predictions) #Tells us the indexes of the variables that are important from most to least. 15th largest lambda returned 11 values.
names(soil)[c(1, 1972, 3027, 3028, 3031, 3181, 3182, 3183, 3184, 3185)] # Lists the top 10 variables by importance for pH predictions
