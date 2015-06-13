# Classwork 5/2/2015
library(EBImage)

setwd("C:/Users/BLC/Desktop/CSCI_DATA/images_training_rev1/")
imageList <- list.files(path="C:/Users/BLC/Desktop/CSCI_DATA/images_training_rev1", pattern=".jpg$", full.names = TRUE)
for (i in 1:61578 )
{
  img = readImage(imageList[i])
  img <- resize(img, 50, 50)
  colorMode(img) <- Grayscale
  galaxy_train$Galaxy_ID[i] <- imageList[i] # Adds the given Galaxy ID based on the image ID
  galaxy_train$Prob_Smooth <- 0 # Will be the default value
  galaxy_train$mean[i] <- mean(img) # Calculates the mean for the given image and adds it
  galaxy_train$var <- var(img) # Same as mean for variance
  galaxy_train$'10q'[i] <- quantile(img, probs = .10) # etc with the quantiles
  galaxy_train$'25q'[i] <- quantile(img, probs = .25)
  galaxy_train$'75q'[i] <- quantile(img, probs = .75)
  galaxy_train$'90q'[i] <- quantile(img, probs = .90) 
}
#My script didnt use the Matrix X because I had lost my environment. So I ended up doing an extra pass through all the files.


library(tools)
galaxy_train = galaxy_train$GalaxyID
galaxy_train$Match <- 0
for (i in 1:30789)
{  
  galaxy_train$Match[galaxy_train$GalaxyID[x==galaxy_train_orig_sort$GalaxyID[i]]] <- 1 #galaxy_train_orig_sort is a table I made out of the
  #training set from the Galaxy Competition. Basically I just sorted it by Galaxy ID for whatever reason. Now here I am indexing which of the
  #scanned images is from the training set, where as the unmarked ones are from the test set. 
}

for (i in 1:30789)
{
  test_set_mod$GalaxyID[i] = test_set_k2$GalaxyID[i];
  test_set_mod$mean[i] = galaxy_train$mean[test_set_mod$GalaxyID[i]]
  test_set_mod$var[i] = galaxy_train$var[test_set_mod$GalaxyID[i]]
  test_set_mod$X10q[i] = galaxy_train$X10q[test_set_mod$GalaxyID[i]]
  test_set_mod$X25q[i] = galaxy_train$X25q[test_set_mod$GalaxyID[i]]
  test_set_mod$X75q[i] = galaxy_train$X75q[test_set_mod$GalaxyID[i]]
  test_set_mod$X90q[i] = galaxy_train$X90q[test_set_mod$GalaxyID[i]]
  
}
test_set_k2 = galaxy_train[galaxy_train$Match==0]
for (i in 1:30789)
{
  test_set_mod2$GalaxyID[i] = test_set_k2$GalaxyID[i];
  test_set_mod2$one[i] = new_features2[1][test_set_mod2$GalaxyID[i]]
  test_set_mod2$two[i] = galaxy_train[2][test_set_mod2$GalaxyID[i]]
  
}


for (i in 1:30789)
{
  galaxy_train$Prob_Smooth[galaxy_train$GalaxyID==galaxy_train_orig_sort$GalaxyID[i]] <- galaxy_train_orig_sort$Prob_Smooth
  submission_index$GalaxyID[i]
}
for (i in 1:30789)
{
new_stuff$Prob_Smooth[new_stuff$GalaxyID==galaxy_train_orig_sort$GalaxyID[i]] <- galaxy_train_orig_sort$Prob_Smooth[i]
}
library(caret)

fC <- trainControl(method = "repeatedcv", number = 10, repeats = 2, verbose = TRUE)

gG <-  expand.grid(interaction.depth = c(4), n.trees = (1:50)*100, shrinkage = c(0.1,0.001) )

gbF <- train(Prob_Smooth ~ mean + var + X10q + X25q + X75q + X90q + p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10, data = new_test_data,
                 method = "gbm",
                 trControl = fC,
                 verbose = FALSE,
                 ## Now specify the exact models 
                 ## to evaludate:
                 tuneGrid = gG)
gbF


#new_frame3 = merge(new_features2, image_index, by="GalaxyID")
#library(plyr)
#rename(d, c("beta"="two", "gamma"="three"))

#basename(file_path_sans_ext(galaxy_train$Galaxy_ID))
galaxy_train$Galaxy_ID <- basename(file_path_sans_ext(galaxy_train$Galaxy_ID))
# set our path with extensions to no path and no extension
galaxy_train_3 = merge(galaxy_train, galaxy_train_orig, by="GalaxyID")
galaxy_train$Prob_Smooth = galaxy_train_3$Prob_Smooth
#GO back and add the original probabilities given by the training data.

#Now we should have a table where some values of Prob_Smooth are filled and some are not filled.
#We can then disassemble the table into two tables: Our training set and the test set we need to predict values for!
#Then create a model using the training set to predict the test set
#Format the predictions of the test set using the sample submission
#Submit



