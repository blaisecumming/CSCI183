# Classwork 5/2/2015
library(EBImage)

setwd("C:/Users/BLC/Desktop/CSCI_DATA/images_training_rev1/")
imageList <- list.files(path="C:/Users/BLC/Desktop/CSCI_DATA/images_training_rev1", pattern=".jpg$", full.names = TRUE)

img = readImage(imageList[1]) #stores image entirely in img object using readImage function
img <- resize(img, 50, 50) #resizes the image to be 50x50
colorMode(img) <- Grayscale

X = as.vector(img)
X = as.matrix(X)
for (2 in 1:61578 )
{
  img = readImage(imageList[i]) #stores image entirely in img object using readImage function
  img <- resize(img, 50, 50) #resizes the image to be 50x50
  colorMode(img) <- Grayscale #converts the color RGB values to Grayscale
  X[i] = img # Each image will be added as a new collumn to the matrix X
}

# The matrix X holds a collumn for each image. Could be transposed if you prefer rows.