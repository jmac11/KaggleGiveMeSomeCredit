library(rpart)
setwd("C:/Ambarish/Kaggle/Give me some credit")

traindata <- read.csv("cs-training.csv")
traindata <- traindata[,c(-1)]

testdata <- read.csv("cs-test.csv")
testdata.x <-testdata[,c(-1,-2)]

ntrees <-  100  

prd <- numeric (nrow (testdata.x))  

for(i in 1: ntrees)
{
  ss <- sample(1:dim(traindata)[1],replace=T) 
  
  cols <- runif (ncol(traindata))> 0.3 ; 
  
  cols[1] <-  TRUE
  
  traindata2 <- traindata[ss,cols] ;
  
  # Generates a full tree is the process of optimizing  
  tree <- rpart (SeriousDlqin2yrs ~., traindata2, control = rpart.control (cp =.0 ))  
  
  prd <- prd + predict (tree, testdata.x)  
  
}

predictions <- prd / ntrees  

resultSet <- data.frame(id = testdata$X , Probability = predictions)

write.csv(resultSet, file="predictionCreditRPart3.csv",row.names = FALSE) 
