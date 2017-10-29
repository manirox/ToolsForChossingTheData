set.seed(123)
#Reading the data 
housing_data_import1 = read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
#assigining the coulmn names 
colnames(housing_data_import1) = c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")



##Creating a dataframe 
dat_frame_res <- data.frame(x = numeric(20), y = numeric(20), z = numeric(20) ,stringsAsFactors = FALSE)
sumofresult = 0
#Converting to data frame
(as.data.frame.matrix(housing_data_import1))
#storing the values in a pargrid
pargrid = expand.grid(gamma=c(.5,1,2,10),cost=10^(-1:3))
#assigning the values of cosa and gama 
for(i in 1:nrow(pargrid))
{
  
 #the values of the data sets getiing splitted 
  for(j in 1:5)
  {
    #Spiiting up the data 
    trainIdx=sample(nrow(housing_data_import1),0.75*nrow(housing_data_import1),replace = FALSE)
    testIdx = setdiff(seq(1, nrow(housing_data_import1)), trainIdx)
    train1 <- housing_data_import1[trainIdx, ]
    test1  <- housing_data_import1[testIdx, ]
    #storing the values of cos and gama 
    cost=pargrid[i,2]
    gama=pargrid[i,1]
    #tuning the model 
    svm_model_aftertune = svm(MEDV ~. ,data = train1,kernel="radial",cost=cost,gamma=gama)
    
    #making the predictions 
    predictFeatures = predict(svm_model_aftertune,test1[,-14])
    result = ((sum((predictFeatures-test1[14])^2))/nrow(test1))
    sumofresult= sumofresult+result
    #print("resultvalue =")
    #print(result)
   
    
   
  }
  #print("Sum of result function")
  #cat("\n Value of current cost =",cost)
  #cat("\n Value of current gama = ",gama)
  ##Caliculating the mse error 
   MseError= (sumofresult/5)
  #cat("\n The  average of the function =",MseError)
  #print("Iam Breaking")
  #Storing the vlues of cos and gama 
  dat_frame_res$x[i] =  cost
  dat_frame_res$y[i] =  gama
  dat_frame_res$z[i] =  MseError
  ##equating the sum to zero 
sumofresult=0
}
#Selecting the best values of cos and gama 
length(sumofresult)
dat_frame_res
result1=min(dat_frame_res[,3])
result1
result2 =subset(dat_frame_res, z == result1 )
print(result2[,1])
print(result2[,2])



#Caliculating the best result using the selected cos and gama 
for(j in 1:5)
{
  trainIdx=sample(nrow(housing_data_import1),0.75*nrow(housing_data_import1),replace = FALSE)
  testIdx = setdiff(seq(1, nrow(housing_data_import1)), trainIdx)
  train1 <- housing_data_import1[trainIdx, ]
  test1  <- housing_data_import1[testIdx, ]
  
  cost=pargrid[i,2]
  gama=pargrid[i,1]
  
  svm_model_aftertune = svm(MEDV ~. ,data = train1,kernel="radial",cost=result2[,1],gamma=result2[,2])
  
  
  predictFeatures = predict(svm_model_aftertune,test1[,-14])
  result = ((sum((predictFeatures-test1[14])^2))/nrow(test1))
  sumofresult= sumofresult+result
  #print("resultvalue =")
  print(result)
  
  
  
}


