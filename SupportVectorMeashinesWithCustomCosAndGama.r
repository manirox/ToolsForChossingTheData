rm(list=ls())
warnings()
library("e1071")
set.seed(123)
List1=list()
List2=list()
List3=list()
housing_data_import1 = read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")

colnames(housing_data_import1) = c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")

(as.data.frame.matrix(housing_data_import1))
pargrid = expand.grid(gamma=c(.5,1,2,10),cost=10^(-1:3))
#print(pargrid)

List1= NULL
CaliculatingMinVal  <- function(List1,List2,List3)
{
  df <- data.frame(matrix(unlist(List1), nrow = nrow(pargrid), byrow  = FALSE))
  df2 <- data.frame(matrix(unlist(List2), nrow = nrow(pargrid), byrow = FALSE))
  df3 <- data.frame(matrix(unlist(List3), nrow = nrow(pargrid), byrow = FALSE))
  df_combined =data.frame(df2,df3,df)
  print(df_combined)
  resultmeanval=rowMeans(df_combined[,-1:-2])
  df_resultvalue=data.frame(df_combined,resultmeanval)
  #print(resultmeanval)
  #print(df_resultvalue)
  res=min(df_resultvalue[,ncol(df_resultvalue)])
  #df_resultvalue[which.min(df_resultvalue$resultmeanval)]
  print(res)
  result2 =subset(df_resultvalue, resultmeanval == res )
  print(result2)
  print(result2[,1:2])
  print(result2[,2])
}

WiteToAfile <- function(j,train1,test1)
{
  Traindatafile=paste(j,"Trainfilename.csv",sep = "")
  print(Traindatafile)
  write.csv(train1, file = Traindatafile,row.names=FALSE)
  Testingdatafile=paste(j,"Testingdata.csv",sep = "")
  print(Testingdatafile)
  write.csv(test1, file = Testingdatafile,row.names=FALSE)
}

RetrningBestPargrid <- function()
{ 
  Mod_data <- housing_data_import1[sample(nrow(housing_data_import1)),]
  
  #split the data into train and test set
  folds <- cut(seq(1,nrow(Mod_data)), breaks = 5, labels = FALSE) 
  
  
  
  for(j in 1:5)
  {
    
  
    
    testIndex = which(folds==j,arr.ind = TRUE)
    train1 <- housing_data_import1[-testIndex, ]
    test1  <- housing_data_import1[testIndex, ]
    print(train1)
    #print(test1)
    
    List2=NULL
    List3=NULL
    
    for(i in 1:nrow(pargrid))
    {
      
      
      cost=pargrid[i,2]
      gama=pargrid[i,1]
      
      svm_model_aftertune = svm(MEDV ~. ,data = train1,kernel="radial",cost=cost,gamma=gama)
      
      
      predictFeatures = predict(svm_model_aftertune,train1[,-14])
      result = ((sum((predictFeatures-test1[14])^2))/nrow(train1))
      
      List1[[length(List1)+1]] = result
      List2[[length(List2)+1]] = cost
      List3[[length(List3)+1]] = gama
      #print(List1)
      
      
    }
    
    
    
  }
  
  CaliculatingMinVal(List1,List2,List3)
  
  
}

RetrningBestPargrid()
#print(List1)
