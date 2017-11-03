rm(list=ls())
library("e1071")
##Setting the seed function
set.seed(123)
#Creting three differernt list each for cos , gama and one for storing the resultant set
List1=list()
List2=list()
List3=list()
#Getting the housing data into a table 
housing_data_import1 = read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")

#adding the colum names to the data set 

colnames(housing_data_import1) = c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")
#Converting the data set into a data frame 

(as.data.frame.matrix(housing_data_import1))

pargrid = expand.grid(gamma=c(.5,1,2,10),cost=10^(-1:3))
print(pargrid)
#assigning the list that stores the result values to null 
List1= NULL

#converting the result values lists into a data frame and picking up the min value 
CaliculatingMinVal  <- function(List1,List2,List3)
{
  
  df <- data.frame(matrix(unlist(List1), nrow = nrow(pargrid), byrow  = FALSE))
  df2 <- data.frame(matrix(unlist(List2), nrow = nrow(pargrid), byrow = FALSE))
  df3 <- data.frame(matrix(unlist(List3), nrow = nrow(pargrid), byrow = FALSE))
  #Combining three different data frames into one 
  df_combined =data.frame(df2,df3,df)
  print(df_combined)
  #Calicuating the mean of all the errors 
  resultmeanval=rowMeans(df_combined[,-1:-2])
  #Combining the result value the mean along with th data frame 
  df_resultvalue=data.frame(df_combined,resultmeanval)
  print(resultmeanval)
  print(df_resultvalue)
  #Picking up the minimum value from the resultatnt data frame 
  res=min(df_resultvalue[,ncol(df_resultvalue)])
  #df_resultvalue[which.min(df_resultvalue$resultmeanval)]
  print(res)
  #Picking up the row in data frame that has the min value  
  result2 =subset(df_resultvalue, resultmeanval == res )
  print(result2)
  #choosint the best cos and gama extracting it from the data frame 
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
  for(j in 1:5)
  {
    
    trainIdx=sample(nrow(housing_data_import1),0.75*nrow(housing_data_import1),replace = FALSE)
    testIdx = setdiff(seq(1, nrow(housing_data_import1)), trainIdx)
    train1 <- housing_data_import1[trainIdx, ]
    test1  <- housing_data_import1[testIdx, ]
    
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
    
    WiteToAfile(j,train1,test1)
    print(List2)
    print(List3)
    print(List1)
    
  }
 
  CaliculatingMinVal(List1,List2,List3)
  
  
}

RetrningBestPargrid()
print(List1)
