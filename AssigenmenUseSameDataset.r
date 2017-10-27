rm(list=ls())
vectorvalueone = 0
tempresult1 =0 
set.seed(123)

resultmatrix=matrix()
listfunction = list()
housing_data_import1 = read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")

colnames(housing_data_import1) = c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")



##Creating a dataframe 
dat_frame_res <- data.frame(x = numeric(20), y = numeric(20) ,stringsAsFactors = FALSE)
sumofresult = 0

(as.data.frame.matrix(housing_data_import1))
pargrid = expand.grid(gamma=c(.5,1,2,10),cost=10^(-1:3))
s=NULL
k=0
for(j in 1:5)
{
  k=k+1
  trainIdx=sample(nrow(housing_data_import1),0.75*nrow(housing_data_import1),replace = FALSE)
  testIdx = setdiff(seq(1, nrow(housing_data_import1)), trainIdx)
  train1 <- housing_data_import1[trainIdx, ]
  test1  <- housing_data_import1[testIdx, ]
  kres = paste(c("value", k), collapse = " ")
 
  print(kres)
  for(i in 1:nrow(pargrid))
  {
    
    
    cost=pargrid[i,2]
    gama=pargrid[i,1]
    
    svm_model_aftertune = svm(MEDV ~. ,data = train1,kernel="radial",cost=cost,gamma=gama)
    
    
    predictFeatures = predict(svm_model_aftertune,test1[,-14])
    result = ((sum((predictFeatures-test1[14])^2))/nrow(test1))
    sumofresult= result
    #print("resultvalue =")
    #print(result)
    #dat_frame_res$x[i] =  cost
    #dat_frame_res$y[i] =  gama
    #dat_frame_res$z[i] =  sumofresult
   
    #print(dat_frame_res)
    dat_frame_res$x[i] =  cost
    dat_frame_res$y[i] =  gama
    dat_frame_res$kres[i] =  sumofresult
    vectrorresult=sumofresult
    #resultmatrix[j,i]=sumofresult
    vectorvalueone[i] = vectrorresult
    
  }
  #k= value+str(k+1)
  
  
  #dat_frame_res=append(dat_frame_res,dat_frame_res)
 cbindres = cbind(dat_frame_res,kres)
  print("result of one c bind values =")
  print(cbindres)
  length(dat_frame_res)
  tempresult1=c(tempresult1,cbi)
}
print("C bind value of two results ")
print(tempresult1)

dat_frame_res
