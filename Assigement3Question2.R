housing_data1= read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
colnames(housing_data1) = c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")
housing_data1

length(housing_data1)
housing_data1
names(housing_data1)
tempsub1=housing_data1[1:11,]

#df1[!(df1$name %in% df2$name),]
tempsub2=housing_data1-tempsub1  

df2 <-matrix(1:6,ncol=2,byrow=TRUE)
df1 <-matrix(1:10,ncol=2,byrow=TRUE)

data.frame(v1=setdiff(df1[,1], df2[,1]), v2=setdiff(df1[,2], df2[,2]))









housing_data1[1]
housing_data1[1:14]
subset(housing_data1,y>1 & y<100)


as.data.frame.matrix(housing_data1)
housing_data1
label=housing_data1[14]
label
fetures=housing_data1[-14]
fetures
#head(housing_data1,100)
trainX=fetures[1:100,]
trainY=fetures[100:500,]
trainX
trainY
nrow(housing_data1)

#####For loop
for(i in 1:nrow(housing_data1))
{
  
  a = i 
  b = i + 5
  i = i + 5
  trainx = testx[a:b,]
  trainy = testx[a:b,] 
  newmod <- new_model(trainx, testx, trainy, testy, 5, a, b)
  newmod
  
}




######End of the function 

splitfun <- function(Trainx)
{
  trainsplit1=Trainx[1:400,]
  testsplit1=Trainx[400:500,]
  trainsplit2=Trainx[200:500,]
  testsplit2=Trainx[1:100,]
  return(trainsplit1)
  
}

resfun1 <- splitfun(fetures)
resfun1
