rm(list=ls())
pollutantmean <- function(specdata,corrval)
{
  filenames <- list.files(specdata, pattern="*.csv", full.names=TRUE)
  print(filenames[1:2])
  resvvalue1 <- filenames[1:2]
  print(resvvalue1)
  
  ldf <- lapply(resvvalue1, read.csv)
  
  temprbind = NULL
  #temprbind  =0
  corrr = numeric()
  for(i in 1:2)
  {
    
    temprbind <- rbind(data.frame(temprbind),data.frame(ldf[i]))
    
    
  }
  
 OmmitingNas <- na.omit(temprbind)
 #print(temprbind)
 print(OmmitingNas)
 GettingSubset <- OmmitingNas[1:corrval,]

 print((GettingSubset))
 sulphate <- GettingSubset$sulfate
 nitrate <- GettingSubset$nitrate
 
 for(i in 1:nrow(GettingSubset))
 {
 #data.frame(ResToSupply) <- GettingSubset[,i]
 #print(class(ResToSupply[i]))
 #print(ResToSupply)
 print("--sulphate--")
 print(sulphate[i])
 print("--nitrate--")
 print((nitrate[i]))
 corrr = c(corrr, cor(sulphate[i], nitrate[i]))
 print("the correlation of the result")
 #cor(sulphate[i], nitrate[i])
 print(cor(sulphate[i], nitrate[i]))
 #CorResult <- cor(sulphate[i],nitrate[i])

 #print(corrr)
 }
 print(corrr)
  
}
cor(c(6,4),c(7,5))
x <- mtcars[1:3]
y <- mtcars[4:6]
cor(x, y)


#pollutantmean("/Users/manishreddybendhi/Desktop/Fun/RProgrammingCoursera/specdata",1:300)
pollutantmean("C://Users//Mreddy//Desktop//MeshineLearning//DataScience//CourseraAssigenment-1//rprog%2Fdata%2Fspecdata//specdata",100)


