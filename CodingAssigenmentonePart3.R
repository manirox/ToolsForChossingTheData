rm(list=ls())
pollutantmean <- function(specdata,x)
{
  filenames <- list.files(specdata, pattern="*.csv", full.names=TRUE)
  print(filenames[1:2])
  resvvalue1 <- filenames[1:2]
  print(resvvalue1)
  
  ldf <- lapply(resvvalue1, read.csv)
  
  temprbind = NULL
  #temprbind  =0
  for(i in 1:2)
  {
    
    temprbind <- rbind(data.frame(temprbind),data.frame(ldf[i]))
    
    
  }
 OmmitingNas <- na.omit(temprbind)
 GettingSubset <- OmmitingNas[1:3,]
 print(GettingSubset)
 
 for(i in 1:length(GettingSubset))
 {
 data.frame(ResToSupply) <- GettingSubset[,i]
 #print(class(ResToSupply[i]))
 #print(ResToSupply)
 print(i)
 print((ResToSupply[i]))
 #CorResult <- cor(ResToSupply$sulfate,ResToSupply$nitrate)
 #print(i)
 #print(CorResult)
 }
  
}

pollutantmean("/Users/manishreddybendhi/Desktop/Fun/RProgrammingCoursera/specdata",1:300)
