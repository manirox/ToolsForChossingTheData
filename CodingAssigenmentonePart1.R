rm(list=ls())
pollutantmean <- function(specdata, pollutant,x)
{
  filenames <- list.files(specdata, pattern="*.csv", full.names=TRUE)
  print(filenames[x])
  resvvalue1 <- filenames[x]
  print(resvvalue1)
  
  ldf <- lapply(resvvalue1, read.csv)
  
  temprbind = NULL
  #temprbind  =0
  for(i in 1:length(x))
  {
    print(i)
    print(data.frame(ldf[i]))
    temprbind <- rbind(data.frame(temprbind),data.frame(ldf[i]))
    
    print(temprbind)
  }
  print(temprbind)
  
  
  if (pollutant  == "nitrate") {
    print(mean(temprbind$nitrate,na.rm = TRUE))
  } else {
    print(mean(temprbind$sulfate,na.rm = TRUE))
    
  }
  
}

pollutantmean("/Users/manishreddybendhi/Desktop/Fun/RProgrammingCoursera/specdata","sulphate",1:300)
