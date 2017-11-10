rm(list=ls())
pollutantmean <- function(specdata,x)
{
  filenames <- list.files(specdata, pattern="*.csv", full.names=TRUE)
  
  
  
  #temprbind  =0
  List1=list()
  List2=list()
  for(i in x)
  {
    
    print(x)
    resvvalue1 <- filenames[i]
    print(resvvalue1)
    
    ldf <- lapply(resvvalue1, read.csv)
    print(ldf)
    Countnull <-(nrow(na.omit(data.frame(ldf))))
    #print(Countnull)
    List1[[length(List1)+1]] =  i
    List2[[length(List2)+1]] = Countnull
    
    
  }
  
  
  print(List1)
  print(List2)
  df <- data.frame(matrix(unlist(List1), nrow = length(x), byrow  = FALSE))
  df2 <- data.frame(matrix(unlist(List2), nrow = length(x), byrow = FALSE))
  df_combined =data.frame(df,df2)
  print(df_combined)
  
}

pollutantmean("/Users/manishreddybendhi/Desktop/Fun/RProgrammingCoursera/specdata",1:60)
