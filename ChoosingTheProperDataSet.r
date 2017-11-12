rm(list=ls())

removedata <- function(DataSet)
{
housing_data_import1 = read.table(DataSet,sep = ',')
print(housing_data_import1)
head(housing_data_import1)
print("the no of colums in the dta are =")
print(ncol(housing_data_import1))
print("no of rows in the data set are =")
print(nrow(housing_data_import1))
print("no of rows after ommiting the nas ")
Omitting_Nas <- na.omit(housing_data_import1)
print(nrow(Omitting_Nas))
print("Total no of nas in the data set ")
print((nrow(housing_data_import1)-nrow(Omitting_Nas)))
}
removedata("http://archive.ics.uci.edu/ml/machine-learning-databases/soybean/soybean-large.data")
