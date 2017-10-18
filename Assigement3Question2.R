#####implementing the real vallues of the function 
#####Importint the data 
library("e1071")

HousingDataFun <- function(Knodes)
{
##importing the data set 
housing_data1= read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
housing_data1
#setting the column names 
colnames(housing_data1) = c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")
(as.data.frame.matrix(housing_data1))
housing_data1
rowsinds1=nrow(housing_data1)
rowsinds1
housing_data1$ID <- seq.int(nrow(housing_data1))
housing_data1
final_result =0

for(i in 1:Knodes)
{
temp1=rowsinds1/Knodes
temp2 = ceiling(temp1)
temp2
rand1=sample(1:rowsinds1,temp2)
Trainingdata1=housing_data1[! (housing_data1$ID %in% rand1),]
TrainindDatawithoutID= Trainingdata1[,-15]
#print(TrainindDatawithoutID)
TestigData=housing_data1[(housing_data1$ID %in% rand1),]
TrainingDataFeatures = Trainingdata1[,-14:-15]
TrainingDataLabel= Trainingdata1[14]
TestingDataFeatures=TestigData[,-14:-15]
TestigDataLables=TestigData[14]
Traindatafile=paste(i,"Trainfilename.csv",sep = "")
print(Traindatafile)
write.csv(Trainingdata1, file = Traindatafile,row.names=FALSE)
Testingdatafile=paste(i,"Testingdata.csv",sep = "")
print(Testingdatafile)
write.csv(TestigData, file = Testingdatafile,row.names=FALSE)
#print(TrainingDataFeatures)
#print(TrainingDataLabel)
#print(TestingDataFeatures)
#print(TestigDataLables)
#svm_tune <- tune(svm, train.x=x, train.y=y, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
svm_tune <- tune(svm, train.x=TrainingDataFeatures, train.y=TrainingDataLabel, kernel="radial", ranges=list(cost=10^(-1:3), gamma=c(.5,1,2,10)))
#print(svm_tune)
svm_model_aftertune = svm(MEDV ~. ,data = TrainindDatawithoutID,kernel="radial",cost=10,gamma=0.5)
#print(summary(svm_model_aftertune))
#print(svm_model_aftertune)
predictFeatures = predict(svm_model_aftertune,TestingDataFeatures)
#print(predictFeatures)
###Calicuating the mse
result = ((sum((predictFeatures-TestigDataLables)^2))/temp2)
#result = (((predictFeatures-TestigDataLables)^2)/temp2)
#print(result)
#print("hellwo")
final_result = final_result+result
#print(final_result)

}
print(final_result)
crossvalidatioerror = (final_result/Knodes)
print(crossvalidatioerror)
}
resultant <- HousingDataFun(5)
print(resultant)
