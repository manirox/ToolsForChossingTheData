#####implementing the real vallues of the function 
#####Importint the data 
library("e1071")
housing_data1= read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
housing_data1
colnames(housing_data1) = c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")
(as.data.frame.matrix(housing_data1))
housing_data1
rowsinds1=nrow(housing_data1)
rowsinds1
housing_data1$ID <- seq.int(nrow(housing_data1))
housing_data1
head(housing_data1,5)
Traindatax=housing_data1[,-14]
head(Traindatax,5)
Traindatax
Testingdatax=housing_data1[,14:15]
head(Testingdatax,5)
Testingdatax

for(i in 1:5)
{
temp1=rowsinds1/5
temp2 = ceiling(temp1)
temp2
rand1=sample(1:rowsinds1,temp2)
#print(rand1)
Trainingdata1=housing_data1[! (housing_data1$ID %in% rand1),]
TestigData=housing_data1[(housing_data1$ID %in% rand1),]
TrainingDataFeatures = Trainingdata1[,-14:-15]
TrainingDataLabel= Trainingdata1[14]
TestingDataFeatures=TestigData[,-14:-15]
TestigDataLables=TestigData[14]
#print(TrainingDataFeatures)
#print(TrainingDataLabel)
#print(TestingDataFeatures)
#print(TestigDataLables)
svm_tune <- tune(svm,train.x = TrainingDataFeatures,train.y = TrainingDataLabel,kernel="radial",ranges = list(cost=10^(-1:3)),gamma=c(.5,1,2,10))
print(svm_tune)
}

