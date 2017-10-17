library("e1071")
head(iris,10)
attach(iris)
x= subset(iris,select = -Species)
y=Species
x
y
svm_model1=svm(Species ~ .,data = iris)
summary(svm_model1)
svm_model2=svm(x,y)
summary(svm_model2)
predict1=predict(svm_model2,x)
system.time(predict1<-predict(svm_model2,x))
table(predict1)
table(predict1,y)

svm_tune <- tune(svm,train.x = x,train.y = y,kernel="radial",ranges = list(cost=10^(-1:2)),gamma=c(.5,1,2))
print(svm_tune)


svm_tune <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune)


svm_model_aftertune = svm(Species ~. ,data = iris,kernel="radial",cost=1,gamma=0.5)
summary(svm_model_aftertune)
predict2=predict(svm_model_aftertune,x)
predict2

table(predict2,y)
