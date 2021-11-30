library(class)
library(caret)
#library(scatterplot3d)

#load iris data
data(iris)

before_suffling=head(iris)


View(iris)
table(iris$Species)

#visualize iris data

library(ggplot2)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+
  geom_point(aes(color = Species, shape = Species))+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

ggplot(iris, aes(x = Petal.Length, y = Petal.Width))+
  geom_point(aes(color = Species, shape = Species))+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))


#shuffle iris data
iris=iris[sample(1:length(iris[,1]),length(iris[,1])),]
head(iris)
before_suffling


#assign training data and test data
nrow(iris)

train_index=1:round( nrow(iris) *0.7)

train_data=iris[train_index,1:4]
nrow(train_data)
train_label=as.character(iris[train_index,5])

test_data=iris[-train_index,1:4]
nrow(test_data)
test_label=as.character(iris[-train_index,5])

#scale data
#train_data=scale(train_data)
#test_data=scale(test_data)

#find the best k using cross validation 
caret_fit <- train(train_data, train_label, method = "knn", trControl = trainControl(method="cv",number = 10))


#plot accuracy and k
plot(caret_fit)
caret_fit$bestTune


#predict labels
prediction<- predict(caret_fit, newdata = test_data)
prediction

#check the prediction result
summary(prediction)

#tabularize prediction vs observed value
tb <- table(prediction,test_label)
tb

#calculate accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tb)

