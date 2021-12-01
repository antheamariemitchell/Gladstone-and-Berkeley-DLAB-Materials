library(cluster)
library(ggplot2)

#load iris data
data(iris)
head(iris)

#retrieve variables
data=iris[,1:4]

#choose k
library(factoextra)
fviz.p <-fviz_nbclust(x = data,FUNcluster = kmeans, method = 'wss' )
fviz.p #save ggplot output for methods below


#train kmeans
fit <- kmeans(data, centers= 4, nstart=25, algorithm ="Lloyd", iter.max=100)
fit$cluster
#nstart has to do with random initialization, so it's set to 25 to start

#visualize clusters
fviz_cluster(fit, data = data, palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"), ggtheme = theme_minimal())

##### Some ways to find the elbow point, aka ways to find the k value, there's no one answer about which to use -- 
##### there are different methods that can help with that decision, suggestions, not single anwers

#1 kneepointDetection function
BiocManager::install("SamSPECTRAL")
library(SamSPECTRAL)
fviz.y <- ggplot_build(fviz.p)$data[[1]][,"y"] #extract y values
detected <- kneepointDetection(vect=fviz.y) 
print(detected$MinIndex)

#2 visualize second-order differences
fviz_diff2 <- function(p){
  ggpoints <- ggplot_build(p)$data[[1]][,c("x","y")] #extract x,y points
  ggpdiff2<-apply( ggpoints[2] , 2 , diff, differences=2 ) #calc lagged second-order difference
  ggpdiff2<-c(0,ggpdiff2) #no diff2 for first k
  ggpdiff2<-append(ggpdiff2, 0) #no diff2 for last k
  ggpoints$diff2 <- ggpdiff2
  ggpoints$str<- ifelse(ggpoints$diff2 > 0, ggpoints$diff2, 0) #keep postive values
  # ggpoints$2d <- x[i+1] + x[i-1] - 2 * x[i]
  p + geom_bar(aes(x=x, y=str),data=ggpoints,stat="identity", width = .2, fill="transparent", color="black") #add bars
}
fviz_diff2(fviz.p)


