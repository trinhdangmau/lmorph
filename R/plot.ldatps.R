### plot function for lda.tps ###
plot.ldatps<-function(data, axis1 = 2, axis2 = 3, ...){
  score<-data$scores
  group<-data$group
  
  zdata<-cbind(group, as.data.frame(score))
  
  plot(zdata[,axis1], zdata[,axis2], pch = as.numeric(group), asp = 1, ...)
  
  n<-length(levels(group))
  for(i in 1:n){
    a<-levels(zdata[,1])[i]
    x<-zdata[zdata[,1]==a,axis1]
    y<-zdata[zdata[,1]==a,axis2]
    lines(ELLI(x, y))
    text(mean(x), mean(y), paste("L. ", a, sep = ""), cex = 0.8, font = 3)
  }
  
}