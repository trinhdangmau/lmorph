####### lda method for metric data #####
plot.ldametric<-function(data, axis1 = 2, axis2=3, ...){
  
  mdata<-data$mdata
  group<-data$group
  formlda<-data$formlda
  proj1<-mdata%*%formlda$scaling
  zdata<-cbind(group, as.data.frame(proj1))
  
  plot(zdata[,axis1], zdata[,axis2],pch=(1:50)[as.factor(group)],asp=1, cex=0.6, ...)
  
  n<-length(levels(zdata[,1]))
  for(i in 1:n){
    a<-levels(zdata[,1])[i]
    x<-zdata[zdata[,1]==a,axis1]
    y<-zdata[zdata[,1]==a,axis2]
    lines(ELLI(x, y))
    text(mean(x), mean(y), paste("L. ", a, sep = ""), cex = 0.8, font = 3)
  }
}
