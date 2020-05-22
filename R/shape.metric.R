####### SHAPE FDA - lda method for metric data #####
shape.metric<-function(data, axis1 = 2, axis2=3){
  require(MASS)
  nc<-ncol(data)
  mdata<-as.matrix(data[,-1])
  group<-data[,1]
  size<-apply(mdata,1,prod)^(1/(dim(mdata)[2]))
  shapedata<-mdata/size
  shapelda<-lda(shapedata, group)
  proj2<-shapedata%*%shapelda$scaling
  
  zdata<-cbind(group, as.data.frame(proj2))
  
  xl<-paste("FD", axis1-1, sep = "")
  yl<-paste("FD", axis2-1, sep = "")
  plot(zdata[,axis1], zdata[,axis2],pch=(1:50)[as.factor(data[,1])],asp=1, cex=0.6,xlab=xl,ylab= yl, main="Shape FDA")
  
  n<-length(levels(zdata[,1]))
  for(i in 1:n){
    a<-levels(zdata[,1])[i]
    x<-zdata[zdata[,1]==a,axis1]
    y<-zdata[zdata[,1]==a,axis2]
    lines(ELLI(x, y))
    text(mean(x), mean(y), a, cex = 0.8)
  }
}