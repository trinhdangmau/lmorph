####### lda method for metric data #####
lda.metric<-function(data, axis1 = 2, axis2=3){
  require(MASS)
  mdata<-as.matrix(data[,-1])
  group<-data[,1]
  formlda<-lda(mdata, group)
  
  meangroup<-formlda$mean
  meanproj<-meangroup%*%formlda$scaling
  dist<-dist(meanproj)
  out<-list(dist = dist, group = group, mdata = mdata, formlda = formlda)
  class(out)<-"ldametric"
  return(out)
}