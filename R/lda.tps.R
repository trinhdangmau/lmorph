#### Discriminant and multivariate Analysis of Variance ######
##data is tps.lecane$land not included group
lda.tps<-function(data, group){
  x1<-orp(pgpa(aligne(data))$rotated)#gpagen(data)$coords
  #x1<-bookstein2d(data, l1 = 2, l2=5)$bshpv
  group<-as.factor(group)
  n<-dim(x1)[1]
  p<-dim(x1)[2]
  k<-dim(x1)[3]
  
  m<-t(matrix(x1, n*p, k))
  #m<-m[,c(-2,-5,-14,-17)]
  n<-dim(m)[1]
  
  mod1<-lm(m ~ group)
  dfef<- length(levels(group))-1
  dfer<- n - length(levels(group))
  SSef<-(n-1)*var(mod1$fitted.values)
  SSer<-(n-1)*var(mod1$residuals)
  rs<-Hotellingsp(SSef, SSer, dfef, dfer)
  
  formlda<-lda(m, group)
  score<-predict(formlda)$x
  
  meangroup<-formlda$mean
  meanproj<-meangroup%*%formlda$scaling
  dist<-dist(meanproj)
  
  ## estimate the withis group variance-vovariance
  VCVw<-SSer/dfer
  LD<-formlda$scaling
  LDs<-VCVw%*%LD
  
  
  out<-list(formlda = formlda, dist = dist, scores = score, mdata = m, group = group, 
            test = rs, GVC = LDs)
  class(out)<-"ldatps"
  return(out)
}