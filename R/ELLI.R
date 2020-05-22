ELLI<-function(x,y,conf=0.95,np=50){
  centroid<-apply(cbind(x,y),2,mean)
  ang <- seq(0,2*pi,length=np)
  z<-cbind(cos(ang),sin(ang))
  radiuscoef<-qnorm((1-conf)/2, lower.tail=F)
  vcvxy<-var(cbind(x,y))
  r<-cor(x,y)
  M1<-matrix(c(1,1,-1,1),2,2)
  M2<-matrix(c(var(x), var(y)),2,2)
  M3<-matrix(c(1+r, 1-r),2,2, byrow=T)
  ellpar<-M1*sqrt(M2*M3/2)
  t(centroid + radiuscoef * ellpar %*% t(z))
}