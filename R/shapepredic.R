#### shape prediction from metrix data #######
shapepredic<-function(model, shape){
  
  predic<-predict(model, shape)
  x<-predic$posterior
  x<-as.data.frame(x)
  out<-x[order(x, decreasing = TRUE)]
  out<-out[1:3]*100
  #names(out)<-names(x)[1:3]
  return(out)
}

#### shape prediction from tps data #######
tpspredic<-function(datam, target){
  data<-datam$land
  groupdata<-datam$spnames
  model<-lda.tps(data, groupdata)$formlda
  g<-dim(data)[3]+1
  
  data2<-array(c(data, target), dim=c(dim(data)[1], 2, g))
  x1<-orp(pgpa(aligne(data2))$rotated)#gpagen(data)$coords
  #x1<-bookstein2d(data2, l1 = 2, l2=5)$bshpv
  n<-dim(x1)[1]
  p<-dim(x1)[2]
  k<-dim(x1)[3]
  m<-t(matrix(x1, n*p, k))
  #m<-m[,c(-2,-5,-14,-17)]
  
  #m<-lda.tps(data, groupdata)$mdata
  
  i<-nrow(m)
  shape<-m[i,]
  
  predic<-predict(model, shape)
  x<-predic$posterior
  x<-as.data.frame(x)
  out<-x[order(x, decreasing = TRUE)]
  out<-out[1:3]
  #names(out)<-names(x)[1:3]
  return(out)
}