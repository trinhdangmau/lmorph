##### manova ########
manova.metric<-function(data, test = "Hotelling-Lawley"){
  nc<-ncol(data)
  mdata<-NULL
  for(i in 2:nc){
    xc<-data[,i]
    mdata<-cbind(mdata, xc)
  }
  out<-summary(manova(mdata ~ as.factor(data[,1])), test = test)
  return(out)
}