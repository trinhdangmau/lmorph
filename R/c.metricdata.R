#### combine tps and metric data ##########
c.metricdata<-function(metricdata, sp = NULL){
  if(length(sp)==0){
    list<-metricdata$splist
  }else{list<-intersect(sp, metricdata$splist)}
  
  data<-metricdata$main

  group<-NULL
  out<-NULL
  spnames<-NULL
  
  for(i in 1:length(list)){
    datai<-data[[list[i]]]
    spname<-rep(list[i], nrow(datai))
    group<-c(group, rep(i, nrow(datai)))
    spnames<-c(spnames, spname)
    datai<-cbind(spname, datai)
    
    out<-rbind(out, datai)
  }
  
  out<-as.data.frame(out)
  nc<-ncol(out)
  out[,2:nc]<-apply(out[,2:nc], 2, function(x){as.numeric(as.character(x))})
  out[out>208]<-0
  outf<-list(group=group, data=out, spnames=spnames)
  class(outf)<-"cmetricdata"
  return(outf)
}