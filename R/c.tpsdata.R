#### combine tps and metric data ##########
c.tpsdata<-function(tpsdata, sp = NULL, maxland = 4, nland = NULL, nsp = NULL){
  if(length(sp)==0){
    list<-tpsdata$splist
  }else{list<-intersect(sp, tpsdata$splist)}
  
  data<-tpsdata$main
  
  dim3<-NULL
  group<-NULL
  out<-NULL
  spnames<-NULL
  if(length(nland) == 0){
    x<-dim(data[[1]])[1]
  }else{x<-length(nland)}
  
  for(i in 1:length(list)){
    dimi<-dim(data[[list[i]]])[3]
    if(dimi>=maxland){
      dim3<-c(dim3, dimi)
      
      repi<-rep(i, dimi)
      group<-c(group, repi)
      
      namei<-rep(list[i], dimi)
      spnames<-c(spnames, namei)
      
      dimout<-dim(out)[3]
      g<-sum(dimi, dimout)
      currentland<-subland(data[[list[i]]], nland, nsp)
      out<-array(c(out, currentland), dim=c(x, 2, g))
    }
  }
  #arrayout<-array
  outf<-list(group=group, land=out, spnames=spnames)
  class(outf)<-"ctpsdata"
  return(outf)
}