### megr data from tps file####
combine<-function(list, from = 1, to = length(list), maxland = 2, nland = NULL, nsp = NULL){
  dim3<-NULL
  group<-NULL
  out<-NULL
  spnames<-NULL
  if(length(nland) == 0){
    x<-dim(get(list[1]))[1]
  }else{x<-length(nland)}
  for(i in from:to){
    dimi<-dim(get(list[i]))[3]
    if(dimi>=maxland){
      dim3<-c(dim3, dimi)
      repi<-rep(i, dimi)
      group<-c(group, repi)
      namei<-rep(list[i], dimi)
      spnames<-c(spnames, namei)
      
      dimout<-dim(out)[3]
      g<-sum(dimi, dimout)
      currentland<-subland(get(list[i]), nland, nsp)
      out<-array(c(out, currentland), dim=c(x, 2, g))
    }
    
  }
  #arrayout<-array
  outf<-list(group=group, land=out, spnames=spnames)
  return(outf)
}