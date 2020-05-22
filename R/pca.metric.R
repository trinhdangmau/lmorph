#### plot pca for data frame ############
pca.metric<-function(x, label = TRUE, axis1 = 2, axis2 = 3, ...){
  nc<-ncol(x)
  group <- x[,1]
  prin_comp<-prcomp(x[,2:nc], scale = TRUE)

  rs<-summary(prin_comp)
  out<-list(prin_comp = prin_comp, group = group, summary = rs)
  class(out)<-"pcametric"
  return(out)
}