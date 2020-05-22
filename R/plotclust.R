#### hclust plot ##########
plotclust<-function(x, method = "ave"){
  require(ade4)
  nc<-ncol(x)
  bb<-hclust(dist(x[,2:nc]), method=method)
  #plot(bb)
  kk<-hclust2phylog(bb, FALSE)
  radial.phylog(kk, clabel.l=0.7, labels.leaves = x[,1], cleaves=0, circle = 1.7)
}

#### hclust plot 2 for landmark data ### x = c.tpslecane$land & group = c.tpslecane$group
## dim(n, p, k)
plotclust2<-function(data, group, method = "complete"){
  require(shapes)
  x<-data
  x1<-orp(pgpa(x)$rotated)
  n<-dim(x1)[1]
  p<-dim(x1)[2]
  k<-dim(x1)[3]
  m<-t(matrix(x1, n*p, k))
  plot(hclust(dist(m), method=method),main=method,
       labels=data$spnames,cex=0.7)
}