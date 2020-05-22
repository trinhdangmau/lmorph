centsiz<-function(M)
{p<-dim(M)[1]
size<-sqrt(sum(apply(M, 2,var))*(p-1))
list("centroid_size" = size,"scaled" = M/size)}
