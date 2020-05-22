aligne<-function(A){
  B<-A
  n<-dim(A)[3]; k<-dim(A)[2]
  for (i in 1:n){
    sv<-svd(var(A[,,i]))
    M<-A[,,i]%*%sv$u
    v1<-A[2,,i]-A[1,,i]; v2<-A[3,,i]-A[1,,i]
    V1<-M[2,]-M[1,]; V2<-M[3,]-M[1,]
    if (k ==2){
      if (round(angle2d(v1,v2),3)!=round(angle2d(V1,V2),3)){
        M[,1]=-M[,1]
      }
    }
    if (k ==3){
      if (round(angle3(v1,v2),3)!=round(angle2d(V1,V2),3)){
        M[,1]=-M[,1]
      }
    }
    B[,,i]<-M
  }
  B  
}
