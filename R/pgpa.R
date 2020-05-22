pgpa<-function(A){
 p<-dim(A)[1];k<-dim(A)[2];n<-dim(A)[3]
 temp2<-temp1<-array(NA, dim=c(p,k,n)); Siz<-numeric(n)
 for (i in 1:n)
	{Acs<-centsiz(A[,,i])
	Siz[i]<-Acs[[1]]
	temp1[,,i]<-trans1(Acs[[2]])}
 Qm1<-dist(t(matrix(temp1,k*p,n)))
 Q<-sum(Qm1); iter<-0
 while (abs(Q)>0.00001)
	{for(i in 1:n){
	M<-mshape(temp1[,,-i])
	temp2[,,i]<-pPsup(temp1[,,i],M)[[1]]}
 Qm2<-dist(t(matrix(temp2,k*p,n)))
 Q<-sum(Qm1)-sum(Qm2)
 Qm1<-Qm2
 iter=iter+1
 temp1<-temp2}
 list("rotated"=temp2,"it.number"=iter,"Q"=Q,"intereucl.dist"=Qm2,"mshape"=centsiz(mshape(temp2))[[2]],"cent.size"=Siz)
}