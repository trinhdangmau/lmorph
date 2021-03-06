pPsup<-function(M1,M2)
{k<-ncol(M1)
Z1<-trans1(centsiz(M1)[[2]])
Z2<-trans1(centsiz(M2)[[2]])
sv<-svd(t(Z2)%*%Z1)
U<-sv$v; V<-sv$u; Delt<-sv$d
sig<-sign(det(t(Z1)%*%Z2))
Delt[k]<-sig*abs(Delt[k]) ; V[,k]<-sig * V[,k]
Gam<-U%*%t(V)
phi<-U%*%t(V)
beta<-sum(Delt)
list(Mp1=Z1%*%phi,Mp2=Z2, rotation=Gam,
DP=sqrt(sum(ild2(Z1%*%phi, Z2)^2)),rho=acos(beta))}
