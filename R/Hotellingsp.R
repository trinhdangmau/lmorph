Hotellingsp<-function(SSef, SSer, dfef, dfer, exact=F){
library(MASS)
p <- qr(SSef+SSer)$rank
k<-dfef; w<-dfer
s<-min(k,p)
m<-(w-p-1)/2
t1<-(abs(p-k)-1)/2
Ht<-sum(diag(SSef%*%ginv(SSer)))
Fapprox<-Ht*(2 * (s*m+1))/(s^2*(2*t1+s+1))
ddfnum<-s*(2*t1+s+1)
ddfden<-2*(s*m+1)
pval= 1-pf(Fapprox, ddfnum, ddfden)
if (exact)
{b<-(p+2*m)*(k+2*m)/((2*m+1)*(2*m-2))
c1<-(2+(p*k+2)/(b-1))/(2*m)
Fapprox<-((4+(p*k+2)/(b-1))/(p*k))*(Ht/c1)
ddfnum<-p*k
ddfden<-4+(p*k+2)/(b-1)}
unlist(list("dfeffect"=dfef,"dferror"=dfer,"T2"=Ht,
"Approx_F"=Fapprox,"df1"=ddfnum,"df2"=ddfden,"p"=pval))
}
