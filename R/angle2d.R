angle2d <- function(v1,v2)
{v1<-complex(1,v1[1],v1[2])
v2<-complex(1,v2[1],v2[2])
(pi+Arg(v1)-Arg(v2))%%(2*pi)-pi}
