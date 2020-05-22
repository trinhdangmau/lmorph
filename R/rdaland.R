### rda method #########
rdaland<-function(landata){ ###x = land data
  x1.gpa<-gpagen(landata$land)
  xf1<-two.d.array(x1.gpa$coords)
  prin_comp<-rda(xf1)
  pca_scores<-scores(prin_comp)
  plot(pca_scores$sites[,1], pca_scores$sites[,2], pch=as.numeric(landata$group), xlim=c(-0.25,0.25), ylim=c(-0.25,0.25))
  
  group<-as.vector(landata$spnames)
  zdata<-cbind(group, as.data.frame(pca_scores$sites))
  
  n<-length(levels(group))
  for(i in 1:n){
    a<-levels(zdata[,1])[i]
    x<-zdata[zdata[,1]==a,2]
    y<-zdata[zdata[,1]==a,3]
    lines(ELLI(x, y))
    text(mean(x), mean(y), paste("L. ", a, sep = ""), cex = 0.8, font = 3)
  }
  #ordiellipse(prin_comp,landata$spnames, kind = c("sd"), col = TRUE, label = TRUE, conf=0.99)
}