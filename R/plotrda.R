#### plot rda for data frame ############
plotrda<-function(x, label = TRUE, axis1 = 2, axis2 = 3, ...){
  require(vegan)
  n<-ncol(x)
  prin_comp<-rda(x[,2:n], scale = TRUE)
  pca_scores<-scores(prin_comp)
  colchart <- as.numeric(x[,1])*3
  colcode<-colors()[colchart]
  
  xma<-max(pca_scores$species[,1], pca_scores$site[,1])
  xmi<-min(pca_scores$species[,1], pca_scores$site[,1])
  yma<-max(pca_scores$species[,2], pca_scores$site[,2])
  ymi<-min(pca_scores$species[,2], pca_scores$site[,2])
  
  zdata<-cbind(x[,1], as.data.frame(pca_scores$site))
  
  plot(zdata[,axis1], zdata[,axis2], pch = 21, bg = colcode, 
       xlim = range(xmi, xma), ylim = range(ymi, yma), ...)
  
  n<-length(levels(x[,1]))
  for(i in 1:n){
    a<-levels(zdata[,1])[i]
    x<-zdata[zdata[,1]==a,axis1]
    y<-zdata[zdata[,1]==a,axis2]
    lines(ELLI(x, y))
    text(mean(x), mean(y), paste("L. ", a, sep = ""), cex = 0.8, font = 3)
  }
  
  arrows(0,0,pca_scores$species[,1],pca_scores$species[,2],lwd=1,length=0.2)
  
  for(i in 1:nrow(pca_scores$species)){
    text(pca_scores$species[i,1], pca_scores$species[i,2], 
         rownames(pca_scores$species)[i], cex = 0.8, font = 2)
  }
  
  #ordiellipse(prin_comp, x[,1], conf=0.99, kind = method, label = TRUE, cex = 0.6)
}