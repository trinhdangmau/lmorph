#### plot pca for data frame ############
plot.pcametric<-function(data, label = TRUE, ellip=TRUE, axis1 = 2, axis2 = 3, ...){
  
  group <- data$group
  prin_comp<-data$prin_comp
  colchart <- as.numeric(group)*3
  colcode<-colors()[colchart]
  
  zdata<-cbind(group, as.data.frame(prin_comp$x))
  
  plot(zdata[,axis1], zdata[,axis2], pch = 21, bg = colcode, ...)
  
if(ellip==TRUE){
  n<-length(levels(group)) # 2
  for(i in 1:n){
    a<-levels(zdata[,1])[i]
    x<-zdata[zdata[,1]==a,axis1]
    y<-zdata[zdata[,1]==a,axis2]
    lines(ELLI(x, y))#
    text(mean(x), mean(y), paste("L. ", a, sep = ""), cex = 0.8, font = 3)
  }
}
  arrow_data<-prin_comp$rotation*2
  #arrow_data<-prin_comp$rotation
  arrows(0,0,arrow_data[,(axis1-1)],arrow_data[,(axis2-1)],lwd=2,length=0.1,col = 4)
  
  for(i in 1:nrow(arrow_data)){
    text(arrow_data[i,(axis1-1)], arrow_data[i,(axis2-1)], rownames(arrow_data)[i], cex = 0.8, font = 2, col = "red")
  }
}