### templot ####
temp<-function(t){ #t = toe1, toe2, toe3
  x<-c(tpslecane, sp = as.character(t))
  gpa<-gpagen(x$land)
  plotTangentSpace(gpa$coords, label=TRUE)
}

### plot vca score ####
plot.vca<-function(data, axis1 = 2, axis2 = 3, ...){
  colchart <- as.numeric(data[,1])
  colcode<-colors()[colchart]
  
  xl<-paste("PC", axis1-1, sep = "")
  yl<-paste("PC", axis2-1, sep = "")
  plot(data[,axis1], data[,axis2], pch = colchart, xlab = xl, ylab = yl, ...)
  
  n<-length(levels(data[,1]))
  for(i in 1:n){
    a<-levels(data[,1])[i]
    x<-data[data[,1]==a,axis1]
    y<-data[data[,1]==a,axis2]
    lines(ELLI(x, y))
    text(mean(x), mean(y), a, cex = 0.8)
  }
}

#### temp plot for plot pca metric #############
tem.pcametric<-function(){
  par(mfrow = c(2,2), omi = c(0, 0, 0 ,0), mai = c(0.7, 0.7, 0.1, 0.1),
      mgp = c(2.2, 1, 0))
  plot(pca.metric(metric.toe1$data[-16, 1:9]), xlab = "", ylab = "PC2")
  mtext("Group 1", side = 3, line = -2, adj = 1)
  plot(pca.metric(metric.toe20$data[, 1:9]), xlab = "", ylab = "")
  mtext("Group 2", side = 3, line = -2, adj = 1)
  plot(pca.metric(metric.toe30$data[-53, 1:9]), xlab = "PC1", ylab = "PC2")
  mtext("Group 3", side = 3, line = -4, adj = 1)
  plot(pca.metric(metric.toe31$data[, 1:10]), xlab = "PC1", ylab = "")
  mtext("Group 4", side = 3, line = -2, adj = 1)
}

#### temp plot for plot rda metric #############
tem.rdametric<-function(){
  par(mfrow = c(2,2), omi = c(0, 0, 0 ,0), mai = c(0.7, 0.7, 0.1, 0.1),
      mgp = c(2.2, 1, 0))
  plotrda(metric.toe1$data[-16, 1:9], xlab = "", ylab = "PC2")
  mtext("Group 1", side = 3, line = -2, adj = 1)
  plotrda(metric.toe20$data[, 1:9], xlab = "", ylab = "")
  mtext("Group 2", side = 3, line = -2, adj = 1)
  plotrda(metric.toe30$data[-53, 1:9], xlab = "PC1", ylab = "PC2")
  mtext("Group 3", side = 3, line = -4, adj = 1)
  plotrda(metric.toe31$data[, 1:10], xlab = "PC1", ylab = "")
  mtext("Group 4", side = 3, line = -2, adj = 1)
}

#### temp plot for plot pca metric #############
tem.ldametric<-function(){
  par(mfrow = c(2,2), omi = c(0, 0, 0 ,0), mai = c(0.7, 0.7, 0.1, 0.1),
      mgp = c(2.2, 1, 0))
  plot(lda.metric(metric.toe1$data[-16, 1:9]), xlab = "", ylab = "LD2")
  mtext("Group 1", side = 3, line = -2, adj = 1)
  plot(lda.metric(metric.toe20$data[, 1:9]), xlab = "", ylab = "")
  mtext("Group 2", side = 3, line = -2, adj = 1)
  plot(lda.metric(metric.toe30$data[-53, 1:9]), xlab = "LD1", ylab = "LD2")
  mtext("Group 3", side = 3, line = -2, adj = 1)
  plot(lda.metric(metric.toe31$data[, 1:10]), xlab = "LD1", ylab = "")
  mtext("Group 4", side = 3, line = -2, adj = 1)
}

#### temp plot for plot pca landmark #############
tem.ldatps<-function(){
  par(mfrow = c(2,2), omi = c(0, 0, 0 ,0), mai = c(0.7, 0.7, 0.1, 0.1),
      mgp = c(2.2, 1, 0))
  plot(lda.tps(tps12.toe1$land, tps12.toe1$spnames), axis2 = 4, xlab = "", ylab = "LD2")
  mtext("Group 1", side = 3, line = -2, adj = 1)
  plot(lda.tps(tps12.toe20$land, tps12.toe20$spnames), xlab = "", ylab = "")
  mtext("Group 2", side = 3, line = -2, adj = 1)
  plot(lda.tps(tps12.toe30$land, tps12.toe30$spnames), axis2 = 4, xlab = "LD1", ylab = "LD2")
  mtext("Group 3", side = 3, line = -2, adj = 1)
  plot(lda.tps(tps12.toe31$land, tps12.toe31$spnames), axis2 = 4, xlab = "LD1", ylab = "")
  mtext("Group 4", side = 3, line = -2, adj = 1)
}