###boxplot by character ###
boxgroup<-function(data1, data2){
  par(mfrow = c(4,4), omi = c(0, 0, 0 ,0), mai = c(0.7, 0.7, 0.1, 0.1),
      mgp = c(2.2, 1, 0))
  for(i in 2:ncol(data1)){
    boxplot(data1[,i], data2[,i], col = c("white", "gray"))
    mtext(colnames(data1)[i], side = 3, line = -1)
  }
}