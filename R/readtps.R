#### read tps data from imageJ ####
readtps<-function(filepath, nland = 12){
  tempdata<-read.table(filepath)
  ##### get lable and check correspond nlandmark point to all specimen
  label <- tempdata["Label"]
  l.table<-table(label)
  
  if(length(l.table[l.table!=nland])!=0){
    err<-l.table[l.table!=nland]
    print(err)
    stop("The number of landmark not equal to all specimen")
  }
  ### get x and y coordinate
  X<-round(tempdata["X"], 0)
  Y<-round(tempdata["Y"],0)
  XY<-cbind(X, Y)
  XY<-as.vector(t(XY))
  n = nland
  m = 2
  k = nrow(tempdata)/nland
  coords<-aperm(array(XY, c(m, n, k)), c(2,1,3))
  v.names<-names(l.table)
  dimnames(coords)<-list(paste("LM", 1:nland, sep=""),c("X", "Y"), v.names)
  return(coords)
}