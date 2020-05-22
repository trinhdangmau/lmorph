### load biometric data from imageJ #### #scale = (a,b):a= length, b=known length
readmetric <- function(filepath, scale = FALSE, maxlength = NULL,
                       char.name=c("DPl", "DPw", "VPl", "VPw", "HawD", "HawV", "Tl", "Cl", "ASl")){
  ncharac <- length(char.name)
  tem<-read.table(filepath)
  x<-tem[,"Length"]
  #v.names<-names(table(tem["Label"]))
  xmatrix<-matrix(x, ncol = ncharac, byrow = TRUE)
  #### reset maxlength ## if the value >= maxlength set as zero
  if(identical(maxlength, NULL)==FALSE){
    xmatrix[xmatrix>=maxlength]<-0
  }
  ####scale data
  if(identical(scale, FALSE)==FALSE){
    rate = scale[2]/scale[1]
    xmatrix<-xmatrix*rate
  }
  colnames(xmatrix)<-char.name
  #rownames(xmatrix)<-v.names
  return(xmatrix)
}