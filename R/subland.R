#### sub spilit lankmark point #####
subland<-function(landdata, nland = NULL, nsp = NULL){
  if(length(nland) != 0){
    if(length(nsp) == 0){
      newland<-landdata[nland, ,]
    }else{
      newland<-landdata[nland, ,nsp]
    }
  }else{
    newland <- landdata
  }
  return(newland)
}