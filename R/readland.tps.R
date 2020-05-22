readland.tps2<- function (file, specID = c("None", "ID", "imageID"), warnmsg = F) 
{
  specID <- match.arg(specID)
  tpsfile <- scan(file = file, what = "char", sep = "\n", quiet = TRUE)
  lmdata <- grep("LM=", tpsfile)
  if (length(lmdata) == 0) {
    lmdata <- grep("LM3=", tpsfile)
    nland <- as.numeric(sub("LM3=", "", tpsfile[lmdata]))
    k <- 3
  }
  else {
    nland <- as.numeric(sub("LM=", "", tpsfile[lmdata]))
    k <- 2
  }
  n <- nspecs <- length(lmdata)
  if (max(nland) - min(nland) != 0) {
    stop("Number of landmarks not the same for all specimens.")
  }
  p <- nland[1]
  imscale <- as.numeric(sub("SCALE=", "", tpsfile[grep("SCALE", 
                                                       tpsfile)]))
  if (is.null(imscale)) {
    imscale = array(1, nspecs)
  }
  if (warnmsg == T) {
    if (length(imscale) != nspecs) {
      print("Not all specimens have scale. Assuming landmarks have been previously scaled.")
    }
  }
  if (length(imscale) != nspecs) {
    imscale = array(1, nspecs)
  }
  tmp <- tpsfile[-(grep("=", tpsfile))]
  options(warn = -1)
  tmp <- matrix(as.numeric(unlist(strsplit(tmp, split = " +")), 
                           ncol = k, byrow = T))
  if (warnmsg == T) {
    if (sum(which(is.na(tmp) == TRUE)) > 0) {
      print("NOTE.  Missing data identified.")
    }
  }
  coords <- aperm(array(t(tmp), c(k, p, n)), c(2, 1, 3))
  imscale <- aperm(array(rep(imscale, p * k), c(n, k, p)), 
                   c(3, 2, 1))
  coords <- coords * imscale
  
  if (specID == "None") {
    if (warnmsg == T) {print("No Specimen names extracted")
    }
  }
  if (specID == "imageID") {
    imageID <- (sub("IMAGE=", "", tpsfile[grep("IMAGE", tpsfile)]))
    if (length(imageID) != 0) {
      imageID <- sub(".jpg", "", imageID)
      imageID <- sub(".tif", "", imageID)
      imageID <- sub(".bmp", "", imageID)
      imageID <- sub(".tiff", "", imageID)
      imageID <- sub(".jpeg", "", imageID)
      imageID <- sub(".jpe", "", imageID)
      dimnames(coords)[[3]] <- as.list(imageID)
      if (warnmsg == T) {
        print("Specimen names extracted from line IMAGE=")
      }
    }
    if (length(imageID) == 0) {
      if (warnmsg == T) {
        print("No name given under IMAGE=. Specimen names not extracted")
      }
    } 
  }
  
  if (specID == "ID") {
    ID <- sub("ID=", "", tpsfile[grep("ID", tpsfile)])
    if (length(ID) == 0) {
      if(warnmsg ==T){
        print("No name given under ID=. Specimen names not extracted")
      }
    }
    if (length(ID) != 0) {
      dimnames(coords)[[3]] <- as.list(ID)
      if (warnmsg == T) {
        print("Specimen names extracted from line ID=")
      }
    }
  }
  return(coords = coords)
}