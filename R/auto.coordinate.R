# What are the cartesian coordinates of pixels within the tolerance?
extract.coord<-function(channel,tolerance=0.99){
  positions<-which(img[,,channel]>=tolerance) 
  row<-nrow(img) - (positions %% nrow(img))
  col<-floor(positions / nrow(img))  +1
  data.frame(x=col,y=row)
}

# Do these two pixels touch? (Diagonal touch returns TRUE)
touches<-function(coord1,coord2)
  coord2$x <= (coord1$x+1) & coord2$x >= (coord1$x-1) & coord2$y <= (coord1$y+1) & coord2$y >= (coord1$y-1)

# Does this pixel touch any pixel in this list?
touches.list<-function(coord1,coord.list)
  any(sapply(1:nrow(coord.list),function(x)touches(coord.list[x,],coord1)))

# Given a data.frame of pixel coordinates, give me a list of data frames
# that contain the "blobs" of pixels that all touch.
extract.pixel.blobs<-function(coords){
  blob.list<-list()
  for(row in 1:nrow(coords)){
    coord<-coords[row,]
    matched.blobs<-sapply(blob.list,touches.list,coord1=coord)
    if(!any(matched.blobs)){
      blob.list[[length(blob.list)+1]]<-coords[row,,drop=FALSE]
    } else {
      if(length(which(matched.blobs))==1) {
        blob.list[[which(matched.blobs)]]<-rbind(blob.list[[which(matched.blobs)]],coords[row,,drop=FALSE])
      } else { # Pixel touches two blobs
        touched.blobs<-blobs[which(matched.blobs)]
        blobs<-blobs[-which(matched.blobs)]
        combined.blobs<-do.call(rbind,touched.blobs)
        combined.blobs<-rbind(combined.blobs,coords[row,,drop=FALSE])
        blobs[[length(blob.list)+1]]<-combined.blobs
      }
    }
  }
  blob.list
}

# Not exact center, but maybe good enough?
extract.center<-function(coords){
  round(c(mean(coords$x),mean(coords$y))) # Good enough?
}