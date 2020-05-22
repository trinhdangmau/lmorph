##list = setdiff(ls(), lsf.str())
clear.functions<-function(){
  x <- lsf.str()
  remove(list = ls(),  envir = .GlobalEnv)
}

#cat(paste("Do you want to remove all functions", "(y/n)?"), 
 #   "\n")
#ans <- readLines(n = 1)
#if (ans == "y") {
  #selected[ii, 1] <- fix$x
  #selected[ii, 2] <- fix$y
#}
#if (ans == "n") {
 # cat(paste("Select Landmark ", ii, " Again"), 
  #    "\n")
#}