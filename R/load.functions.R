#### load all .R from a folder ############
load.functions<-function(path){
  file.sources <- list.files(path, pattern="*.R$", full.names=TRUE, ignore.case=TRUE)
  sapply(file.sources,source,.GlobalEnv)
}