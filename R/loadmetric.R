#### load biometric data ##########
loadmetric<-function(path, char.name=c("DPl", "DPw", "VPl", "VPw", "HawD", "HawV", "Tl", "Cl", "ASl"), 
                     method = "list", scale = F, maxlength = NULL){
  file.sources <- list.files(path, pattern="*.txt$", full.names=TRUE, ignore.case=TRUE)
  name<-gsub(".txt", "", basename(file.sources))
  ncharac <- length(char.name)
  
  methods<-c("list", "GlobalEnv")
  method<-match.arg(method, methods)
  switch(method, list = {
    out<-NULL
    out[name]<-list(NULL)
    for(i in 1:length(file.sources)){
      xmatrix<-readmetric(file.sources[i], char.name = char.name, scale = scale, maxlength = maxlength)
      out[[name[i]]]<-xmatrix
    }
    out<-list(main = out, splist = name)
    class(out)<-"metricdata"
    return(out)
  }, GlobalEnv={
    for(i in 1:length(file.sources)){
      xmatrix<-readmetric(file.sources[i], char.name = char.name, scale = scale, maxlength = maxlength)
      assign(name[i], xmatrix, envir = .GlobalEnv)
    }
    assign("splist", name, envir = .GlobalEnv)
  })
}