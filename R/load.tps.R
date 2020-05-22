load.tps<-function(path, method = "list"){
  file.sources <- list.files(path, pattern="*.tps$", full.names=TRUE, ignore.case=TRUE)
  name<-gsub(".tps", "", basename(file.sources))

  methods<-c("list", "GlobalEnv")
  method<-match.arg(method, methods)
  
  switch(method, list={
    out<-NULL
    out[name]<-list(NULL)
    
    for(i in 1:length(file.sources)){
      x<-readland.tps2(file.sources[i])
      out[[name[i]]]<-x
    }
    out<-list(main = out, splist = name)
    class(out)<-"tpsdata"
    return(out)
    
  }, GlobalEnv={
    for(i in 1:length(file.sources)){
      x<-readland.tps(file.sources[i])
      assign(name[i], x, envir = .GlobalEnv)
    }
    assign("splist", name, envir = .GlobalEnv)
  })
}