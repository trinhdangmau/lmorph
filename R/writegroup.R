writegroup<-function (x, file){
  n <- length(x)
  file.create(file, showWarnings = TRUE)
  for (i in 1:n) {
    write(x[i], file, append = TRUE)
    #write("", file, append = TRUE)
  }
}