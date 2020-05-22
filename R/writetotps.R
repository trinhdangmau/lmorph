writetotps<-function (A, file){
  n <- dim(A)[3]
  k <- dim(A)[2]
  p <- dim(A)[1]
  lmline <- ifelse(k == 2, paste("LM=", p, sep = ""), paste("LM3=", p, sep = ""))
  file.create(file, showWarnings = TRUE)
  for (i in 1:n) {
    write(lmline, file, append = TRUE)
    write.table(A[, , i], file, col.names = FALSE, row.names = FALSE, append = TRUE)
    if (is.null(dimnames(A)[[3]]) == FALSE) {
      idline <- paste("ID=", dimnames(A)[[3]][i], sep = "")
      write(idline, file, append = TRUE)
    }
    #write("", file, append = TRUE)
  }
}