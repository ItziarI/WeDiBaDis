vargeo<-function(d){
    d <- as.matrix(d)
    frec <- dim(d)[1]
    var <- sum(d * d)/(2 * frec * frec)
    return(var)
}