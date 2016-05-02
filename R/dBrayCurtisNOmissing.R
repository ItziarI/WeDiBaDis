dBrayCurtisNOmissing <- function(x)
{
        n <- dim(x)[1]
        parcial <- apply(x, 1, sum)
        
        d<-as.matrix(dist(x,method="manhattan"))
        
        for (i in 1:(n-1)){
                for (j in (i+1):n){
                        d[i,j] <- d[i,j]/(parcial[i]+parcial[j])
                        d[j,i] <- d[i, j]
                }
        }
        return(d)
}