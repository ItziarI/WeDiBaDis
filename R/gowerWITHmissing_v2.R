gowerWITHmissing_v2 <- function(x, vc, vb, vn){
############### Calculates gower distance for mixed variables #######
##                  WITH missing values
# Input:
# x: data matrix
# vc: column position for cuantitative variables
# vb: column position for binary variables
# vn: column position for nominal variables
# Output:
# d: distance matrix
####################################################################

x <- as.matrix(x)
dx <- dim(x)
n <- dx[1]
p <- dx[2]


d <- matrix(0, n, n)      # distance matrix
s <- matrix(0, n, n)      # similarity matrix

a <- matrix(0,n, n)       # concordance-concordance
di <- matrix(0,n, n)      # discordance-discordance

alfa <- matrix(0,n, n)    # concordance for nominal

                      
xcuant <- x[, vc, drop=FALSE]
R <- apply(xcuant, 2, max, na.rm=TRUE) - apply(xcuant, 2, min, na.rm=TRUE)

xbin <- x[, vb, drop=FALSE]
xnom <- x[, vn, drop=FALSE]

for (i in 1:(n-1)){
     for (j in (i+1):n){



         # Cuantitative variables
         xi <- xcuant[i,]
         mi <- sapply(xi, is.na)
         xj <- xcuant[j,]
         mj <- sapply(xj, is.na)
         m <- !(mi|mj)
         swc <- sum(m)                              # weights for cuant.
         s[i, j] <- sum(1 - abs(xi[m] - xj[m])/R[m])
            

         # Binary variables
         xi <- xbin[i, ]
         mi <- sapply(xi, is.na)
         xj <- xbin[j, ]
         mj <- sapply(xj, is.na)
         m <- !(mi|mj)
         swb <- sum(m)                              # weights for bin.
         a[i, j] <- sum(xi[m]*xj[m])
         di[i, j] <- sum((1 - xi[m])*(1 - xj[m]))
     

          # Nominal variables
         xi <- xnom[i, ]
         mi <- sapply(xi, is.na)
         xj <- xnom[j, ]
         mj <- sapply(xj, is.na)
         m <- !(mi|mj)
         swn <- sum(m)                           #  weights for nom.
         alfa[i, j] <- sum(xi[m]==xj[m])       
    


        s[i,j] <- (s[i,j] + a[i,j] + alfa[i,j])/(swc + (swb-di[i,j]) + swn)
        d[i,j] <- sqrt(2*(1-s[i,j]))
        d[j,i] <- d[i, j]

    } # for (j in (i+1):n){
} # for (i in 1:(n-1)){


return(d)
}
