#
#si es related en data y en new.ind se ha de poner en data la distancia relate y en new.ind lo mismo
WDBdisc <- function(data, datatype, classcol=1, new.ind=NULL, distance="euclidean", method="WDB", type){
    if (datatype =="d"){
       aux<-as.matrix(data[, -1])
       if(dim(aux)[1]!=(dim(aux)[2])) stop("incorrect distance matrix")
    }
    p <- dim(data)[2]
    classcol <- as.numeric(classcol)
    if ((classcol<0)|(classcol > p) ){
           stop("incorrect number of column for the class variable")
    }

    DISTANCES <- c("euclidean", "correlation", "Mahalanobis", "Gower", 
        "Bhattacharyya","BrayCurtis","Orloci","Hellinger", "Prevosti")
    if (datatype == "m"){
      distance2 <- pmatch(distance, DISTANCES)
      if (is.na(distance2)) 
        stop("invalid distance method")
    }
    DMETHODS<-c("DB","WDB")
    method2<-pmatch(method,DMETHODS)
    if (is.na(method2)) 
        stop("invalid discriminant method")
    if (any(dim(data) < 2L)) 
            stop("'data' must have at least 2 rows and columns")

   
    clasetot <- data[,classcol]
    clasetot <- as.factor(clasetot)
    etiq <- levels(clasetot)
    if (datatype =="m"){
            x <- data[, -classcol]
            if (distance=="Gower") {type <- lapply(type, function(a){a-1})}
            dtot <- as.matrix(distancia(x,distance,type))
    }else{
            dtot <- as.matrix(data[, -classcol])
    }
    n <- dim(dtot)[1]
    K <- length(tabulate(clasetot))
    clasetot.num <- as.numeric(clasetot)
    Phis <- matrix(NA, nrow=n, ncol=K)
    Is <- matrix(NA, nrow=n, ncol=K)
    
    for (i in 1:n)
    {
            clase.i <- clasetot.num[i]
            clase <- clasetot.num[-i]
            f <- table(clase)
            dxi <- vector("list", K)
            d <- vector("list", K)
            for (k in 1:K)
            {
                    selec <- clase==k
                    aux <- dtot[i,-i]
                    dxi[[k]] <- aux[selec]
                    aux <- dtot[-i, -i]
                    d[[k]] <- aux[selec, selec]
            }
            vg <- sapply(d, vargeo)
            pre <- sapply(dxi, function(x){sum(x^2)/(length(x))})
            Phis[i,] <- pre - vg
            Is[i,] <- 1/(1+Phis[i,]/vg)
    }
    rm(clase.i, clase,dxi, d)
    
    if (method == "WDB")
    {
            M <- Phis*(1-Is)
    }else{
            M <- Phis
    }
    
    Pred <- apply(M, 1, function(x){which(x==min(x))})
    Pred2 <- factor(Pred, levels=1:K, labels = etiq)
    conf.matrix <- table(clasetot, Pred2, dnn=c("Real", "Predicted"))
    rm(M, Phis, Is, Pred, Pred2)
    # Prediction of new individual
    if(is.null(new.ind))
    {
            pred.matrix <- NULL
    }else 
    {
            new.ind <- as.matrix(new.ind)
            if (datatype=="m")
            {
                    if(dim(x)[2]!=dim(new.ind)[2]){stop("new.ind does not have ", dim(x)[2], " columns")}
                    aux <- rbind(x, new.ind)        
                    aux <- as.matrix(distancia(aux, distance, type)) 
                    d.new <- aux[(n+1):dim(aux)[1], 1:n, drop=FALSE]
            }else if(datatype =="d")
            {
                    if(dim(dtot)[2]!=dim(new.ind)[2]){stop("new.ind does not have ", dim(dtot)[2], " columns")}
                    d.new <- new.ind
            }

            m <- dim(new.ind)[1]
            Phis <- matrix(NA, nrow=m, ncol=K)
            Is <- matrix(NA, nrow=m, ncol=K)
            d <- vector("list", K)
            dxi <- vector("list", K)
            for (k in 1:K)
            {
                    selec <- clasetot.num==k
                    dxi[[k]] <- d.new[, selec, drop=FALSE]
                    d[[k]] <- dtot[selec, selec]
             }
            vg <- sapply(d, vargeo)
            pre <- sapply(dxi, function(x){apply(x, 1, function(y){sum(y^2)/length(y)})})
            if(!is.matrix(pre)){pre <- matrix(pre, nrow=m)}
            for (k in 1:K)
            {
                    aux <- pre[,k] - vg[k]
                    Phis[,k] <- aux
                    temp <- 1+aux/vg[k]
                    Is[,k] <- 1/temp
            }
 
            if (method == "WDB")
            {
                    M <- Phis*(1-Is)
            }else{
                    M <- Phis
            }
            
            Pred.ind <- apply(M, 1, function(x){which(x==min(x))})
            pred.matrix <- matrix( etiq[Pred.ind], ncol=1)
            dimnames(pred.matrix) <- list(1:m, "Pred. class")
    }
    input <- ifelse(datatype =="d", "custom dist.", DISTANCES[distance2])
    RVAL <- list(conf=conf.matrix, pred=pred.matrix, parameters=list(method=method, input=input))
    class(RVAL) <- "wdb"
    return(RVAL)
}






