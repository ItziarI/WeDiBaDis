print.wdb <- function(x,...)
{
        cat("Discriminant method: ", x$method, "\n")
        cat("Leave-one-out confusion matrix: \n")
        print(x$conf)
        if(is.null(x$pred))
        {
                cat(" \nNo predicted individuals")
        }else
        {
                cat("\nPrediction for new individuals: \n")
                print(x$pred)
        }
}