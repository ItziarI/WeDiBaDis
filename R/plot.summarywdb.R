plot.summarywdb <- function(x, col=NULL, ...)
{
        if (!inherits(x, "summarywdb")) 
                stop("use only with summary of a \"wdb\" object")
        etiq <- dimnames(x)[[2]]
        if (is.null(col)) 
        {
            col <- grDevices::rainbow(3)    
        }      
        graphics::barplot(x, names.arg=etiq, beside=TRUE, col=col, ...)
        graphics::mtext("Sensitivity", 3, line=3, adj=1, col=col[1])
        graphics::mtext("Positive predicted value", 3, line=2, adj=1, col=col[2])
        graphics::mtext("Specificity", 3, line=1, adj=1, col=col[3])
        graphics::mtext( "Classes", 1, line=2)
        graphics::mtext( "Percentage", 2, line=2)
}

