rbind.flex <- function(...) {
    x <- list(...)
    L <- sapply(x, length)
    x <- x[which(L > 0)]

    if (length(x) == 0)
        return(NULL)
    for (i in 1:length(x)) {
        if (!is.matrix(x[[i]]) && !is.data.frame(x[[i]])) {
            cols <- names(x[[i]])
            x[[i]] <- matrix(x[[i]],nrow=1)
            colnames(x[[i]]) <- cols
        }
    }
    cols <- unique(unlist(lapply(x,colnames)))
    for (i in 1:length(x)) {
        new.cols <- setdiff(cols,colnames(x[[i]]))
        if (length(new.cols) > 0) {
            if (is.matrix(x[[i]])) {
                new.x <- matrix(NA,ncol=length(cols),nrow=nrow(x[[i]]))
                colnames(new.x) <- cols
                new.x[,colnames(x[[i]])] <- x[[i]]
                x[[i]] <- new.x
            }
            else {
                for (col in new.cols)
                    x[[i]][[col]] <- NA
            }
        }
    }
    do.call(rbind, x)
}
