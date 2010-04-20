


rec <- function(x, slsp=c("remove","missing"))
{
    ## Verifications
    if (!inherits(x, "ltraj"))
        stop("x should be of class \"ltraj\"")

    ## Recomputation
    slsp <- match.arg(slsp)
    if (attr(x,"typeII")) {
        y <- .traj2df(.ltraj2traj(x))
        if (!is.null(infolocs(x))) {
            infol <- do.call("rbind", infolocs(x))
            al <- as.ltraj(xy=y[,c("x","y")], date=y$date,
                           id=y$id, burst=y$burst, slsp=slsp,
                           typeII=TRUE, infolocs=infol)
        } else {
            al <- as.ltraj(xy=y[,c("x","y")], date=y$date,
                           id=y$id, burst=y$burst, slsp=slsp,
                           typeII=TRUE)
        }
        return(al)
    } else {
        attr(x,"typeII") <- TRUE
        y <- .traj2df(.ltraj2traj(x))
        if (!is.null(infolocs(x))) {
            infol <- do.call("rbind", infolocs(x))
            al <- as.ltraj(xy=y[,c("x","y")], id=y$id,
                           burst=y$burst, slsp=slsp,
                           typeII=FALSE, infolocs=infol)
        } else {
            al <- as.ltraj(xy=y[,c("x","y")], id=y$id,
                           burst=y$burst, slsp=slsp,
                           typeII=FALSE)
        }
        return(al)
    }
}
