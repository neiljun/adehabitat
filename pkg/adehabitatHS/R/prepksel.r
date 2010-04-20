prepksel <- function(sa, hr, locs)
{
    ## Verifications
    if (!inherits(sa, "SpatialPixelsDataFrame"))
        stop("should be an object of class SpatialPixelsDataFrame")
    gridded(sa) <- TRUE
    gr <- gridparameters(sa)
    if (nrow(gr) > 2)
          stop("x should be defined in two dimensions")
    if ((gr[1, 2] - gr[2, 2])> get(".adeoptions", envir=.adehabitatMAEnv)$epsilon)
        stop("the cellsize should be the same in x and y directions")
    if (!inherits(hr, "SpatialPixelsDataFrame"))
        stop("should be an object of class SpatialPixelsDataFrame")
    gridded(hr) <- TRUE
    gr <- gridparameters(hr)
    if (nrow(gr) > 2)
          stop("x should be defined in two dimensions")
    if ((gr[1, 2] - gr[2, 2])> get(".adeoptions", envir=.adehabitatMAEnv)$epsilon)
        stop("the cellsize should be the same in x and y directions")
    if (nrow(hr)!=nrow(sa))
        stop("hr and sa should have the same dimensions")
    if (!inherits(locs, "SpatialPixelsDataFrame"))
        stop("should be an object of class SpatialPixelsDataFrame")
    gridded(locs) <- TRUE
    gr <- gridparameters(locs)
    if (nrow(gr) > 2)
          stop("x should be defined in two dimensions")
    if ((gr[1, 2] - gr[2, 2])> get(".adeoptions", envir=.adehabitatMAEnv)$epsilon)
        stop("the cellsize should be the same in x and y directions")
    if (any(dim(locs)!=dim(hr)))
        stop("hr and locs should have the same dimensions (rows and columns)")


    dat <- slot(sa, "data")
    df <- do.call("rbind", lapply(1:ncol(hr), function(i) {
        dv <- hr[,i]
        dv <- dv[!is.na(slot(dv,"data")[,1]),]
        dat[overlay(sa, dv),]
    }))
    loct <- slot(locs, "data")
    locdf <- unlist(lapply(1:ncol(hr), function(i) {
        dv <- hr[,i]
        dv <- dv[!is.na(slot(dv,"data")[,1]),]
        loct[overlay(locs, dv),i]
    }))
    fac <- unlist(lapply(1:ncol(hr), function(i) {
        dv <- hr[,i]
        dv <- dv[!is.na(slot(dv,"data")[,1]),]
        rep(names(loct)[i], length(overlay(locs, dv)))
    }))
    return(list(tab=df, weight=locdf, factor=fac))
}
