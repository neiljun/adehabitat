.charhull <- function(xy)
{
    require("tripack")
    xy <- as.data.frame(xy)
    names(xy) <- c("x","y")
    tri <- tri.mesh(xy)
    tri <- triangles(tri)
    tri <- tri[,1:3]
    tri <- lapply(1:nrow(tri), function(i) {
        rbind(xy[tri[i,1],],xy[tri[i,2],],xy[tri[i,3],],xy[tri[i,1],])
    })
    area <- sapply(tri, function(x) {
        y <- as(x, "gpc.poly")
        area.poly(y)
    })
    tri <- tri[order(area)]
    area <- area[order(area)]
    df <- data.frame(area=area, percent=100*c(c(1:length(area))/length(area)))
    sp <- SpatialPolygons(lapply(1:length(tri), function(i) {
        Polygons(list(Polygon(as.matrix(tri[[i]]))), i)
    }))
    res <- SpatialPolygonsDataFrame(sp, df)
    return(res)
}

CharHull <-  function(xy, unin = c("m", "km"), unout = c("ha", "m2", "km2"),
              duplicates = c("random", "remove"), amount = NULL)
{
    if (!inherits(xy, "SpatialPoints"))
        stop("xy should inherit the class SpatialPoints")
    pfs <- proj4string(xy)
    if (inherits(xy, "SpatialPointsDataFrame")) {
        if (ncol(xy) != 1) {
            warning("xy contains more than one column. id ignored")
            m <- 1
            id <- rep("a", nrow(coordinates(xy)))
        }
        else {
            id <- xy[[1]]
            m <- 2
        }
    }
    else {
        m <- 1
        id <- rep("a", nrow(coordinates(xy)))
    }
    id <- factor(id)
    duplicates <- match.arg(duplicates)
    unin <- match.arg(unin)
    unout <- match.arg(unout)
    xy <- as.data.frame(coordinates(xy))
    lixy <- split(xy, id)
    res <- list()

    for (i in 1:length(lixy)) {
        x <- as.matrix(lixy[[i]])
        levv <- factor(apply(x, 1, paste, collapse = " "))
        if (duplicates == "remove") {
            x <- as.data.frame(x)
            x <- as.matrix(do.call("rbind", lapply(split(x, levv),
                                                   function(z) z[1, ])))
        }
        else {
            x <- as.data.frame(x)
            sam1 <- x[, 1] - jitter(x[, 1], 1, amount)
            sam2 <- x[, 1] - jitter(x[, 1], 1, amount)
            lixyt <- split(x, levv)
            lisam1 <- split(sam1, levv)
            lisam2 <- split(sam2, levv)
            x <- as.matrix(do.call("rbind", lapply(1:length(lixyt),
                function(a) {
                  z <- lixyt[[a]]
                  if (nrow(z) > 1) {
                    z[, 1] <- z[, 1] + lisam1[[a]]
                    z[, 2] <- z[, 2] + lisam2[[a]]
                  }
                  return(z)
                })))
        }


        resa <- .charhull(x)
        if (unin == "m") {
            if (unout == "ha")
                resa[[1]] <- resa[[1]]/10000
            if (unout == "km2")
                resa[[1]] <- resa[[1]]/1e+06
        }
        if (unin == "km") {
            if (unout == "ha")
                resa[[1]] <- resa[[1]] * 100
            if (unout == "m2")
                resa[[1]] <- resa[[1]] * 1e+06
        }
        res[[i]] <- resa
        if (!is.na(pfs))
            proj4string(res[[i]]) <- CRS(pfs)
    }
    if (m == 1) {
        return(res[[1]])
    }
    else {
        names(res) <- names(lixy)
        class(res) <- "MCHu"
        return(res)
    }
}
