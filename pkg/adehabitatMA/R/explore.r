explore <- function (ka, coltxt="black",
                     hscale = 1, vscale = 1,
                     panel.last=NULL, ...)
{
    if (!inherits(ka,"SpatialPixelsDataFrame"))
      stop("ka should be of class SpatialPixelsDataFrame")
    gridded(ka) <- TRUE
    gr <- gridparameters(ka)
    if (nrow(gr) > 2)
        stop("ka should be defined in two dimensions")
    if ((gr[1, 2] - gr[2, 2])> get(".adeoptions", envir=.adehabitatMAEnv)$epsilon)
        stop("the cellsize should be the same in x and y directions")

    e1 <- new.env(parent = baseenv())
    assign("nn", NULL, envir=e1)
    assign("whi", 1, envir=e1) ## current graph

    ## function replot: to replot the content of the object
    replot <- function() {
        if (get("lim", envir=e1)) {
            assign("xlim", range(coordinates(ka)[,1]), envir=e1)
            assign("ylim", range(coordinates(ka)[,2]), envir=e1)
        }
        assign("nn", n2mfrow(ncol(slot(ka,"data"))), envir=e1)
        split.screen(c(1,2))
        split.screen(get("nn", envir=e1), screen=2)
        on.exit(close.screen(all.screens=TRUE))
        na <- names(slot(ka,"data"))
        sapply(1:length(na), function(j) {
            i <- na[j]
            screen(j+2)
            opar <- par(mar=c(0,0,2,0))
            kab <- as(ka[,i], "SpatialGridDataFrame")
            kab <- as.image.SpatialGridDataFrame(kab)
            image(kab, xlim=get("xlim", envir=e1), ylim=get("ylim", envir=e1), asp=1, axes=FALSE, ...)
            if (!is.null(panel.last))
                panel.last()
            title(main=i)
            if (get("ajoupo", envir=e1)) {
                text(get("a5", envir=e1)[1], get("a5", envir=e1)[2],
                     c(kab$z)[get("ia5", envir=e1)], col=coltxt, font=2, cex=1.15)
            }
            box()
            par(opar)
        })
        screen(1)
        opar <- par(mar=c(0,0,2,0))
        kab <- as(ka[,get("whi", envir=e1)], "SpatialGridDataFrame")
        kab <- as.image.SpatialGridDataFrame(kab)
        image(kab, xlim=get("xlim", envir=e1), ylim=get("ylim", envir=e1), asp=1,  axes=FALSE,...)
        if (!is.null(panel.last))
            panel.last()
        title(main=na[get("whi", envir=e1)])
        assign("cusr", par("usr"), envir=e1)
        assign("cplt", par("plt"), envir=e1)
        if (get("ajouli", envir=e1))
            lines(c(get("a1", envir=e1)[1], get("a2", envir=e1)[1]), c(get("a1", envir=e1)[2], get("a2", envir=e1)[2]), lwd = 2,
                  col = "black")
        box()
        par(opar)
    }


    N <- length(slot(ka,"data"))
    assign("D", 0, envir=e1)
    assign("xlim", range(coordinates(ka)[,1]), envir=e1)
    assign("ylim", range(coordinates(ka)[,2]), envir=e1)
    assign("lim", TRUE, envir=e1)
    assign("ajouli", FALSE, envir=e1)
    assign("ajoupo", FALSE, envir=e1)
    opt <- options(warn = -1)
    on.exit(options(opt))

    if (!require(tkrplot))
        stop("'tkrplot' package needed\n")

    help.txt <- paste("\n-------- to obtain this help, type 'h' ------------------",
                      "z/o            -- Zoom in/Out",
                      "Right-Click    -- Identify value",
                      "Left-Click     -- Select variable /
                  Measure the distance between two points",
                      "q              -- Quit",
                      "---------------------------------------------------------",
                      "\n", sep = "\n")

    tt <- tktoplevel()
    tkwm.title(tt, "Exploration of SpatialPixelsDataFrame")
    img <- tkrplot(tt, replot, hscale = hscale, vscale = vscale)
    txt <- tktext(tt, bg = "white", font = "courier 10")
    scr <- tkscrollbar(tt, repeatinterval = 5, command = function(...) tkyview(txt, ...))
    tkconfigure(txt, yscrollcommand = function(...) tkset(scr,
                     ...))
    tkpack(img, side = "top")
    tkpack(txt, side = "left", fill = "both", expand = TRUE)
    tkpack(scr, side = "right", fill = "y")
    iw <- as.numeric(tcl("image", "width", tkcget(img, "-image")))
    ih <- as.numeric(tcl("image", "height", tkcget(img, "-image")))

    showz <- function() tkrreplot(img)
    type <- function(s) {
        tkinsert(txt, "end", s)
        tksee(txt, "end")
    }
    type(help.txt)
    cc <- function(x, y) {
        x <- (as.real(x) - 1)/iw
        y <- 1 - (as.real(y) - 1)/ih
        px <- (x - get("cplt", envir=e1)[1])/(get("cplt", envir=e1)[2] - get("cplt", envir=e1)[1])
        py <- (y - get("cplt", envir=e1)[3])/(get("cplt", envir=e1)[4] - get("cplt", envir=e1)[3])
        ux <- px * (get("cusr", envir=e1)[2] - get("cusr", envir=e1)[1]) + get("cusr", envir=e1)[1]
        uy <- py * (get("cusr", envir=e1)[4] - get("cusr", envir=e1)[3]) + get("cusr", envir=e1)[3]
        c(ux, uy)
    }
    cc2 <- function(x, y) {
        x <- (as.real(x) - 1)/iw
        y <- 1 - (as.real(y) - 1)/ih
        px <- (x - get("cplt", envir=e1)[1])/(get("cplt", envir=e1)[2] - get("cplt", envir=e1)[1])
        py <- (y - get("cplt", envir=e1)[3])/(get("cplt", envir=e1)[4] - get("cplt", envir=e1)[3])
        c(px, py)
    }

    mm.t <- function(x, y) {
        veri <- cc2(x,y)
        if (veri[1]<0.5) {
            i <- get("D", envir=e1)
            if (i == 0) {
                ux <- (2*veri[1]) * (get("cusr", envir=e1)[2] - get("cusr", envir=e1)[1]) + get("cusr", envir=e1)[1]
                uy <- veri[2]* (get("cusr", envir=e1)[4] - get("cusr", envir=e1)[3]) + get("cusr", envir=e1)[3]
                assign("a1", c(ux, uy), envir=e1)
                assign("D", 1, envir=e1)
            }
            if (i == 1) {
                ux <- (2*veri[1]) * (get("cusr", envir=e1)[2] - get("cusr", envir=e1)[1]) + get("cusr", envir=e1)[1]
                uy <- veri[2]* (get("cusr", envir=e1)[4] - get("cusr", envir=e1)[3]) + get("cusr", envir=e1)[3]
                assign("a2", c(ux, uy), envir=e1)
                assign("D", 0, envir=e1)
                di <- sqrt(sum((get("a2", envir=e1) - get("a1", envir=e1))^2))
                type(paste("distance:", di, "\n"))
                assign("ajouli", TRUE, envir=e1)
                showz()
                assign("ajouli", FALSE, envir=e1)
            }
        } else {
            veri[1] <- 2*(veri[1]-0.5)
            rc <- veri
            rc[1] <- as.numeric(cut(veri[2],seq(0,1,length=get("nn", envir=e1)[1]+1)))
            rc[2] <- as.numeric(cut(veri[1],seq(0,1,length=get("nn", envir=e1)[2]+1)))
            rc[1] <- get("nn", envir=e1)[1] - rc[1] +1
            assign("whi", get("nn", envir=e1)[2]*(rc[1]-1) + rc[2], envir=e1)
            showz()
        }
        return()
    }
    mm.t2 <- function(x, y) {
        veri <- cc2(x,y)
        if (veri[1]<0.5) {
            ux <- (2*veri[1]) * (get("cusr", envir=e1)[2] - get("cusr", envir=e1)[1]) + get("cusr", envir=e1)[1]
            uy <- veri[2]* (get("cusr", envir=e1)[4] - get("cusr", envir=e1)[3]) + get("cusr", envir=e1)[3]
            assign("a3", c(ux, uy), envir=e1)
            xyc <- as.image.SpatialGridDataFrame(ka)
            yc <- rep(xyc$y, each=length(xyc$x))
            xc <- rep(xyc$x, length(xyc$y))
            iy <- rep(1:length(xyc$y), each=length(xyc$x))
            ix <- rep(1:length(xyc$x), length(xyc$y))

            di <- sqrt((xc - get("a3", envir=e1)[1])^2 + (yc - get("a3", envir=e1)[2])^2)
            assign("a5", unlist(cbind(xc,yc)[which.min(di),]), envir=e1)
            assign("ia5", which.min(di), envir=e1)
            assign("ajoupo", TRUE, envir=e1)
            showz()
            assign("ajoupo", FALSE, envir=e1)
            type(paste("Index:", get("ia5", envir=e1),"\n"))
        }
        return()
    }
    mm.mouset <- function(x, y) {
        veri <- cc2(x,y)
        if (veri[1]<0.5) {
            ux <- (2*veri[1]) * (get("cusr", envir=e1)[2] - get("cusr", envir=e1)[1]) + get("cusr", envir=e1)[1]
            uy <- veri[2]* (get("cusr", envir=e1)[4] - get("cusr", envir=e1)[3]) + get("cusr", envir=e1)[3]
            assign("a8", c(ux, uy), envir=e1)
        }
        return()
    }
    kb <- function(A) {
        key <- tolower(A)
        if (key == "q") {
            tkdestroy(tt)
            return("OK - Finished")
        }
        if (key == "z") {
            tmppx <- (get("cusr", envi=e1)[1:2] - get("cusr", envir=e1)[1])
            assign("xlim", c(get("a8", envir=e1)[1] - tmppx[2]/4, (get("a8", envir=e1)[1] + tmppx[2]/4)), envir=e1)
            assign("tmppy", (get("cusr", envir=e1)[3:4] - get("cusr", envir=e1)[3]), envir=e1)
            assign("ylim", c(get("a8", envir=e1)[2] - get("tmppy", envir=e1)[2]/4, (get("a8", envir=e1)[2] + get("tmppy", envir=e1)[2]/4)), envir=e1)
            assign("lim", FALSE, envir=e1)
            showz()
        }
        if (key == "o") {
            assign("lim", TRUE, envir=e1)
            showz()
        }
    }
    showz()
    tkbind(tt, "<Key>", kb)
    tkbind(img, "<Button-1>", mm.t)
    tkbind(img, "<Motion>", mm.mouset)
    tkbind(img, "<Button-3>", mm.t2)
    tkwait.window(tt)

}
