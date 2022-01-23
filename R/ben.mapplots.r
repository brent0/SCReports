#Create own draw.bubble function
 #use to create maps with graduated symbols (such as weights)


draw.bubble= function (x, y, z, maxradius = 1, ...)
{
    cex <- 2 * maxradius/par("cxy")[2]/0.375
    maxz <- max(z, na.rm = T)
    points(x, y, cex = cex * sqrt(z)/sqrt(maxz), ...)
}

 #Create own legend.bubble function
 #use to create legends for maps with graduated symbols (such as weights)

legend.box= function (x, y = NULL, maxradius, mab = 1.2, inset = 0, double = F)
{
    auto <- if (is.character(x))
        match.arg(x, c("bottomright", "bottom", "bottomleft",
            "left", "topleft", "top", "topright", "right", "center"))
    else NA
    asp <- get.asp()
    h <- mab * 2 * maxradius
    w <- h * asp
    if (double)
        h <- h * 2
    usr <- par("usr")
    inset <- rep(inset, length.out = 2)
    if (!is.na(auto)) {
        insetx <- inset[1L] * (usr[2L] - usr[1L])
        left <- switch(auto, bottomright = , topright = , right = usr[2L] -
            w - insetx, bottomleft = , left = , topleft = usr[1L] +
            insetx, bottom = , top = , center = (usr[1L] + usr[2L] -
            w)/2)
        insety <- inset[2L] * (usr[4L] - usr[3L])
        top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3L] +
            h + insety, topleft = , top = , topright = usr[4L] -
            insety, left = , right = , center = (usr[3L] + usr[4L] +
            h)/2)
    }
    else {
        left <- x - 1.2 * asp * maxradius
        top <- y + 1.2 * maxradius
    }
    return(c(left, top, left + w, top - h))
}

legend.bubble= function (x, y = NULL, z, maxradius = 1, n = 3, round = 0, bty = "o",
    mab = 1.2, bg = NULL, inset = 0, pch = 21, pt.bg = NULL,
    txt.cex = 1, txt.col = NULL, font = NULL, txt.adj=c(0.5, -0.5), ...)
{
    if (length(z) == 1)
        legend <- round((seq(0, sqrt(z), length.out = n + 1)^2)[-1],
            round)
    else legend <- round(sort(z), round)
    radius <- maxradius * sqrt(legend)/sqrt(max(legend))
    cex <- 2 * radius/par("cxy")[2]/0.375
    box <- legend.box(x, y, maxradius, mab, inset)
    if (bty == "o")
        rect(box[1], box[2], box[3], box[4], col = bg)
    x <- (box[1] + box[3])/2
    y <- box[2] - mab * maxradius + maxradius
    for (i in length(radius):1) {
        ri <- radius[i]
        cex <- 2 * ri/par("cxy")[2]/0.375
        points(x, y - ri, cex = cex, pch = pch, bg = pt.bg, ...)
        text(x, y - ri * 2, legend[i], adj = txt.adj, cex = txt.cex,
            col = txt.col, font = font)
    }
}

get.asp= function ()
{
    pin <- par("pin")
    usr <- par("usr")
    asp <- (pin[2]/(usr[4] - usr[3]))/(pin[1]/(usr[2] - usr[1]))
    return(asp)
}
