plotContour <-
function(OUT, main="", k=8,
                       my.cols = brewer.pal(k, "Spectral")){
  ## k is nlevels of conour()
    Umat <- attr(OUT, "Umat")
    Y <- attr(OUT, "Z")
    m <-  attr(OUT, "m")
    ns <- m + 1
    if(m == 0){
        N.node <- dim(Umat)[2]/2
        Umat1 <- Umat[, 1:N.node]
        Umat2 <- Umat[, (N.node + 1):(2*N.node)]
        U1 <- apply(Umat1, 2, mean)
        U2 <- apply(Umat2, 2, mean)
        plot(Umat1, Umat2, type="n", main="", ## xlim=c(-1.5, 1.5), ylim=c(-1, 1), 
             xlab=expression(u[1]),ylab=expression(u[2]), xaxt="n",yaxt="n")
        abline(h=0,col="gray") ; abline(v=0, col="gray");axis(1);axis(2)
        ## contour overlay
        for (i in 1:N.node){
            z <- kde2d(Umat1[,i], Umat2[,i], n=100)
            contour(z, drawlabels=FALSE, nlevels=k, col=my.cols, add=TRUE)
        }
        points(Umat1, Umat2, cex=0.1, col="grey80")
        text(U1, U2, dimnames(Y)[[1]],cex=0.5, col="black")
    }else{
        if(ns < 5){
            par(mfrow=c(1, ns))
        }else{
            par(mfrow=c(ceiling(ns/4), 4))
        }
        for(i in 1:ns){
            N.node <- dim(Umat[[i]])[2]/2
            Umat1 <- Umat[[i]][, 1:N.node]
            Umat2 <- Umat[[i]][, (N.node + 1):(2*N.node)]
            U1 <- apply(Umat1, 2, mean)
            U2 <- apply(Umat2, 2, mean)
            plot(Umat1, Umat2, type="n", main="", ## xlim=c(-1.5, 1.5), ylim=c(-1, 1), 
                 xlab=expression(u[1]),ylab=expression(u[2]), xaxt="n",yaxt="n")
            abline(h=0,col="gray") ; abline(v=0, col="gray");axis(1);axis(2)
            ## contour overlay
            for (i in 1:N.node){
                z <- kde2d(Umat1[,i], Umat2[,i], n=100)
                ## contour(z,levels=mylevels, add=T)
                contour(z, drawlabels=FALSE, nlevels=k, col=my.cols, add=TRUE)
            }
            points(Umat1, Umat2, cex=0.1, col="grey80")
            text(U1, U2, dimnames(Y)[[1]],cex=0.5, col="black")
        }
    }
    
}
