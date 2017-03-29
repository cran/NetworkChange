updateb <-
function(Z, MU, s2, XtX, b0, B0){
    ZU <- Z - MU
    Xtz <- sum(ZU) 
    cV <- 1/(XtX/s2 +  1/B0)
    cE <- cV*(Xtz/s2 + (1/B0)*b0)  
    bhat <- rnorm(1, cE, sqrt(cV)) 
    return(bhat)
}
