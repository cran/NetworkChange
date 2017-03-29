updatebm <-
function(ns, K, s, s2, B0, p, ZU){
    cV <- 1/(sum(sapply(1:ns, function(j){
        prod(K[1:2])*sum(s == j)/s2[j]})) +  diag(1/B0, p))
    cE <- cV*sum(unlist(lapply(ZU, sum)))
    bhat <- rnorm(1, cE, sqrt(cV)) 
    return(bhat)
}
