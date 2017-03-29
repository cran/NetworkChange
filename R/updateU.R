updateU <-
function(K, U, V, R, Zb, s2, eU, iVU){
    for(i in sample(K[1])){
        Ui <- U ; Ui[i,] <- 0
        ## aperm(N*M*T, c(3,2,1)) generates a T*M*N array. 
        VU <-  aperm(array(apply(Ui,1,"*",t(V)), dim=c(R,K[3],K[1])), c(3,2,1))
        zi <- Zb[i,,]
        ## element-wise multiplication of VU with array(rep(zi,R), dim=c(K[1],K[3],R)
        L <-  apply(VU*array(rep(zi,R), dim=c(K[1],K[3],R)), 3, sum) ## L equivalent to X'y
        Q <-  (t(Ui)%*%Ui ) * ( t(V)%*%V ) ## Q equivalent to X'X
        cV <- solve( Q/s2 + iVU ) 
        cE <- cV%*%( L/s2 + iVU%*%eU) 
        U[i,] <- rMVNorm( 1, cE, cV ) 
    } 
    return(U)
}
