updateVm <-
function(ns, U, V, Zm, Km, R, s2, eV, iVV, UTA){
    Vm <- as.list(rep(NA, ns))
    for(j in 1:ns){
        Uj <- U[[j]]
        Zj <- Zm[[j]]
        Zj[!UTA[[j]]] <- 0            
        Q <- UU <- ZEP <- L <- cV <- cE <- NA
        if(R == 1){
            Q <- ((t(Uj)%*%Uj)^2 - matrix(sum(Uj^4), R, R))/2
        } else{
            Q <- ((t(Uj)%*%Uj)^2 -
                      matrix(apply(apply(Uj^2,1,function(x){x%*%t(x)}), 1, sum), R, R))/2
        }
        UU <- aperm(array(apply(Uj,1,"*",t(Uj)),
                          dim=c(R, Km[[j]][1], Km[[j]][1]) ),c(2,3,1))
        ZEP <- aperm(Zj, c(3,1,2))
        ZUU <- array(apply(UU,3,function(x){apply(ZEP,1,"*",x)}),
                     dim=c(Km[[j]][1], Km[[j]][1], Km[[j]][3], R))
        L <- apply(ZUU, c(3,4),sum)
        cV <- solve(Q/s2[j] + iVV[[j]])
        cE <- (L/s2[j] + rep(1, Km[[j]][3])%*%t(eV[[j]])%*%iVV[[j]])%*%cV    
        Vm[[j]] <-  rmn(cE, diag(Km[[j]][3]), cV)
    }
    return(Vm)
}
