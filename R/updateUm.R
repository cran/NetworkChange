updateUm <-
function(ns, U, V, R, Zm, Km, ej, s2, eU, iVU, UL.Normal){
    for(j in 1:ns){
        Vj <-  matrix(V[ej[[j]] == 1, ], nrow=sum(ej[[j]]), ncol=R)
        for(i in sample(Km[[j]][1])){
            Ui <- U[[j]]
            Ui[i,] <- 0
            VU <-  aperm(array(apply(Ui,1,"*",t(Vj)), dim=c(R, Km[[j]][3], Km[[j]][1])), c(3,2,1))
            zi <- Zm[[j]][i,,]
            L <-  apply(VU*array(rep(zi,R), dim=c(Km[[j]][1], Km[[j]][3], R)), 3, sum) 
            Q <-  (t(Ui)%*%Ui) * (t(Vj)%*%Vj)
            cV <- solve( Q/s2[j] + iVU[[j]] )
            cE <- cV%*%( L/s2[j] + iVU[[j]]%*%eU[[j]])
            U[[j]][i,] <- rMVNorm(1, cE, cV ) 
        }
    }
    ## UL normalization
    if (UL.Normal == "Normal"){
        for(j in 1:ns){
            U[[j]] <- Unormal(U[[j]])
        }
    }else if(UL.Normal == "Orthonormal"){
        for(j in 1:ns){
            U[[j]] <- GramSchmidt(U[[j]])
        }
    }
    return(U)
}
