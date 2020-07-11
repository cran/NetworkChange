## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
require(NetworkChange)
require(sna)
knitr::opts_chunk$set(
  dpi=300,fig.width=7,
  warning=FALSE,
  message=FALSE,
  collapse = TRUE,
  ## fig.asp = 1, 
  comment = "#>"## , 
  ## eval=FALSE
)

## ----img-setup, include=FALSE, cache=FALSE------------------------------------
out.format <- knitr::opts_knit$get("out.format")
img_template <- switch( out.format,
                     word = list("img-params"=list(fig.width=6,
                                                   fig.height=6,
                                                   dpi=150)),
                     {
                       # default
                       list("img-params"=list( dpi=150,
                                               fig.width=6,
                                               fig.height=6,
                                               out.width="504px",
                                               out.height="504px"))
                     } )

knitr::opts_template$set( img_template )

## ----message=FALSE------------------------------------------------------------
library(NetworkChange)

## ---- echo=FALSE, fig.cap="\\label{fig:list}Summary of selected features and functions of the package.", out.width = '100%'----
knitr::include_graphics("list.png")

## -----------------------------------------------------------------------------
set.seed(11173)
n <- 10 ## number of nodes in each cluster
Y <- MakeBlockNetworkChange(n=n, break.point = .5,
                            base.prob=.05, block.prob=.7,
                            T=20, type ="split")
dim(Y)

## ---- message=FALSE, fig.asp = 1, out.width="100%"----------------------------
plotnetarray(Y)

## ---- message=FALSE, fig.asp = 1, out.width="100%"----------------------------
set.seed(11173)
Ymerge <- MakeBlockNetworkChange(n=n, break.point = .5,
                               base.prob=.05, block.prob=.7,
                               T=20, type ="merge")
plotnetarray(Ymerge)

## -----------------------------------------------------------------------------
G <- 100
Yout <-  NetworkChange(Y, R=2, m=1, mcmc=G, burnin=G, verbose=0)

## ---- fig.asp = 0.5, out.width="100%"-----------------------------------------
Ydraw <- drawPostAnalysis(Yout, Y, n.cluster=c(2,3))
multiplot(plotlist=Ydraw, cols=2)

## ---- fig.asp = 0.8, out.width="100%"-----------------------------------------
plotV(Yout, cex=2)

## -----------------------------------------------------------------------------
set.seed(1223)
G <- 100
detect <- BreakDiagnostic(Y, R=2, mcmc=G, burnin=G, verbose=0, break.upper=3)

## ---- fig.asp = 0.25, out.width="100%"----------------------------------------
detect[[1]]

## -----------------------------------------------------------------------------
print(detect[[2]])

## ----ally---------------------------------------------------------------------
data(MajorAlly)
Y <- MajorAlly
time <- dim(Y)[3]
drop.state <- c(which(colnames(Y) == "USA"), which(colnames(Y) == "CHN"))
newY <- Y[-drop.state, -drop.state, 1:62]

## ----test---------------------------------------------------------------------
G <- 100
set.seed(1990)
test.run <- NetworkStatic(newY, R=2, mcmc=G, burnin=G, verbose=0,
                          v0=10, v1=time*2)
V <- attr(test.run, "V")
sigma.mu = abs(mean(apply(V, 2, mean)))
sigma.var = 10*mean(apply(V, 2, var))
v0 <- 4 + 2 * (sigma.mu^2/sigma.var)
v1 <- 2 * sigma.mu * (v0/2 - 1)

## ---- fig.asp = 0.25, out.width="100%"----------------------------------------
set.seed(11223);
detect2 <- BreakDiagnostic(newY, R=2, break.upper=2,
                           mcmc=G, burnin=G, verbose=0,
                           v0=v0, v1=v1)
detect2[[1]]

## ----hncally------------------------------------------------------------------
G <- 100
K <- dim(newY)
m <- 2
initial.s <- sort(rep(1:(m+1), length=K[[3]]))
set.seed(11223);
fit <- NetworkChange(newY, R=2, m=m, mcmc=G, initial.s = initial.s,
                     burnin=G, verbose=0, v0=v0, v1=v1)

## ---- fig.asp = 0.8, out.width="100%"-----------------------------------------
attr(fit, "y") <- 1:K[[3]]
plotState(fit, start=1)

## ---- fig.asp = 0.33, out.width="100%"----------------------------------------
p.list <- drawPostAnalysis(fit, newY, n.cluster=c(4, 4, 3))
multiplot(plotlist = p.list, cols=3)

## ---- fig.asp = 0.33, out.width="100%"----------------------------------------
drawRegimeRaw(fit, newY)

