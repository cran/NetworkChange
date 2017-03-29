\name{plotV}
\alias{plotV}

\title{
Plot of layer-specific network generation rules.
}
\description{
Plot layer-specific network generation rules.
}
\usage{
plotV(OUT, main = "", cex = 2)
}
\arguments{
  \item{OUT}{
  Output of networkchange objects.
}
  \item{main}{
  Title of plot	
}
 \item{cex}{
 Size of dots.
}
}
\examples{
set.seed(1973)

## generate an array with two constant blocks
Y <- MakeBlockNetworkChange(n=10, shape=10, T=40, type ="constant")
out0 <- NetworkStatic(Y, R=2, mcmc=10, burnin=10,
verbose=10, UL.Normal = "Orthonormal")

## latent node positions
plotV(out0)

}