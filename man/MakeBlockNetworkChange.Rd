\name{MakeBlockNetworkChange}
\alias{MakeBlockNetworkChange}
\title{
 Build a synthetic block-structured temporal data with breaks
}
\description{
MakeBlockNetworkChange generates a block-structured temporal data with breaks. 
}
\usage{
MakeBlockNetworkChange(n=10, break.point = .5, 
                       block.number = 3, base.prob=.05, block.prob=.5,
                       shape=10, T=40, type ="merge")
}
\arguments{
  \item{n}{The number of nodes within a block. The total number of nodes is n*block.number.
  }
  \item{break.point}{
  The point of break. 0 indicates the beginning, 0.5 indicates the middle, and 1 indicates the end. 
}
  \item{block.number}{
  The number of blocks.
}
  \item{base.prob}{
  The probability of link among non-block members.
}
  \item{block.prob}{
  The probability of link among within-block members.
}
  \item{shape}{
  The speed of breaks. The larger shape is, the faster the transition is. 0 is the minimum. 	
}
  \item{type}{
  The type of network changes. Options are "constant", "merge", "split", "merge-split", "split-merge."
  If "constant" is chosen, the number of breaks is zero.
  If "merge" or "split" is chosen, the number of breaks is one.
  If either "merge-split" or "split-merge" is chosen, the number of breaks is two.
  
}
  \item{T}{
  The length of time.
}
}
\value{
 An output of \code{MakeBlockNetworkChange} contains a symmetric block-structured temporal network data set with breaks.
}
\examples{
\dontrun{
## A break occurs in the middle.
set.seed(1973)
## Generate an array (30 by 30 by 40) with block transitions from 3 blocks to 2 blocks
## A cluster-merging break is planted in the middle.
Y <- MakeBlockNetworkChange(n=10, T=40, type ="merge")
G <- 100 ## only 100 mcmc scans to save time
## latent node positions
plotU(out1)

## latent connection rules
plotV(out1)
}
}
