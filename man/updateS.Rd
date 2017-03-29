\name{updateS}
\alias{updateS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update latent states.
}
\description{
Update latent states.
}
\usage{
updateS(iter, s, V, m, Zb, Zt, Time, MU.state, P, s2, N.upper.tri, random.perturb)
}
\arguments{
  \item{iter}{
  The iteration number
}
  \item{s}{
Previous hidden states.
}
  \item{V}{
  Layer-specific network generation rules.
}
  \item{m}{
Number of break.
}
  \item{Zb}{
  Z - beta.
}
  \item{Zt}{
  Z stacked by time.
}
  \item{Time}{
 The length of time.
}
  \item{MU.state}{
  UVU for each state
}
  \item{P}{
  Transition matrix.
}
  \item{s2}{
  The variance of error.
}
  \item{N.upper.tri}{
  The number of upper triangular elements
}
\item{random.perturb}{
	If \code{random.perturb} = TRUE and a single state observation is found, the latent state
	is randomly selected by equal weights. 
}
}