\name{updatebm}
\alias{updatebm}

\title{
Update regime-changing beta
}
\description{
Update regime-changing beta
}
\usage{
updatebm(ns, K, s, s2, B0, p, ZU)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ns}{
  The number of hidden states
}
  \item{K}{
  The dimensionality of Z
}
  \item{s}{
  Latent state vector.
}
  \item{s2}{
  The variance of error.
}
  \item{B0}{
  The prior variance of beta.
}
  \item{p}{
  The rank of X.
}
  \item{ZU}{
   Z - ULU
}
}
