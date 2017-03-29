\name{updateUm}
\alias{updateUm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update regime-changing latent node positions. 
}
\description{
Update regime-changing latent node positions.
}
\usage{
updateUm(ns, U, V, R, Zm, Km, ej, s2, eU, iVU, UL.Normal)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ns}{
The number of latent states
}
  \item{U}{
THe latent node positions
}
  \item{V}{
  Layer-specific network generation rule.
}
  \item{R}{
The dimensionality of latent space
}
  \item{Zm}{
  Regim-specific Z - beta
}
  \item{Km}{
The dimension of regime-specific Z.
}
  \item{ej}{
Regime indicator. 
}
  \item{s2}{
The variance of error.
}
  \item{eU}{
The regim-specific mean of U.
}
  \item{iVU}{
The regim-specific variance of U.
}
  \item{UL.Normal}{
Normalization method for U.
}
}
