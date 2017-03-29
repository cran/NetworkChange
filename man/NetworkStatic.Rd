\name{NetworkStatic}
\alias{NetworkStatic}
\title{
A degree-corrected multilinear tensor decomposition 
}
\description{
NetworkStatic implements a degree-corrected multilinear tensor decomposition of network time series  
}
\usage{
NetworkStatic(Y, R=2, mcmc = 100, burnin = 100, verbose = 0,thin = 1, 
	      degree.normal="eigen", UL.Normal = "Orthonormal",
	      plotUU = FALSE, plotZ = FALSE,
	      b0 = 0, B0 = 1, c0 = NULL, d0 = NULL,
	      u0 = NULL, u1 = NULL, v0 = NULL, v1 = NULL,
	      marginal = FALSE, DIC = FALSE,  Waic=FALSE)
}
\arguments{
    \item{Y}{
        Input network data. Unweighted or weighted data are possible. Only symmetric data are accepted. 
    }
    \item{R}{
        The dimensionality of the latent space. The defaul is \code{R=2}.
    }
    \item{burnin}{The number of burn-in iterations for the sampler.}
    
    \item{mcmc}{The number of MCMC iterations after burnin.}
    
    \item{verbose}{If \code{verbose} is greater than 0, seleteced parameters are printed to
                   the screen at every \code{verbose}th iteration.}
    
    \item{thin}{The thinning interval used in the simulation.  The number of
                MCMC iterations must be divisible by this value.}
    
    
    \item{degree.normal}{
	A null model for degree correction. Users can choose "NULL", "eigen" or "Lsym."
	"NULL" is no degree correction. "eigen" is a principal eigen-matrix consisting of
        the first eigenvalue and the corresponding eigenvector. "Lsym" is a modularity matrix. The default is "eigen." 
    }
    \item{UL.Normal}{
	Transformation of sampled U. Users can choose "NULL", "Normal" or "Orthonormal." 
	"NULL" is no normalization. "Normal" is the standard normalization. "Orthonormal" is the Gram-Schmidt orthgonalization.
	Default is "NULL." 
    }
    \item{plotUU}{If \code{plotUU = TRUE} and \code{verbose > 0}, then the plot of the latent space will be
                  printed to the screen at every \code{verbose}th iteration. The default is \code{plotUU = FALSE}.}
    
    \item{plotZ}{If \code{plotZ = TRUE} and \code{verbose > 0}, then the plot of the degree-corrected input matrix will be
                 printed to the screen with the sampled mean values at every \code{verbose}th iteration.
                 The default is \code{plotUU = FALSE}.}    
    \item{b0}{The prior mean of \eqn{\beta}{beta}. This must be a scalar. The default value is 0.}
    
    \item{B0}{The prior variance of \eqn{\beta}{beta}. This must be a scalar.  The default value is 1.}
    
    \item{c0}{\eqn{c_0/2}{c0/2} is the shape parameter for the inverse
              Gamma prior on \eqn{\sigma^2}{sigma^2}. The default value is 1.}
    
    \item{d0}{\eqn{d_0/2}{d0/2} is the scale parameter for the
              inverse Gamma prior on \eqn{\sigma^2}{sigma^2}. The default value is the variance of the pseudo residual.}
    
    \item{u0}{\eqn{u_0/2}{u0/2} is the shape parameter for the inverse
              Gamma prior on variance parameters for U.
	      The default is 10.}
    \item{u1}{\eqn{u_1/2}{u1/2} is the scale parameter for the
              inverse Gamma prior on variance parameters for U.
	      The default is 1.}  
    \item{v0}{\eqn{v_0/2}{v0/2} is the shape parameter for the inverse
              Gamma prior on variance parameters for V.
	      The default is 10.}
    \item{v1}{\eqn{v_1/2}{v1/2} is the scale parameter for the
              inverse Gamma prior on variance parameters for V.
	      The default is the time length of Y.}	
    
    \item{marginal}{If \code{marginal = TRUE}, the log marignal likelihood is computed using the method of Chib (1995).}
    
    \item{DIC}{If \code{DIC = TRUE}, the deviation information criterion is computed.}
    
    \item{Waic}{If \code{Waic = TRUE}, the Watanabe information criterion is computed.}
}
\value{
    An output contains the posterior mean of the decomposed latent array (\code{U*V*t(U)}). 
    The object contains an attribute of a degree-corrected input array \code{Z}, a storage matrix for U (\code{Umat}),
    a storage matrix for V (\code{Vmat}), a storage matrix for \eqn{\beta}{beta} (\code{bmat}),
    a storage matrix for \eqn{\sigma^2}{sigma^2}(\code{s2mat}), the log likelihood of the model (\code{loglike}),
    the log marginal likelihood of the model (\code{logmarglike}), the DIC of the model (\code{DIC}),
    and the output vector of the waic (\code{waic.out}).	
}
\references{
    Jong Hee Park and Yunkyun Sohn. 2017. "Detecting Structural Change
   in Network Time Series Data using Bayesian Inference." Working Paper.
    
    Peter D. Hoff 2011. "Hierarchical Multilinear Models for Multiway Data."
    \emph{Computational Statistics \& Data Analysis}. 55: 530-543.
    
    Siddhartha Chib. 1998. "Estimation and comparison of multiple change-point models."
    \emph{Journal of Econometrics}. 86: 221-241.
    
    Sumio Watanabe. 2010. "Asymptotic equivalence of Bayes cross validation and widely 
   applicable information criterion in singular learning theory."
    \emph{Journal of Machine Learning Research}. 11: 3571-3594.
}
\author{
    Jong Hee Park and Yunkyu Sohn
}
\seealso{
    See Also as \code{\link{NetworkChange}}.
}
\examples{
\dontrun{
set.seed(1973)

## generate an array with two constant blocks
Y <- MakeBlockNetworkChange(n=10, shape=10, T=40, type ="constant")
out0 <- NetworkStatic(Y, R=2, mcmc=10, burnin=10,
        verbose=10, UL.Normal = "Orthonormal")

## contour plot of latent node positions
plotContour(out0)

## plot latent node positions
plotU(out0)

## plot layer-specific network connection rules
plotV(out0)
}
}
\keyword{NetworkChange}
