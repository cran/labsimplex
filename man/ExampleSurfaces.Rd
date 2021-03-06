% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exampleSurfaces.R
\name{ExampleSurfaces}
\alias{ExampleSurfaces}
\alias{exampleSurfaceR2}
\alias{exampleSurfaceR2.2pks}
\alias{exampleSurfaceR3}
\title{Multivariate functions that define hypothetical response surfaces.}
\usage{
exampleSurfaceR2(x1, x2, noise = 0)

exampleSurfaceR2.2pks(x1, x2, noise = 0)

exampleSurfaceR3(x1, x2, x3, noise = 0)
}
\arguments{
\item{x1}{temperature in Kelvin. Numeric between 278 and 365.}

\item{x2}{pH. Numeric between 0 and 14.}

\item{noise}{absolute noise included in the response surface result.
Default to zero.}

\item{x3}{concentration in arbitrary units. Numeric between 0 and 1.
Only used in \code{exampleSurfaceR3()}.}
}
\value{
\code{exampleSurfaceR2(x1, x2, noise = 0)} defines a response
  surface with one maximum at pH 10 and 300 K.

\code{exampleSurfaceR2.2pks(x1, x2, noise = 0)} defines a response
  surface with global and local maxima at pH 10 and 300 K and pH 4.5 and
  340 K, respectively.

\code{exampleSurfaceR3(x1, x2, x3, noise = 0)} defines a response
  surface with one maximum at pH 10, 300 K and a concentration of 0.5.
}
\description{
The functions in this section simulate the yield of hypothetical chemical
reactions as a function of temperature, pH, and concentration (the latter
only for \code{exampleSurfaceR3()}). Those functions are useful to
illustrate most concepts of the simplex optimization algorithms
implemented in the \code{labsimplex} package, as shown in the vignentte of
the package. This vignette can be visualized by running
\code{vignette('labsimplex')}.
}
\details{
Parameters \code{x1}, \code{x2}, and \code{x3} may be supplied as
vectors in which case all must have the same length. \cr
Boundary values are proposed consistently with real-life limitations
in aqueous media. If such boundaries are violated in the variables
input, a negative result without physical meaning is returned. This
negative value represents an \emph{infinitely bad response} that will
force the simplex to move in another direction.
}
\examples{
  exampleSurfaceR2(x1 = 320, x2 = 4.5)
  exampleSurfaceR2(x1 = c(310, 320), x2 = c(4.5, 5.8))
  exampleSurfaceR2(x1 = c(310, 320), x2 = c(4.5, 5.8), noise = 5)
  exampleSurfaceR2.2pks(x1 = 320, x2 = 4.5)
  exampleSurfaceR2.2pks(x1 = c(310, 320), x2 = c(4.5, 5.8))
  exampleSurfaceR2.2pks(x1 = c(310, 320), x2 = c(4.5, 5.8), noise = 5)
  exampleSurfaceR3(x1 = 320, x2 = 4.5, x3 = 0.3)
  exampleSurfaceR3(x1 = c(310, 320), x2 = c(4.5, 5.8), x3 = c(0.3, 0.5))
  exampleSurfaceR3(x1 = c(310, 320), x2 = c(4.5, 5.8), x3 = c(0.3, 0.5),
                   noise = 5)
}
\seealso{
\code{\link{cntr}}, \code{\link{prspctv}} and
\code{\link{exampleOptimization}}
}
\author{
Cristhian Paredes, \email{craparedesca@unal.edu.co}

Jesús Ágreda, \email{jagreda@unal.edu.co}
}
