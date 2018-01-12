\name{cgraph}
\alias{cgraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Complete graph}

\description{
Creates a complete graph for the given cloud of vertices.
}

\usage{cgraph(x, y, ...)}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{x}{X values, or a matrix with two columns containing X and Y values.}
  \item{y}{Y values. Can be left empty if \code{x} is a matrix.}
  \item{...}{Other arguments to be passed to \code{\link{segments}}.}
}

\details{
If \code{y} is not given, \code{x} is required to be a matrix containing both x and y values.
}

\value{Plots a complete graph between the given vertices.}

\author{Samuel Brown <s_d_j_brown@hotmail.com>}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot.ordinDNA}}.
}

\examples{
x <- runif(15)
y <- runif(15)

plot(x, y)
cgraph(x, y)

M <- cbind(x, y)
cgraph(M[1:10,], col = "blue")

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Visualisation}
