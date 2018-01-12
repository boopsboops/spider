\name{titv}
\alias{titv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Number of pairwise transitions and transversions in an alignment.
}
\description{
Calculates the number of pairwise transitions and transversions between sequences.
}
\usage{
titv(DNAbin)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DNAbin}{
A DNA alignment of class `DNAbin'.
}
}

\value{
A square matrix with dimensions of \code{length(dat)}. The upper triangle contains the number of transversions. The lower triangle contains the number of transitions.
}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(dolomedes)

subs <- titv(dolomedes)

#Transversions
subs[upper.tri(subs)]
tv <- t(subs)
tv <- tv[lower.tri(tv)]

#Transitions
ti <- subs[lower.tri(subs)]


#Saturation plot
doloDist <- dist.dna(dolomedes)
plot(doloDist, ti, type="p", pch=19, col="blue", 
    main="Saturation plot of number of transitions and transversions\n
    against K2P distance. Red: transversions. Blue: transitions")
points(doloDist, tv, pch=19, col="red")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Utilities}
