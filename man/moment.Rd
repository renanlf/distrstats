\name{moment}
\alias{moment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Moments of a probability distribution
}
\description{
Some functions of useful moments of a probability distribution (raw, incomplete, standardized, kurtosi, etc).
}
\usage{ moment(psi, pdf, par, lower=-Inf,upper=Inf) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{psi}{ A x-only param function }
  \item{pdf}{ Function of the probability distribution which its first param is the parameters of the pdf and its second param is the x (random variable). }
  \item{par}{ pdf's parameters (a vector of) }
  \item{lower}{ Lower bound of pdf. }
  \item{psi}{ Upper bound of pdf. }
}
\value{
  \item{value}{The value of the momentum.}
}
\author{
  Renan L. Fernandes
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
