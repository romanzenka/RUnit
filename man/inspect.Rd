\name{inspect}
\alias{inspect}
\title{Track the executed code lines of a method.}

\usage{
  inspect(...)
}

\arguments{
  \item{...}{A usual R function call.}
}

\description{
  \code{inspect} examines and modifies the source code of a function or
  method. Not all R expression can be handled at the current
  state. After the modifcation of the source code, the modified function
  will be executed and the result of the tracking process will be
  stored. To store the information a \code{tracker} environment with the
  name track must exist.
}

\details{The return value of \code{inspect} is the result of the
  function. In some cases it is not possible to track every code line.
}

\authors{Thomas K\"onig, Klaus J\"unemann \& Matthias Burger}

\examples{

## example function
foo <- function(x){
   y <- 0
   for(i in 1:100)
   {
      y <- y + i
   }
   return(y)
}

## the name track is necessary
track <- tracker()

## initialize the tracker
track$init()

## inspect the function
## res is the result of foo
res <- inspect(foo(10))

## get the tracking info
resTrack <- track$getTrackInfo()

## create HTML sites
printHTML.trackInfo(resTrack)
}

\keyword{programming}
\keyword{RUnit}
