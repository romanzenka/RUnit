\encoding{latin1}
\name{inspect}
\alias{inspect}
\title{Track the executed code lines of a function or method.}

\usage{
  inspect(expr, track=track)
}

\arguments{
  \item{expr}{Any R function or methods call.}
  \item{track}{list object, as returned by a call to \code{tracker}.}
}

\description{
  \code{inspect} examines and modifies the source code of a function or
  method.  After the modifcation of the source code, the modified function
  will be executed and the result of the tracking process will be
  stored. To store the information a \code{tracker} environment with the
  name track must exist.
  Note, that not all R code constructs can be handled at the current
  state. In some cases it is not possible to track a specific code line.
  Therefore, clearly structured code with consequent use of
  opening and closing braces to indicate conditional expressions can
  prevent these parser problems.
}

\details{The return value of \code{inspect} is the result returned by
the function executed. If the function has no return value nothing is
returned either.}

\author{Thomas \enc{König}{Koenig}, Klaus \enc{Jünemann}{Juenemann} \& Matthias Burger}

\seealso{
  \code{\link{tracker}} for the call tracking object.
}

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
## res will collect the result of calling foo
res <- inspect(foo(10), track=track)

## get the tracked function call info
resTrack <- track$getTrackInfo()

## create HTML sites
printHTML.trackInfo(resTrack)
}

\keyword{programming}
\concept{RUnit}
