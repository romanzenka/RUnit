%%
%% $Id$
%%
\name{tracker}
\alias{tracker}

\title{Tracking the results of the inspect process.}


\usage{tracker()
          tracker$init()
          tracker$getTrackInfo()
}

\description{
  \code{tracker} create a new environment to manage and store the result
  of the tracking process. The \code{inspector} function requires such an
  environment with the name "track". The tracker records how often each
  and every
  function was called by \code{inspect} and summarizes the results of
  all calls.
  \code{tracker$init} initializes the tracker environment.
  \code{tracker$getTrackInfo} returns a list with the tracked results of the
  inspection process.
}

\details{The return object is a list with the following elements.
  \item{src}{The source code of the function.}
  \item{run}{The number of executions for each line of code.}
  \item{graph}{A matrix. Each element in the matrix counts how often a
    code line was called from the previous code line in the execution flow.}
  \item{nrRuns}{Counts how often the function was called.}
  \item{funcCall}{The declaration of the function.}
}

\authors{Thomas K\"onig, Klaus J\"unemann \& Matthias Burger}

\examples{

## example functions
foo <- function(x){
   y <- 0
   for(i in 1:100)
   {
      y <- y + i
   }
   return(y)
}

bar <- function(x){
   y <- 0
   for(i in 1:100)
   {
      y <- y - i
   }
   return(y)
}



## the name track is necessary
track <- tracker()

## initialize the tracker
track$init()

## inspect the function
## resFoo1 is the result of foo(50)
resFoo1 <- inspect(foo(50))

## resFoo2 is the result of foo(20)
resFoo2 <- inspect(foo(20))

## resBar1 is the result of bar(30)
resBar1 <- inspect(bar(30))

## get the tracking info for all inspect call
resTrack <- track$getTrackInfo()

## create HTML sites for all inspect calls
printHTML.trackInfo(resTrack)
}

\keyword{programming}
\keyword{RUnit}
