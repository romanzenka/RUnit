\name{tracker}
\alias{tracker}

\title{Tracking the results of the inspect process.}


\usage{track <- tracker()
       track$init()
       track$getTrackInfo()
}

\description{
  The current implementation uses the 'closure trick' to hide all details from
  the user and only allows to retrieve the results of the code inspection.
  \code{tracker} is used to create a new environment to manage and store the results
  of the tracking process. The \code{inspect} function requires such an
  environment with the name "track" (currently mandatory). The tracker records how often each
  and every function was called by \code{inspect} and summarizes the results of
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

\author{Thomas K\"onig, Klaus J\"unemann \& Matthias Burger}

\seealso{
  \code{\link{inspect}} for the registration of functions \& methods to
  be on the tracking list.
}

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



##  the object name track is 'fixed' (current implementation)
track <- tracker()

##  initialize the tracker
track$init()

##  inspect the function
##  resFoo1 will contain the result of calling foo(50)
resFoo1 <- inspect(foo(50))

resFoo2 <- inspect(foo(20))

resBar1 <- inspect(bar(30))

##  get the tracked function call info for all inspect calls
resTrack <- track$getTrackInfo()

##  create HTML sites in folder ./results for all inspect calls
printHTML.trackInfo(resTrack)
}

\keyword{programming}
\concept{RUnit}
