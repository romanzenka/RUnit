\name{printHTML.trackinfo}
\alias{printHTML.trackinfo}
\title{Write HTML pages of the tracking result.}

\usage{
  printHTML.trackinfo(res,baseDir=".")
}

\arguments{
  \item{res}{The result of the function tracking.}
  \item{baseDir}{Base directory for the HTML pages.}
}

\description{
  \code{printHTML.trackinfo} creates a directory named
  "result"  in the base directory. All HTML pages and images will be put
  in that directory.
}

\details{An "index.html" page will be created in the directory
  "result" and is the root of the HTML pages. The result for every
  function consists of two HTML pages. The first page is an overview how
  often every line of code was executed. Not executed lines are red,
  executed lines are green. The second page is a representation of the
  execution flow of the function.  
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
