\name{printHTML.trackInfo}
\alias{printHTML.trackInfo}
\title{Write HTML pages of the tracking result.}

\usage{
  printHTML.trackInfo(trackInfo, baseDir=".")
}

\arguments{
  \item{trackInfo}{A list, containing the result of the function \code{tracker}.}
  \item{baseDir}{A character string, specifying the base directory for
  the HTML pages to be written to. Defaults to the current working directory.}
}

\description{
  \code{printHTML.trackInfo} creates a subdirectory named
  "result"  in the base directory specified via \code{baseDir}. All HTML pages and images will be put
  in that directory.
}

\details{An "index.html" page will be created in the directory
  "result" and which is the root entry page of the HTML pages. The
  displayed result for every function consists of two HTML pages.
  The first page is an overview on how
  often every line of code was executed. Code lines not executed are
  highlighted red, executed lines are shown in green. The second page is
  a graph representation of the
  execution flow of the function. Each code line has a edge pointing to
  the next code line that is executed subsequently. Thus loops and jumps
  become clearly visible.
}

\author{Thomas K\"onig, Klaus J\"unemann \& Matthias Burger}

\examples{

##  example function
foo <- function(x){
   y <- 0
   for(i in 1:100)
   {
      y <- y + i
   }
   return(y)
}

##  the name track is necessary
track <- tracker()

##  initialize the tracker
track$init()

##  inspect the function
##  res is the result of foo
res <- inspect(foo(10))

##  get the tracking info
resTrack <- track$getTrackInfo()

##  create HTML pages
printHTML.trackInfo(resTrack)
}

\keyword{programming}
\concept{RUnit}
