\name{checkFuncs}
\alias{checkEquals}
\alias{checkEqualsNumeric}
\alias{checkTrue}
\alias{checkException}
\alias{DEACTIVATED}
\title{RUnit check functions}

\usage{
checkEquals(a, b, msg, tolerance = .Machine$double.eps^0.5, ...)
checkEqualsNumeric(a, b, msg, tolerance = .Machine$double.eps^0.5, ...)
checkTrue(expr, msg)
checkException(expr, msg)
DEACTIVATED(msg)
}

\arguments{
  \item{a, b}{two objects to be compared (should not be S4 class objects).}
  \item{msg}{an optional message to document a check and to facilitate
    the identification of a possible failure. The message only appears
    as text in the test protocol, it is not further used in any of the check functions.}
  \item{tolerance}{numeric >= 0.  A check does not fail if differences smaller than `tolerance'.}
  \item{expr}{syntactically valid R expression which can be evaluated
  and must return a logical scalar (TRUE|FALSE). A named expression is
  also allowed but the name is disregarded.}
  \item{...}{optional arguments passed to \code{all.equal} or \code{all.equal.numeric}}
}

\description{A set of functions used to check the results of some test
  calculation. If these functions are called within the RUnit framework,
  the results of the checks are stored and reported in the test
  protocol.

  \code{checkEquals} compares two R objects by invoking \code{all.equal} on
  the two objects. If the objects are not equal an error is
  generated and the failure is reported to the test logger such that it
  appears in the test protocol.

  \code{checkEqualsNumeric} works just like \code{checkEquals} except
  that it invokes \code{all.equal.numeric} instead of \code{all.equal}

  \code{checkTrue} uses the function \code{identical} to check if the expression
  provided as first argument evaluates to \code{TRUE}. If not, an error is
  generated and the failure is reported to the test logger such that it
  appears in the test protocol.

  \code{checkException} evaluates the passed expression and uses the
  \code{try} mechanism to check if the evaluation generates an error.
  If it does the test is OK. Otherwise an error is
  generated and the failure is reported to the test logger such that it
  appears in the test protocol.

  \code{DEACTIVATED} interrupts the test function and reports the test case
  as deactivated. In the test protocol deactivated test functions are
  listed separately. Test case deactivation can be useful in the case of
  major refactoring. Alternatively, test cases can be commented out
  completely but then it is easy to forget the test case altogether.
}

\details{
  The check functions are direct equivalents of the various
  methods of the class junit.framework.Assert of Javas junit framework
  which served as basis for the RUnit package.
}



\author{Thomas K\"onig, Klaus J\"unemann \& Matthias Burger}

\seealso{
  \code{\link{all.equal}}, \code{\link{all.equal.numeric}} and
  \code{\link{identical}} are the underlying comparison functions.
  \code{\link{try}} is used for error catching.
}


\examples{

checkTrue(1 < 2, "check1")     ## passes fine
## checkTrue(1 > 2, "check2")  ## appears as failure in the test protocol

v <- 1:3
w <- 1:3
checkEquals(v,w)          ## pasess fine
names(v) <- c("A", "B", "C")
## checkEquals(v,w)      ## fails because v and w have different names
checkEqualsNumeric(v,w)  ## passes fine because names are ignored


fun <- function(x) {
  if(x)
  {
   stop("stop conditions signaled")
  }
  return()
}

checkException(fun(TRUE))   ## passes fine
## checkException(fun(FALSE))  ## failure, because f raises no error

## DEACTIVATED("here one can document on the reason for deactivation")
}

\keyword{programming}
\concept{RUnit}
