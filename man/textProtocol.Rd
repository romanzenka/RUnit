%%  RUnit : A unit test framework for the R programming language
%%  Copyright (C) 2003-2007  Thomas Koenig, Matthias Burger, Klaus Juenemann
%%
%%  This program is free software; you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation; either version 2 of the License, or
%%  (at your option) any later version.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with this program; if not, write to the Free Software
%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

%%  $Id$

\encoding{latin1}
\name{textProtocol}
\alias{printTextProtocol}
\alias{printHTMLProtocol}
\alias{print.RUnitTestData}
\alias{summary.RUnitTestData}
\alias{getErrors}

\title{Printing a plain text or HTML version of an RUnit test run.}

\usage{
  printTextProtocol(testData, fileName = "", separateFailureList = TRUE, showDetails = TRUE, traceBackCutOff=9)
  printHTMLProtocol(testData, fileName = "", separateFailureList = TRUE, traceBackCutOff=9, testFileToLinkMap=function(x) x )
  print.RUnitTestData(x, ...)
  summary.RUnitTestData(object, ...)
  getErrors(testData)
}

\arguments{
  \item{testData, x, object}{objects of class \code{RUnitTestData}, typically
    obtained  as return value of a test run.}
  \item{fileName}{Connection where to print the text protocol (printing
    is done by the \code{cat} command).}
  \item{separateFailureList}{If \code{TRUE} a separate list of failures
    and errors is produced at the top of the protocol. Otherwise, the
    failures and errors are only listed in the details section.}
  \item{showDetails}{If \code{TRUE} the protocol contains a detailed listing of
    all exectuted test functions.}
  \item{traceBackCutOff}{The details section of the test protocol contains the call stack for
    all errors. The first few entries of the complete stack typically
    contain the internal \pkg{RUnit} function calls that execute the test cases and
    are irrelevant for debugging. This argument specifies how many calls
    are removed from the stack before it is written to the protocol. The
    default value is chosen such that all uninteresting \pkg{RUnit} calls are
    removed from the stack if \code{runTestSuite} has been called from
    the console. This argument takes effect only if
    \code{showDetails=TRUE}.}
  \item{testFileToLinkMap}{This function can be used to map the full name of the test file
    to a corresponding html link to be used in the html protocol. By default, this is the identity map.}
  \item{...}{additional arguments to summary are passed on to the
  printTextProtocol() call.}
}

\description{
  \code{printTextProtocol} prints a plain text protocol of a test run.
  The resulting test protocol can be configured through the function
  arguments.

  \code{printHTMLProtocol} prints an HTML protocol of a test run.
  For long outputs this version of the test protocol is slightly more
  readable than the plain text version due to links in the document.
  The resulting test protocol can be configured through the function arguments.

  \code{print} prints the number of executed test functions and the
  number of failures and errors.

  \code{summary} directly delegates the work to
  \code{printTextProtocol}.

  \code{getErrors} returns a list containing the number of test
  functions, the number of deactivated functions (if there are any), the
  number of errors and the number of failures.
}

\details{
  The text protocol can roughly be divided into three sections with an
  increasing amount of information. The first section just reports the
  number of executed test functions and the number of failures and
  errors. The second section describes all test suits. Optionally, all
  errors and failures that occured in some test suite are listed.
  In the optional third section details are given about all executed
  test functions and in the case of an error or failure as much debug
  information as possible is provided.
}

\author{Thomas \enc{König}{Koenig}, Klaus \enc{Jünemann}{Juenemann} \& Matthias Burger}

\seealso{
  \code{\link{runTestSuite}}
}


\examples{

## run some test suite
myTestSuite <- defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), testFileRegexp="correctTestCase.r")
testResult <- runTestSuite(myTestSuite)


## prints detailed text protocol
## to standard out:
printTextProtocol(testResult, showDetails=TRUE)
## prints detailed html protocol
## to standard out
printHTMLProtocol(testResult)

}

\keyword{programming}
\concept{RUnit}
