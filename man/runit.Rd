\name{runTestSuite}
\alias{runTestSuite}
\alias{runTestFile}
\alias{defineTestSuite}
\alias{isValidTestSuite}
\title{Definition and execution of RUnit test suites.}

\usage{
defineTestSuite(name, dirs, testFileRegexp="^runit.+\\.r$", testFuncRegexp="^test.+")
isValidTestSuite(testSuite)
runTestSuite(testSuites, useOwnErrorHandler=TRUE)
runTestFile(absFileName, useOwnErrorHandler=TRUE, testFuncRegexp="^test.+")
}

\arguments{
  \item{name}{The name of the test suite.}
  \item{dirs}{Vector of absolute directory names where to look for test files.}
  \item{testFileRegexp}{Regular expression for test files.}
  \item{testFuncRegexp}{Regular expression for test functions.}
  \item{testSuite}{A single object of class test suite.}
  \item{testSuites}{A single object of class test suite or a list of
    test suite objects.}
  \item{useOwnErrorHandler}{If \code{TRUE} the RUnit framework installs
    its own error handler during test case execution (but reinstalls the
    original handler before it returns). If \code{FALSE} the error
    handler is not touched by RUnit but then the test protocol does not
    contain any call stacks in the case of errors.}
  \item{absFileName}{Absolute file name of a test function.}
}

\description{
  \code{runTestSuite} is the central function of the RUnit package.
  Given one or more test suites it sources all test files and then
  executes all test functions as defined by the test suites. During
  the execution information about the test function calls including the
  possible occurence of failures or errors is stored and returned at the
  end of the test run. The return value can then be used to create a
  test protocol of various formats.

  \code{runTestFile} is just a  convenience function for executing the
  tests in a single test file.

  \code{defineTestSuite} is a helper function to define a test
  suite. See below for a precise definition of a test suite.

  \code{isValidTestSuite} checks if an object defines a valid test suite.
}

\details{
  The basic idea of the RUnit test framework is to declare a certain set
  of functions to be test functions and report the results of their
  execution. The test functions must not take any parameter nor return
  anything important such that their execution can be automatised.

  The specification which functions are taken as test functions is
  contained in an object of class  \code{RUnitTestSuite} which is a list
  with the following elements.
  \item{name}{A simple character string. The name of a test suite is mainly used to create a well
    structure test protocol.}
  \item{dirs}{A character vector containing the absolute names of all
    directories where to look for test files.}
  \item{testFileRegexp}{A regular expression specifying the test
    files. All files in the test directories whose name match this
    regular expression are taken as test files.}
  \item{testFuncRegexp}{A regular expression specifying the test
    functions. All functions defined in the test files whose names match
    this regular expression are used as test functions}

  After the RUnit framework has executed all test suites it returns all
  data collected during the test run as an object of class
  \code{RUnitTestData}. This is a (unfortunately rather deeply nested)
  list with one list element for each executed test suite. Each of these
  executed test suite lists contains the following elements:
  \item{nTestFunc}{The number of test functions executed in the test
    suite.}
  \item{nErr}{The number of errors that occured during the execution.}
  \item{nFail}{The number of failures that occured during the execution.}
  \item{dirs}{The test directories of the test suite.}
  \item{testFileRegexp}{The regular expression for identifying the test
    files of the test suite.}
  \item{testFuncRegexp}{The regular expression for identifying the test
    functions of the test suite.}
  \item{sourceFileResults}{A list containing the results for each
    separate test file of the test suite.}

  The \code{sourceFileResults} list just mentioned contains one element
  for each test function ion the source file. This elemnt is a list with
  the following entries:
  \item{kind}{One of \code{success}, \code{error} or \code{failure}
    describing the outcome of the test function.}
  \item{msg}{the error message in the case of an error or failure and
    \code{NULL} for a successful test function.}
  \item{time}{The duration (measured in seconds) of the successful
    execution of a test function and \code{NULL} in the case of an error
    or failure.}
  \item{traceBack}{The full trace back as a character vector in the case of an error and
    \code{NULL} otherwise.}

  To further control test case execution it is possible to define two
  parameterless function \code{.setUp()} and \code{.tearDown()} in a
  test file. \code{.setUp()} is executed directly before and
  \code{.tearDown()} directly after each test function.

  Quite often, it is useful to base test cases on random numbers. To
  make this procedure reproducible, the function \code{runTestSuite}
  sets the random number generator to the default setting
  \code{RNGkind(kind="Marsaglia-Multicarry",
    normal.kind="Kinderman-Ramage")} before sourcing each test file
  (note that this default has been chosen due to historical reasons and
  differs from the current R default). This default can be overwritten
  by  configuring the random number generator at the beginning of a test
  file. This setting, however, is valid only inside its own source file
  and gets overwritten when the next test file is sourced.
}


\value{
  \code{runTestSuite} and \code{runTestFile} both return an object of
  class RUnitTestData.

  \code{defineTestSuite} returns an object of class  \code{RUnitTestSuite}.
}

\authors{Thomas K\"onig, Klaus J\"unemann \& Matthias Burger}

\seealso{
  \code{\link{checkTrue}} and friends for writing test cases.
  \code{\link{printTextProtocol}} and \code{\link{printHTMLProtocol}} for printing the test protocol.
}


\examples{

## run some test suite
myTestSuite <- defineTestSuite("my test suite", "tests")
testData <- runTestSuite(myTestSuite)

## prints detailed text protocol
## to standard out:
printTextProtocol(testData, showDetails=TRUE)
}

\keyword{programming}
\concept{RUnit}
