##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003, 2004  Thomas Koenig, Matthias Burger, Klaus Juenemann
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

##  $Id$


.newTestLogger <- function(useOwnErrorHandler) {
  ##@bdescr
  ## creates a new, empty TestLogger 'object'.
  ## TestLogger is an object based on the 'closure trick'. It has the task
  ## to store, administrate and print the test protocol.
  ##@edescr

  ## private data:
  ## -----------------------
  testData <- list()
  class(testData) <- "RUnitTestData"
  currentTestSuiteName <- NULL
  currentSourceFileName <- NULL
  currentTraceBack <- NULL

  isFailure <- FALSE
  checkNo <- 0

  ## define own error handler
  ## -----------------------
  errorHandler<-function() {
    dump.frames();
    frameLimit<-length(last.dump)-1
    currentTraceBack <<- NULL
    for(i in 1:frameLimit)
      currentTraceBack <<- c(currentTraceBack, names(last.dump)[i])
  }
  if(useOwnErrorHandler) {
    options(error=errorHandler)
  }



  ## public methods:
  ## -----------------------
  getTestData <- function() {
    ##@bdescr
    ## returns the protocol data collected during the test runs
    ##@edescr
    return(testData)
  }

  setCurrentTestSuite <- function(testSuite) {
    ##@bdescr
    ## specify the test suite that is currently executed.
    ##@edescr
    ##@in testSuite : [testSuite - list] the current testSuite

    if(is.null(testSuite)) {
      currentTestSuiteName <<- NULL
    }
    else {
      if(is.element(testSuite$name, names(testData))) {
        stop(paste("Duplicate test suite:", testSuite$name))
      }
      currentTestSuiteName <<- testSuite$name
      testData[[testSuite$name]] <<- list(nTestFunc=0, nErr=0, nFail=0,
                                          dirs=testSuite$dirs,
                                          testFileRegexp=testSuite$testFileRegexp,
                                          testFuncRegexp=testSuite$testFuncRegexp,
                                          sourceFileResults=list())
    }
  }

  setCurrentSourceFile <- function(sourceFileName) {
    ##@bdescr
    ## specify the source file whose test functions are currently executed
    ##@edescr
    ##@in sourceFileName : [character] name of current source file

    if(is.null(sourceFileName)) {
      currentSourceFileName <<- NULL
    }
    else {
      currentSourceFileName <<- sourceFileName
      testData[[currentTestSuiteName]]$sourceFileResults[[sourceFileName]] <<- list()
    }
  }

  addSuccess <- function(testFuncName, secs) {
    ##@bdescr
    ## add a successful test function run.
    ##@edescr
    ##@in testFuncName : [character] name of test function
    ##@in secs : [numeric] time in seconds needed by the test function to complete

    testData[[currentTestSuiteName]]$nTestFunc <- 1 + testData[[currentTestSuiteName]]$nTestFunc

    testData[[currentTestSuiteName]]$sourceFileResults[[currentSourceFileName]][[testFuncName]] <<-
      list(kind="success", time=secs)
  }

  addError <- function(testFuncName, errorMsg) {
    ##@bdescr
    ## add a test function that generated an error.
    ##@edescr
    ##@in testFuncName : [character] name of test function
    ##@in errorMsg : [character] the error message

    testData[[currentTestSuiteName]]$nTestFunc <- 1 + testData[[currentTestSuiteName]]$nTestFunc
    testData[[currentTestSuiteName]]$nErr <- 1 + testData[[currentTestSuiteName]]$nErr

    testData[[currentTestSuiteName]]$sourceFileResults[[currentSourceFileName]][[testFuncName]] <<-
      list(kind="error", msg=errorMsg, traceBack=currentTraceBack)
    currentTraceBack <<- NULL
  }

  addFailure <- function(testFuncName, failureMsg, checkNum) {
    ##@bdescr
    ## add a test function that generated an error.
    ##@edescr
    ##@in testFuncName : [character] name of test function
    ##@in failureMsg : [character] the failure message
    ##@in checkNum: [integer] number of check function that failed.

    testData[[currentTestSuiteName]]$nTestFunc <- 1 + testData[[currentTestSuiteName]]$nTestFunc
    testData[[currentTestSuiteName]]$nFail <- 1 + testData[[currentTestSuiteName]]$nFail

    testData[[currentTestSuiteName]]$sourceFileResults[[currentSourceFileName]][[testFuncName]] <<-
      list(kind="failure", msg=failureMsg, checkNo=checkNum, traceBack=NULL)  ## traceBack is useless in this case
    currentTraceBack <<- NULL
  }

  return(list(getTestData=getTestData,
              setCurrentTestSuite=setCurrentTestSuite,
              setCurrentSourceFile=setCurrentSourceFile,
              addSuccess=function(testFuncName, secs) addSuccess(testFuncName, secs),
              addError=function(testFuncName, errorMsg) addError(testFuncName, errorMsg),
              addFailure=function(testFuncName, failureMsg, checkNum) addFailure(testFuncName, failureMsg, checkNum),
              isFailure=isFailure,
              checkNo=checkNo))
}





## tools to handle the testData listlistlist
.getErrors <- function(testData) {
  if(class(testData) != "RUnitTestData") {
    stop(".getErrors needs an object of class 'RUnitTestData' as argument.")
  }
  ret <- list(nErr=0, nFail=0, nTestFunc=0)
  for(i in seq(length=length(testData))) {
    ret$nErr <- ret$nErr + testData[[i]]$nErr
    ret$nFail <- ret$nFail + testData[[i]]$nFail
    ret$nTestFunc <- ret$nTestFunc + testData[[i]]$nTestFunc
  }
  return(ret)
}

