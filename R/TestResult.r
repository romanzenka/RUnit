##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003-2008  Thomas Koenig, Matthias Burger, Klaus Juenemann
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


## --- Validation method
isValidTestResult <- function(object) {
 ## @bdescr
  ## checks validity of TestResult object
  ## @edescr
  ##
  ##@in object : [TestResult] object of type 'TestResult' to be checked
  ##@ret       : [logical | character] TRUE, if object is valid, error message otherwise
  
  if(!is(object, "TestResult")) {
    return("Invalid TestResult object: Wrong type")
  }
  
  for(idx in seq_along(object@testSuiteData)) {
    msg <- isValidTestSuiteResult(object@testSuiteData[[idx]])
    if(!isTRUE(msg)) {
      return(msg)
    }
  }

  if(!identical(sort(names(object@systemInfo)), c("sessionInfo", "sysInfo"))) {
    return("Invalid TestResult object: Incomplete systemInfo slot")
  }
  if(!is(object@systemInfo$sessionInfo, "sessionInfo")) {
    return("Invalid TestResult object: systemInfo slot must contain element of type sessionInfo")
  }
  if(as.double(object@execStop-object@execStart) < 0) {
    return("Invalid TestResult object: negative execution time")
  }
  
  return(TRUE)
}


## --- Class definition

setClass("TestResult",
         representation("RUnitBase", 
                        testSuiteData="list",  # List of TestSuiteResult objects
                        systemInfo="list",     # System info(s) where tests are run
                        execStart="POSIXct",   # Time and date when test execution started
                        execStop="POSIXct"),   # Time and date when test execution stopped
         prototype=list(systemInfo=list(sysInfo=Sys.info(), sessionInfo=sessionInfo()),
           execStart=Sys.time(),
           execStop=Sys.time()))


## --- Get methods
setMethod("countResult", "TestResult",
          function(obj) {
            ## see 00Init.r for documentation
            count <- c(0,0,0,0)
            names(count) <- .getTestFuncStates()
            lapply(obj@testSuiteData, function(x) count <<- count + countResult(x))
            return(count)
          })



setMethod("testFuncsByState", c("TestResult", "character"),
          function(obj, state) {
            ## see 00Init.r for documentation
            ret <- list()
            lapply(obj@testSuiteData, function(ts) 
                   ret <<- append(ret, testFuncsByState(ts, state)))
            return(ret)
          })
            

setMethod("sourceErrors", c("TestResult"),
          function(obj) {
            ## see 00Init.r for documentation

            ret <- list()
            lapply(obj@testSuiteData, function(ts) 
                   ret <<- append(ret, sourceErrors(ts)))
            return(ret)
          })


## --- Display methods
setMethod("toText", c("TestResult", "numeric"),
          function(obj, verbosity) {
            ## see 00Init.r for documentation
            len <- length(obj@testSuiteData)
            if(len == 0) {
              return("Empty RUnit test result object")
            }
            else if(len == 1) {
              return(toText(obj@testSuiteData[[1]], verbosity))
            }

            ## More than 1 test suite
            text <- paste("RUnit Test Result for Test Suites",
                          paste(sapply(obj@testSuiteData, function(x)
                                       paste("'", x@testSuite@name, "'", sep="")),
                                collapse=", "))
            if(verbosity<=0) {   # don't list all test file names in this case
              count <- countResult(obj)
              text <- append(text, paste("Number of test functions:", sum(count)))
              if(count["DEACTIVATED"] > 0) {
                text <- append(text, paste("Number of deactivated test functions:",
                                           count["DEACTIVATED"]))
              }
              text <- append(text, paste("Number of errors:", count["ERROR"]))
              text <- append(text, paste("Number of failures:", count["FAILURE"]))
              srcErrs <- sourceErrors(obj)
              if(length(srcErrs) > 0) {
                text <- append(text, paste(.sop(length(srcErrs), "file"),
                                           "failed to source!"))
              }
            } else {
              sapply(obj@testSuiteData, function(x)
                     text <<- append(text,
                                     c("---", toText(x, verbosity))))
            }
            names(text) <- NULL
            return(text)
          })


getErrors <- function(obj) {
  ## provided for backwards compatibility
  
  flag <- isValidTestResult(obj)
  if(!isTRUE(flag)) {
    stop(flag)
  }
  
  count <- countResult(obj)
  ret <- list(nErr=0, nDeactivated=0, nFail=0, nTestFunc=0)
  
  ret$nErr <- as.numeric(count["ERROR"])
  ret$nDeactivated <- as.numeric(count["DEACTIVATED"])
  ret$nFail <- as.numeric(count["FAILURE"])
  ret$nTestFunc <- as.numeric(sum(count))
  return(ret)
}
