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

isValidTestSuiteResult <- function(object) {
 ## @bdescr
  ## checks validity of TestSuiteResult object
  ## @edescr
  ##@in object : [TestSuiteResult] object of type 'TestSuiteResult' to be checked
  ##@ret       : [logical | character] TRUE, if object is valid, error message otherwise
  
  if(!is(object, "TestSuiteResult")) {
    return("Invalid TestSuiteResult object: Wrong type")
  }
  msg <- isValidTestSuite(object@testSuite) 
  if(!isTRUE(msg)) {
    return(msg)
  }
  if(as.double(object@execStop-object@execStart) < 0) {
    return("Invalid TestSuiteResult object: negative execution time")
  }
  for(idx in seq_along(object@testFileData)) {
    msg <- isValidTestFileResult(object@testFileData[[idx]])
    if(!isTRUE(msg)) {
      return(msg)
    }
  }
  return(TRUE)
}

## --- Class definition
setClass("TestSuiteResult",
         representation("RUnitBase", 
                        testSuite="TestSuite", # Definition of test suite
                        testFileData="list",   # List of TestFileResult objects
                        execStart="POSIXct",   # Time and date when test suite execution started
                        execStop="POSIXct"),   # Time and date when test suite execution stopped
         prototype=list(execStart=Sys.time(),
                        execStop=Sys.time()))




## --- get methods
setMethod("countResult", "TestSuiteResult",
          function(obj) {
            ## see 00Init.r for documentation
            count <- c(0,0,0,0)
            names(count) <- .getTestFuncStates()
            lapply(obj@testFileData, function(x) count <<- count + countResult(x))
            return(count)
          })

setMethod("testFuncsByState", c("TestSuiteResult", "character"),
          function(obj, state) {
             ## see 00Init.r for documentation
            ret <- list()
            lapply(obj@testFileData, function(tf) 
                   ret <<- append(ret, testFuncsByState(tf, state)))
            return(ret)
          })
            

setMethod("sourceErrors", c("TestSuiteResult"),
          function(obj) {
            ## see 00Init.r for documentation
            if(length(obj@testFileData) > 0) {
              mask <- sapply(obj@testFileData, function(tf) tf@sourceState != "OK")
              ret <- obj@testFileData[mask]
              names(ret) <- rep(obj@testSuite@name, length(ret))
              return(ret)
            }
            else {
              return(list())
            }
          })
          


## --- Display methods
setMethod("toText", c("TestSuiteResult", "numeric"),
          function(obj, verbosity) {
            ## see 00Init.r for documentation
            text <- toText(obj@testSuite, verbosity)
            count <- countResult(obj)
            
            if(verbosity<=0) {   # don't list all test file names in this case
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
              ## write error numbers into first line and list of all files in this case
              text[1] <- paste(text[1], ": ", .sop(count["ERROR"], "error"), ", ",
                               .sop(count["FAILURE"], "failure"), sep="")
              if(count["DEACTIVATED"] > 0) 
                text[1] <- paste(text[1], ", ", count["DEACTIVATED"], " deactivated", sep="")
              sapply(obj@testFileData, function(x) {
                text <<- append(text, toText(x, verbosity))
              })
            }
            names(text) <- NULL
            return(text)
          })
