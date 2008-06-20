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

isValidTestFileResult <- function(object) {
 ## @bdescr
  ## checks validity of TestFileResult object
  ## @edescr
  ##
  ##@in object : [TestFileResult] object of type 'TestFileResult' to be checked
  ##@ret       : [logical | character] TRUE, if object is valid, error message otherwise

  if(!is(object, "TestFileResult")) {
    return("Invalid TestFileResult object: Wrong type")
  }
  if(length(object@testFileName) != 1) {
    return("Invalid TestFileResult object: must have exactly one name")
  }
  if(as.double(object@execStop-object@execStart) < 0) {
    return("Invalid TestFileResult object: negative execution time")
  }
  for(idx in seq_along(object@testFunctionData)) {
    msg <- isValidTestFunctionResult(object@testFunctionData[[idx]])
    if(!isTRUE(msg)) {
      return(msg)
    }
  }
  return(TRUE)
}

## --- Class definition
setClass("TestFileResult",
         representation("RUnitBase", 
                        testFileName="character", # name (with full path) of test file
                        sourceState="character",  # 'OK' if sourcing was successful, otherwise the error message
                        testFunctionData="list",  # List of TestFunctionResult objects
                        execStart="POSIXct",      # Time and date when test file execution started
                        execStop="POSIXct"),      # Time and date when test file execution stopped
         prototype=list(execStart=Sys.time(),
                        execStop=Sys.time()))

## --- Get methods
setMethod("countResult", "TestFileResult",
          function(obj) {
            ## see 00Init.r for documentation
            count <- c(0,0,0,0)
            states <- .getTestFuncStates()
            lapply(obj@testFunctionData, function(tf) {
              mask <- (states == tf@state)
              if(sum(mask) != 1) {
                stop(paste("Test Function", tf@testFunctionName,
                           "has invalid state"))
              }
              count[mask] <<- 1 + count[mask]
            })
            names(count) <- states 
            return(count)
          })

setMethod("testFuncsByState", c("TestFileResult", "character"),
          function(obj, state) {
            ## see 00Init.r for documentation
          if(length(obj@testFunctionData) > 0) {
            mask <- sapply(obj@testFunctionData, function(tf) any(tf@state==state))
            ret <- obj@testFunctionData[mask]
            names(ret) <- rep(obj@testFileName, length(ret))
            return(ret)
          } else {
            return(list())
          }
        })
            




setMethod("toText", c("TestFileResult", "numeric"),
          function(obj, verbosity) {
            ## see 00Init.r for documentation
            indent <- "  "
            if(obj@sourceState != "OK") {
              text <- paste("Test file ", obj@testFileName,
                            ": Error while sourcing the file", sep="")
              text <- append(text, paste(indent, obj@sourceState, sep=""))
            } else {
              execTime <- round(as.double(obj@execStop - obj@execStart), 2)
              text <- paste("Test file ", obj@testFileName, 
                            " (", execTime , " secs):", sep="")
              sapply(obj@testFunctionData, function(x) {
                text <<- append(text, toText(x,verbosity))
              })
            }
            names(text) <- NULL
            return(text)
          })
