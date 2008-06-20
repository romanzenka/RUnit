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

isValidTestFunctionResult <- function(object) {
 ## @bdescr
  ## checks validity of TestFunctionResult object
  ## @edescr
  ##
  ##@in object : [TestFunctionResult] object of type 'TestFunctionResult' to be checked
  ##@ret       : [logical | character] TRUE, if object is valid, error message otherwise

  if(!is(object, "TestFunctionResult")) {
    return("Invalid TestFunctionResult object: Wrong type")
  }
  if(length(object@testFunctionName) != 1) {
    return("Invalid TestFunctionResult object: must have exactly one name")
  }
  if(length(object@state) != 1) {
    return("Invalid TestFunctionResult object: must have exactly one state")
  }
  possibleState <- .getTestFuncStates()
  if(sum(object@state==possibleState) != 1) {
    return("Invalid TestFunctionResult object: state slot has an invalid value")
  }
  if( !(length(object@numChecks == 1) && object@numChecks >= 0)) {
    return("Invalid TestFunctionResult object: numChecks must be a single non-negative number")
  }
  if( !(length(object@numWarns == 1) && object@numWarns >= 0)) {
    return("Invalid TestFunctionResult object: numWarns must be a single non-negative number")
  }
  if( !(length(object@execTime == 1) && object@execTime >= 0)) {
    return("Invalid TestFunctionResult object: execTime must be a single non-negative number")
  }
  return(TRUE)
}



## --- Class definition
setClass("TestFunctionResult",
         representation("RUnitBase", 
                        testFunctionName="character",       # name of test function
                        state="character",      # one of success, failure, deactivation, error 
                        msg="character",        # message in case of failure, deactivation or error
                        numChecks="numeric",    # number of successful check function calls so far
                        numWarns="numeric",     # number of warnings that occured in the test function
                        traceBack="character",  # traceback in case of an error, failed check call in case of failure
                        execTime="numeric"),    # Duration of test function execution (seconds)
         prototype=list(numChecks=0,
           numWarns=0,
           execTime=0))

## --- Get methods
.getTestFuncStates <- function() {
  #@bdescr
  # returns a character vector containing the 4 states a test function can have
  #@edescr
  #@ret:   [character] test function state vector

  return(c("OK", "FAILURE", "ERROR", "DEACTIVATED"))
}

## --- Display methods


setMethod("toText", c("TestFunctionResult", "numeric"),
          function(obj, verbosity) {
            ## see 00Init.r for documentation

            ## Basic output for all verbosity levels:
            text <- paste(obj@testFunctionName, ": ", obj@state, sep="")
            
            # More verbose output if required
            if(verbosity > 1) {  
              
              indent <- "  "
              if(identical(obj@state,"OK")) {
                text <- paste(text, " (", round(obj@execTime,2) , " secs)", sep="")
              } else {
                
                if(obj@numWarns > 0) { # print number of warnings
                  text <- append(text, paste(indent, .sop(obj@numWarns, "warning"),
                                             " occurred!", sep=""))
                }
                if(identical(obj@state,"FAILURE")) { # print reason for failure
                  tmp <- paste(indent, "Check number ", obj@numChecks," '",
                               obj@traceBack, "'", " failed", sep="")
                  if( (length(obj@msg) > 0) && (nchar(obj@msg) > 0)) {
                    tmp <- paste(tmp, " with message: '", obj@msg, "'", sep="")
                  }
                  text <- append(text, tmp)
                } else if(identical(obj@state, "ERROR")) { # print error msg and traceback
                  text <- append(text, paste(indent, "Error message: '",
                                             obj@msg, "'", sep=""))
                  if(length(obj@traceBack) > 0) {
                    text <- append(text, paste(indent, "Traceback:", sep=""))
                    sapply(obj@traceBack, function(line)
                           text <<- append(text, paste(indent, indent, line, 
                                                       sep="")))
                  }
                } else if(identical(obj@state, "DEACTIVATED")) {  # print deact msg if available
                  if(length(obj@msg) > 0) {
                    text[1] <- paste(text[1], obj@msg)
                  }
                } else {
                  stop(paste("RUnit error: unknown test function state", obj@state))
                }
              }
            }
            names(text) <- NULL
            return(text)
          })
