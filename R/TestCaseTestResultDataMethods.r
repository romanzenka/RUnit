##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2006-2007 Matthias Burger, Thomas Koenig, Klaus Juenemann
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



## ----------------------------------------
##
##  1) constructor methods
##
## ----------------------------------------
newTestCaseTestResultData <- function(...) {
  ##@bdescr
  ##  standard construction function
  ##@edescr
  ##
  ##@class    : [TestCaseTestResultData]
  ##
  ##@in  ...  : [ANY] used for dispatch to constructor method
  ##@ret      : [TestCaseTestResultData] new class object
  ##
  ##@codestatus : untested

  this <- new("TestCaseTestResultData", ...)

  ##  postcondition
  ASSERT(verifyObject(this),
         paste("validation of constructed '",is(this)[1],"' class object failed:\n",
               .GLOBAL$getLastErrorMsg(), sep=""))
  return(this)
}


.initialize.TestCaseTestResultData <- function(.Object, ...) {

  ##@bdescr
  ##  standard constructor method
  ##@edescr
  ##
  ##@class    : [TestCaseTestResultData]
  ##
  ##@in  .Object : [TestCaseTestResultData] class object
  ##@ret         : [TestCaseTestResultData] new class object
  ##
  ##@codestatus : untested

  return(.constructTestCaseTestResultData(.Object, ...))
}


.constructTestCaseTestResultData.empty <- function(obj) {
  ##@bdescr
  ##  construct a TestCaseTestResultData class object
  ##@edescr
  ##
  ##@class  : [TestCaseTestResultData]
  ##
  ##@in obj   : [TestCaseTestResultData]
  ##@ret      : [TestCaseTestResultData]
  ##
  ##@codestatus : internal

  return(obj)
}


.constructTestCaseTestResultData.result <- function(obj, para1, ...) {
  ##@bdescr
  ##  construct a TestCaseTestResultData class object
  ##@edescr
  ##
  ##@class  : [TestCaseTestResultData]
  ##
  ##@in  obj   : [TestCaseTestResultData]
  ##@in  para1 : [character] test function name
  ##@in  ...   : [ANY]
  ##@ret       : [TestCaseTestResultData]
  ##
  ##@codestatus : internal

  ASSERT( length(para1) == 1, "argument functionName has to be of length 1.")
  obj@functionName <- para1

  ##  infere other parameters
  args <- list(...)
  if (length(args) > 0) {
    if (length(args) < 2) {
      setFatalError("too few arguments provided.")
    }
    if (!all(names(args) %in% c("sourceFileName", "deactivated", "failure",
                                "error", "setUpError", "tearDownError",
                                "errorMsg", "execTime"))) {
      setFatalError("too many arguments supplied.")
    }
  }
  
  return(obj)
}



## ----------------------------------------
##
##  2) accessor methods
##
## ----------------------------------------
getFunctionName.TestCaseTestResultData <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestCaseTestResultData]
  ##
  ##@in   obj : [TestCaseTestResultData]
  ##@ret      : [character]
  ##
  ##@codestatus : untested
  
  
  return(obj@functionName)
}


getSourceFileName.TestCaseTestResultData <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestCaseTestResultData]
  ##
  ##@in   obj : [TestCaseTestResultData]
  ##@ret      : [character]
  ##
  ##@codestatus : untested
  
  
  return(obj@sourceFileName)
}


getDeactivated.TestCaseTestResultData <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestCaseTestResultData]
  ##
  ##@in   obj : [TestCaseTestResultData]
  ##@ret      : [logical]
  ##
  ##@codestatus : untested
  
  
  return(obj@deactivated)
}


getFailure.TestCaseTestResultData <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestCaseTestResultData]
  ##
  ##@in   obj : [TestCaseTestResultData]
  ##@ret      : [logical]
  ##
  ##@codestatus : untested
  
  
  return(obj@failure)
}


getError.TestCaseTestResultData <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestCaseTestResultData]
  ##
  ##@in   obj : [TestCaseTestResultData]
  ##@ret      : [logical]
  ##
  ##@codestatus : untested
  
  
  return(obj@error)
}


getSetUpError.TestCaseTestResultData <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestCaseTestResultData]
  ##
  ##@in   obj : [TestCaseTestResultData]
  ##@ret      : [logical]
  ##
  ##@codestatus : untested
  
  
  return(obj@setUpError)
}


getTearDownError.TestCaseTestResultData <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestCaseTestResultData]
  ##
  ##@in   obj : [TestCaseTestResultData]
  ##@ret      : [logical]
  ##
  ##@codestatus : untested
  
  
  return(obj@tearDownError)
}


getErrorMsg.TestCaseTestResultData <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestCaseTestResultData]
  ##
  ##@in   obj : [TestCaseTestResultData]
  ##@ret      : [character]
  ##
  ##@codestatus : untested
  
  
  return(obj@ErrorMsg)
}


getExecTime.TestCaseTestResultData <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestCaseTestResultData]
  ##
  ##@in   obj : [TestCaseTestResultData]
  ##@ret      : [numeric]
  ##
  ##@codestatus : untested
  
  
  return(obj@execTime)
}



## ----------------------------------------
##
##  3) compute methods
##
## ----------------------------------------
success.TestCaseTestResultData <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestCaseTestResultData]
  ##
  ##@in   obj : [TestCaseTestResultData] class object on which to invoke this method
  ##@ret      : [logical]
  ##
  ##@codestatus : untested


  return(!obj@deactivated && !obj@failure && !obj@error && !obj@setUpError && !obj@tearDownError)
}


## ----------------------------------------
##
##  4) print/validate methods
##
## ----------------------------------------
.printTextProtocol.TestCaseTestResultData <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestCaseTestResultData]
  ##
  ##@in   obj : [TestCaseTestResultData] class object on which to invoke this method
  ##@ret      : [NULL] returned invisible
  ##
  ##@codestatus : untested

  ##  FIXME
  ##  has to be an argument of method
  traceBackCutOff <- 7

  if(success(obj)) {
    pr(obj@functionName, ":", " ... OK (", obj@execTime, " seconds)", sep="")
    
  } else {
    if(obj@error) {
      pr(functionName, ": ERROR !! ", sep="")
      
    } else if (obj@failure) {
      pr(functionName, ": FAILURE !! (check number ", obj@checkNum, ")", sep="")
      
    } else if (obj@deactivated) {
      pr(functionName, ": DEACTIVATED, ", nl=FALSE)
      
    } else {
      pr(functionName, ": unknown error kind", sep="")
    }
    
    pr(obj@errorMsg, nl=FALSE)
    if(length(obj@traceBack) > 0) {
      pr("   Call Stack:")
      if(traceBackCutOff > length(obj@traceBack)) {
        pr("   (traceBackCutOff: ",traceBackCutOff," argument larger than length of trace back: full trace back printed)")
        for(i in seq(along=obj@traceBack)) {
          pr("   ", i, ": ", obj@traceBack[i], sep="")
        }
        
      } else {
        for(i in traceBackCutOff:length(obj@traceBack)) {
          pr("   ", 1+i-traceBackCutOff, ": ", obj@traceBack[i], sep="")
        }
      }
    }
    
  }
  return(invisible())
}


showObject.TestCaseTestResultData <- function(object) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestCaseTestResultData]
  ##
  ##@in   object : [TestCaseTestResultData] class object on which to invoke this method
  ##@ret         : [NULL] returned invisible
  ##
  ##@codestatus : untested

  objSlotNames <- slotNames(object)
  for (si in objSlotNames) {
    cat("\n slot ",si," [",class(slot(object, si)),"]:\n",sep="")
    show(slot(object, si))
  }
  cat("\n")
  
  return(invisible())
}


printObject.TestCaseTestResultData <- function(x) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestCaseTestResultData]
  ##
  ##@in   x   : [TestCaseTestResultData] class object on which to invoke this method
  ##@ret      : [TestCaseTestResultData] returned invisible
  ##
  ##@codestatus : untested

  className <- class(x)
  cat("\nAn object of type",className)
  cat("\n-----------------",rep("-", length=nchar(className)),sep="")
  cat("\n  contains:\n")

  show(x)

  return(invisible(x))
}


verifyObject.TestCaseTestResultData <- function(obj) {
  ##@bdescr
  ##  generic validation method
  ##@edescr
  ##
  ##@class : [TestCaseTestResultData]
  ##
  ##@in  obj  : [TestCaseTestResultData] the object on which to invoke the method
  ##@ret      : [logical] TRUE if obj is valid
  ##
  ##@codestatus : untested

  if (length(obj@functionName) != 1) {
    errMsg <- paste("invalid '",is(obj)[1],"' class object: slot 'name' has to",
                    "be vector of length 1.", sep="")
    setError(errMsg)
    return(FALSE)
  }
  if (length(obj@deactivated) != 1) {
    errMsg <- paste("invalid '",is(obj)[1],"' class object: slot 'deactivated'",
                    "has to be vector of length 1.", sep="")
    setError(errMsg)
    return(FALSE)
  }
  if (length(obj@failure) != 1) {
    errMsg <- paste("invalid '",is(obj)[1],"' class object: slot 'failure'",
                    "has to be vector of length 1.", sep="")
    setError(errMsg)
    return(FALSE)
  }
  if (length(obj@error) != 1) {
    errMsg <- paste("invalid '",is(obj)[1],"' class object: slot 'error'",
                    "has to be vector of length 1.", sep="")
    setError(errMsg)
    return(FALSE)
  }

  ##  TODO more slot checks

  
  return(TRUE)
}



.defineTestCaseTestResultDataMethods <- function(where=environment()) {
  ##@bdescr
  ## Initialization Function
  ## Not to be called by user(s).
  ## Method definitions for the class 'TestCaseTestResultData'.
  ##@edescr
  ##
  ##@class : [TestCaseTestResultData]

  if (.GLOBAL$getDebug()) {
    
    cat(".defineTestCaseTestResultDataMethods ... ")
  }


  ##  1) constructor methods
  setMethod("initialize", c("TestCaseTestResultData"), 
            .initialize.TestCaseTestResultData, where=where)
  
  defineMethod(".constructTestCaseTestResultData",
               c("TestCaseTestResultData", "character"), 
               .constructTestCaseTestResultData.result, addEllipse=TRUE, where=where)

  defineMethod(".constructTestCaseTestResultData", c("TestCaseTestResultData"), 
               .constructTestCaseTestResultData.empty, where=where)

  
  ##  2) accessor methods
  defineMethod("getFunctionName", c("TestCaseTestResultData"),
               getFunctionName.TestCaseTestResultData, where=where)
  defineMethod("getSourceFileName", c("TestCaseTestResultData"),
               getSourceFileName.TestCaseTestResultData, where=where)
  defineMethod("getDeactivated", c("TestCaseTestResultData"),
               getDeactivated.TestCaseTestResultData, where=where)
  defineMethod("getFailure", c("TestCaseTestResultData"),
               getFailure.TestCaseTestResultData, where=where)
  defineMethod("getError", c("TestCaseTestResultData"),
               getError.TestCaseTestResultData, where=where)
  defineMethod("getSetUpError", c("TestCaseTestResultData"),
               getSetUpError.TestCaseTestResultData, where=where)
  defineMethod("getTearDownError", c("TestCaseTestResultData"),
               getTearDownError.TestCaseTestResultData, where=where)
  defineMethod("getErrorMsg", c("TestCaseTestResultData"),
               getErrorMsg.TestCaseTestResultData, where=where)
  defineMethod("getExecTime", c("TestCaseTestResultData"),
               getExecTime.TestCaseTestResultData, where=where)
  
  ##  3) compute methods

 
  ##  4) print/verify methods
  defineMethod(".printTextProtocol", c("TestCaseTestResultData"),
               .printTextProtocol.TestCaseTestResultData, where=where)
  setMethod("show", c("TestCaseTestResultData"),
            showObject.TestCaseTestResultData, where=where)
  setMethod("print", c("TestCaseTestResultData"),
            printObject.TestCaseTestResultData, where=where)
  
  defineMethod("verifyObject", c("TestCaseTestResultData"),
               verifyObject.TestCaseTestResultData, where=where)


  if (.GLOBAL$getDebug()) {
    
    cat("o.k.\n")
  }
}
