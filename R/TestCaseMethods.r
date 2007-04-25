##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003-2007  Thomas Koenig, Matthias Burger, Klaus Juenemann
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


## --------------------------------------
##
##  1) constructor methods
##
## --------------------------------------
newTestCase <- function(...) {
  ##@bdescr
  ##  standard construction function
  ##@edescr
  ##
  ##@class    : [TestCase]
  ##
  ##@in  ...  : [ANY] used for dispatch to constructor method
  ##@ret      : [TestCase] new class object
  ##
  ##@codestatus : untested

  this <- new("TestCase", ...)

  ##  postcondition
  ASSERT(verifyObject(this),
         paste("validation of constructed '",is(this)[1],"' class object failed:\n",
               .GLOBAL$getLastErrorMsg(), sep=""))
  return(this)
}


.initialize.TestCase <- function(.Object, ...) {

  ##@bdescr
  ##  standard constructor method
  ##@edescr
  ##
  ##@class    : [TestCase]
  ##
  ##@in  .Object : [TestCase] class object
  ##@ret         : [TestCase] new class object
  ##
  ##@codestatus : untested

  return(.constructTestCase(.Object, ...))
}


.constructTestCase.empty <- function(obj) {
  ##@bdescr
  ##  construct a TestCase class object
  ##@edescr
  ##
  ##@class  : [TestCase]
  ##
  ##@in obj   : [TestCase]
  ##@ret      : [TestCase]
  ##
  ##@codestatus : internal

  return(obj)
}


.constructTestCase.character <- function(obj, para1, ...) {
  ##@bdescr
  ##  construct a TestCase class object
  ##@edescr
  ##
  ##@class  : [TestCase]
  ##
  ##@in  obj   : [TestCase]
  ##@in  para1 : [character] function name
  ##@in  ...   : [character]
  ##@ret       : [TestCase]
  ##
  ##@codestatus : internal

  ##  preconditions
  ASSERT( length(para1) == 1)
  ASSERT( !is.na(para1))

  ##  may be missing
  arg <- list(...)
  if (length(arg) > 0) {
    if (length(arg) != 1) {
      setFatalError("only one argument expected.")
    }
  
    if (!(names(arg) %in% c("sourceFileName"))) {
      setFatalError("argument name not matched in slots.")
    }
  
    slot(obj, names(arg)) <- arg[[1]]
  }
  
  obj@functionName <- para1
  
  return(obj)
}


## --------------------------------------
##
##  2) accessor methods
##
## --------------------------------------
getFunctionName.TestCase <- function(obj) {
  ##@bdescr
  ##  getter
  ##  returns test function name
  ##@edescr
  ##
  ##@class    : [TestCase]
  ##
  ##@in   obj : [TestCase] the object on which to invoke this method
  ##@ret      : [character] test function name
  ##
  ##@codestatus : untested

  
  return(obj@functionName)
}


setFunctionName.TestCase <- function(obj, value) {
  ##@bdescr
  ##  setter
  ##  returns modified class object
  ##@edescr
  ##
  ##@class    : [TestCase]
  ##
  ##@in  obj   : [TestCase] the object on which to invoke this method
  ##@in  value : [character]
  ##@ret       : [TestCase] modified class object
  ##
  ##@codestatus : untested

  obj@functionName <- value

  return(obj)
}


getSourceFileName.TestCase <- function(obj) {
  ##@bdescr
  ##  getter
  ##  returns source file name (incl. absolute path)
  ##@edescr
  ##
  ##@class    : [TestCase]
  ##
  ##@in   obj : [TestCase] the object on which to invoke this method
  ##@ret      : [character] source file name
  ##
  ##@codestatus : untested

  
  return(obj@sourceFileName)
}


setSourceFileName.TestCase <- function(obj, value) {
  ##@bdescr
  ##  setter
  ##  returns modified class object
  ##@edescr
  ##
  ##@class    : [TestCase]
  ##
  ##@in  obj   : [TestCase] the object on which to invoke this method
  ##@in  value : [character] source file name
  ##@ret       : [TestCase] modified class object
  ##
  ##@codestatus : untested

  obj@sourceFileName <- value
  
  return(obj)
}



## --------------------------------------
##
##  3) compute methods
##
## --------------------------------------



## --------------------------------------
##
##  4) print/validate methods
##
## --------------------------------------
showObject.TestCase <- function(object) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestCase]
  ##
  ##@in   object : [TestCase] the object on which to invoke this method
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


printObject.TestCase <- function(x) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestCase]
  ##
  ##@in   x   : [TestCase] the object on which to invoke this method
  ##@ret      : [TestCase] returned invisible
  ##
  ##@codestatus : untested

  className <- class(x)
  cat("\nAn object of type",className)
  cat("\n-----------------",rep("-", length=nchar(className)),sep="")
  cat("\n  contains:\n")

  show(x)

  return(invisible(x))
}


verifyObject.TestCase <- function(obj) {
  ##@bdescr
  ##  generic validation method
  ##@edescr
  ##
  ##@class : [TestCase]
  ##
  ##@in  obj  : [TestCase] the object on which to invoke the method
  ##@ret      : [logical] TRUE if obj is valid
  ##
  ##@codestatus : untested

  if (length(obj@functionName) != 1) {
    errMsg <- paste("invalid '",is(obj)[1],"' class object: slot 'functionName' has to",
                    "be vector of length 1.", sep="")
    setError(errMsg)
    return(FALSE)
  }
  if (is.na(obj@functionName)) {
    errMsg <- paste("invalid '",is(obj)[1],"' class object: slot 'functionName'",
                    "contains missing value.", sep="")
    setError(errMsg)
    return(FALSE)
  }
##   if (length(obj@sourceFileName) != 1) {
##     errMsg <- paste("invalid '",is(obj)[1],"' class object: slot 'sourceFileName'",
##                     "has to be vector of length 1.", sep="")
##     setError(errMsg)
##     return(FALSE)
##   }
##   if (is.na(obj@sourceFileName)) {
##     errMsg <- paste("invalid '",is(obj)[1],"' class object: slot 'sourceFileName'",
##                     "contains missing value.", sep="")
##     setError(errMsg)
##     return(FALSE)
##   }
  
  ##  TODO more slot checks
  
  return(TRUE)
}



.defineTestCaseMethods <- function(where=environment()) {
  ##@bdescr
  ## Initialization Function
  ## Not to be called by user(s).
  ## Method definitions for the class 'TestCase'.
  ##@edescr
  ##
  ##@class : [TestCase]

  if (.GLOBAL$getDebug()) {
    
    cat(".defineTestCaseMethods ... ")
  }


  ##  1) constructor methods
  setMethod("initialize", c("TestCase"), 
            .initialize.TestCase, where=where)
  defineMethod(".constructTestCase", c("TestCase", "character"), 
               .constructTestCase.character, addEllipse=TRUE, where=where)
  defineMethod(".constructTestCase", c("TestCase"), 
               .constructTestCase.empty, where=where)

  
  ##  2) accessor methods
  defineMethod("getFunctionName", c("TestCase"), 
               getFunctionName.TestCase, where=where)
  defineSetMethod("setFunctionName", c("TestCase", "character"), 
                  setFunctionName.TestCase, where=where)
  defineMethod("getSourceFileName", c("TestCase"), 
               getSourceFileName.TestCase, where=where)
  defineSetMethod("setSourceFileName", c("TestCase", "character"), 
                  setSourceFileName.TestCase, where=where)
  
  ##  3) compute methods

 
  ##  4) print/verify methods
  setMethod("show", c("TestCase"),
            showObject.TestCase, where=where)
  setMethod("print", c("TestCase"),
            printObject.TestCase, where=where)
  
  defineMethod("verifyObject", c("TestCase"),
               verifyObject.TestCase,where=where)


  if (.GLOBAL$getDebug()) {
    
    cat("o.k.\n")
  }
}
