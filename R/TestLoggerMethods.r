##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2006 Matthias Burger, Thomas Koenig
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
newTestLogger <- function(...) {
  ##@bdescr
  ##  standard construction function
  ##@edescr
  ##
  ##@class    : [TestLogger]
  ##
  ##@in   ...  : [ANY]  
  ##@ret       : [TestLogger] new class object
  ##
  ##@codestatus : untested

  this <- new("TestLogger", ...)
  

  ##  postcondition
  ASSERT(verifyObject(this),
         paste("validation of constructed 'TestLogger' class object failed:\n",
               .GLOBAL$getLastErrorMsg()))
  return(this)
}


.initialize.TestLogger <- function(.Object, ...) {
  ##@bdescr
  ##  standard constructor method
  ##@edescr
  ##
  ##@class    : [TestLogger]
  ##
  ##@in  .Object : [TestLogger] class object
  ##@ret         : [TestLogger] new class object
  ##
  ##@codestatus : untested

  return(.constructTestLogger(.Object, ...))
}


.constructTestLogger.empty <- function(obj) {
  ##@bdescr
  ##  construct a TestLogger class object
  ##@edescr
  ##
  ##@class  : [TestLogger]
  ##
  ##@in obj   : [TestLogger]
  ##@ret      : [TestLogger]
  ##
  ##@codestatus : internal

  return(obj)
}


.constructTestLogger.TestData <- function(obj, para1) {
  ##@bdescr
  ##  construct a TestLogger class object
  ##@edescr
  ##
  ##@class  : [TestLogger]
  ##
  ##@in  obj    : [TestLogger]
  ##@in  para1  : [TestResultData] 
  ##@ret        : [TestLogger]
  ##
  ##@codestatus : internal

  obj@testResultData <- para1
  
  return(obj)
}


.constructTestLogger.logical <- function(obj, para1) {
  ##@bdescr
  ##  construct a TestLogger class object
  ##@edescr
  ##
  ##@class  : [TestLogger]
  ##
  ##@in  obj    : [TestLogger]
  ##@in  para1  : [logical] 
  ##@ret        : [TestLogger]
  ##
  ##@codestatus : internal

  obj@useOwnErrorHandler <- para1
  
  return(obj)
}



## ----------------------------------------
##
##  2) accessor methods
##
## ----------------------------------------



## ----------------------------------------
##
##  3) compute methods
##
## ----------------------------------------
isValidTestData.TestLogger <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestLogger]
  ##
  ##@in   obj : [TestLogger] the object on which to invoke this method.
  ##@ret      : [logical] ...
  ##
  ##@codestatus : untested

  return()
}


errorHandler.TestLogger <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestLogger]
  ##
  ##@in   obj   : [TestLogger] the object on which to invoke this method.
  ##@ret : [] ...
  ##
  ##@codestatus : untested

  return()
}


getTestData.TestLogger <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestLogger]
  ##
  ##@in   obj  : [TestLogger] the object on which to invoke this method.
  ##@ret       : [testResultData] ...
  ##
  ##@codestatus : untested

  return()
}


setCurrentTestSuite.TestLogger <- function(obj, para1) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestLogger]
  ##
  ##@in   obj   : [TestLogger] the object on which to invoke this method.
  ##@in   para1 : [RUnitTestSuite]  
  ##@ret        : [] ...
  ##
  ##@codestatus : untested

  return()
}


setCurrentSourceFile.TestLogger <- function(obj, para1) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestLogger]
  ##
  ##@in   obj   : [TestLogger] the object on which to invoke this method.
  ##@in   para1 : [character]  
  ##@ret        : [] ...
  ##
  ##@codestatus : untested

  return()
}


addSuccess.TestLogger <- function(obj, para1, para2) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestLogger]
  ##
  ##@in   obj   : [TestLogger] the object on which to invoke this method.
  ##@in   para1 : [character]  
  ##@in   para2 : [integer] test execution timing in seconds 
  ##@ret        : [] ...
  ##
  ##@codestatus : untested

  return()
}


addError.TestLogger <- function(obj, para1, para2) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestLogger]
  ##
  ##@in   obj   : [TestLogger] the object on which to invoke this method.
  ##@in   para1 : [character]  
  ##@in   para2 : [character] error message string 
  ##@ret        : [] ...
  ##
  ##@codestatus : untested

  return()
}


addFailure.TestLogger <- function(obj, para1, para2) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestLogger]
  ##
  ##@in   obj   : [TestLogger] the object on which to invoke this method.
  ##@in   para1 : [character]  
  ##@in   para2 : [character] failure message string 
  ##@ret        : [] ...
  ##
  ##@codestatus : untested

  return()
}


addDeactivated.TestLogger <- function(obj, para1) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestLogger]
  ##
  ##@in   obj   : [TestLogger] the object on which to invoke this method.
  ##@in   para1 : [character]  
  ##@ret        : [] ...
  ##
  ##@codestatus : untested

  return()
}


isFailure.TestLogger <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestLogger]
  ##
  ##@in   obj : [TestLogger] the object on which to invoke this method.
  ##@ret      : [] ...
  ##
  ##@codestatus : untested

  return()
}


setFailure.TestLogger <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestLogger]
  ##
  ##@in   obj : [TestLogger] the object on which to invoke this method.
  ##@ret      : [] ...
  ##
  ##@codestatus : untested

  return()
}


isDeactivated.TestLogger <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestLogger]
  ##
  ##@in   obj : [TestLogger] the object on which to invoke this method.
  ##@ret      : [] ...
  ##
  ##@codestatus : untested

  return()
}


setDeactivated.TestLogger <- function(obj, para1) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestLogger]
  ##
  ##@in   obj   : [TestLogger] the object on which to invoke this method.
  ##@in   para1 : [character] error message string 
  ##@ret        : [] ...
  ##
  ##@codestatus : untested

  return()
}


incrementCheckNum.TestLogger <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestLogger]
  ##
  ##@in   obj : [TestLogger] the object on which to invoke this method.
  ##@ret      : [] ...
  ##
  ##@codestatus : untested

  return()
}


cleanup.TestLogger <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestLogger]
  ##
  ##@in   obj : [TestLogger] the object on which to invoke this method.
  ##@ret      : [] ...
  ##
  ##@codestatus : untested

  return()
}



## ----------------------------------------
##
##  4) print/validate methods
##
## ----------------------------------------
printObject.TestLogger <- function(x) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestLogger]
  ##
  ##@in   x  : [TestLogger] the object on which to invoke this method.
  ##@ret     : [] ...
  ##
  ##@codestatus : untested
  
  return(invisible(x))
}


showObject.TestLogger <- function(object) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestLogger]
  ##
  ##@in  object : [TestLogger] the object on which to invoke this method.
  ##@ret        : [] ...
  ##
  ##@codestatus : untested

  return(invisible())
}


verifyObject.TestLogger <- function(obj) {
  ##@bdescr
  ##  generic validation method
  ##@edescr
  ##
  ##@class : [TestLogger]
  ##
  ##@in  obj  : [TestLogger] the object on which to invoke the method.
  ##@ret      : [logical] TRUE if obj is valid
  ##
  ##@codestatus : untested

  return(FALSE)
}



.defineTestLoggerMethods <- function(where=environment()) {
  ##@bdescr
  ## Initialization Function
  ## Not to be called by user(s).
  ## Method definitions for the class 'TestLogger'.
  ##@edescr
  ##
  ##@class : [TestLogger]


  ##  1) constructor methods
  #defineMethod(".newTestLogger", c("TestLogger", "logical"),
  #             .newTestLogger.TestLogger, where=where)
  setMethod("initialize", c("TestLogger"), 
            .initialize.TestLogger, where=where)
  
  defineMethod(".constructTestLogger", c("TestLogger"), 
               .constructTestLogger.empty, where=where)
  
  ##  2) accessor methods
  defineMethod("isValidTestData", c("TestLogger"),
               isValidTestData.TestLogger, where=where)

  defineMethod("errorHandler", c("TestLogger"),
               errorHandler.TestLogger, where=where)
  
  defineMethod("getTestData", c("TestLogger"),
               getTestData.TestLogger, where=where)
  
  defineMethod("setCurrentTestSuite", c("TestLogger", "TestSuite"),
               setCurrentTestSuite.TestLogger, where=where)
  defineMethod("setCurrentSourceFile", c("TestLogger", "character"),
               setCurrentSourceFile.TestLogger, where=where)
  defineMethod("addSuccess", c("TestLogger", "character", "integer"),
               addSuccess.TestLogger, where=where)
  defineMethod("addError", c("TestLogger", "character", "character"),
               addError.TestLogger, where=where)
  defineMethod("addFailure", c("TestLogger", "character", "character"),
               addFailure.TestLogger, where=where)
  defineMethod("addDeactivated", c("TestLogger", "character"),
               addDeactivated.TestLogger, where=where)
  defineMethod("isFailure", c("TestLogger"),
               isFailure.TestLogger, where=where)
  defineMethod("setFailure", c("TestLogger"),
               setFailure.TestLogger, where=where)
  defineMethod("isDeactivated", c("TestLogger"),
               isDeactivated.TestLogger, where=where)
  defineMethod("setDeactivated", c("TestLogger", "character"),
               setDeactivated.TestLogger, where=where)

  
  ##  3) compute methods
  defineMethod("incrementCheckNum", c("TestLogger"),
               incrementCheckNum.TestLogger, where=where)
  defineMethod("cleanup", c("TestLogger"),
               cleanup.TestLogger, where=where)

  ##  4) print/verify methods
  defineMethod("print",  c("TestLogger"), printObject.TestLogger,
               where=where)
  defineMethod("show", c("TestLogger"),
               showObject.TestLogger, where=where)
  defineMethod("verifyObject", c("TestLogger"), verifyObject.TestLogger,
               where=where)


}
