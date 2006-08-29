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
newTestSuiteTestResultData <- function(...) {
  ##@bdescr
  ##  standard construction function
  ##@edescr
  ##
  ##@ipstatus : no:NA
  ##@class    : [TestSuiteTestResultData]
  ##
  ##@in   ... : [ANY]
  ##@ret      : [TestSuiteTestResultData] new class object
  ##
  ##@codestatus : untested

  this <- new("TestSuiteTestResultData", ...)

  ##  postcondition
  ASSERT(verifyObject(this),
         paste("validation of constructed '",is(this)[1],"' class object failed:\n",
               .GLOBAL$getLastErrorMsg()))
  return(this)
}


.initialize.TestSuiteTestResultData <- function(.Object, ...) {
  ##@bdescr
  ##  standard constructor method
  ##@edescr
  ##
  ##@class    : [TestSuiteTestResultData]
  ##
  ##@in  .Object : [TestSuiteTestResultData] class object
  ##@ret         : [TestSuiteTestResultData] new class object
  ##
  ##@codestatus : untested

  return(.constructTestSuiteTestResultData(.Object, ...))
}


.constructTestSuiteTestResultData.empty <- function(obj) {
  ##@bdescr
  ##  construct a TestSuiteTestResultData class object
  ##@edescr
  ##
  ##@class  : [TestSuiteTestResultData]
  ##
  ##@in obj   : [TestSuiteTestResultData]
  ##@ret      : [TestSuiteTestResultData]
  ##
  ##@codestatus : internal

  return(obj)
}



## ----------------------------------------
##
##  2) accessor methods
##
## ----------------------------------------
getErrors.TestSuiteTestResultData <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestSuiteTestResultData]
  ##
  ##@in   obj  : [TestSuiteTestResultData] the object on which to invoke this method.
  ##@ret       : [] ...
  ##
  ##@codestatus : untested

  return()
}



## ----------------------------------------
##
##  3) compute methods
##
## ----------------------------------------



## ----------------------------------------
##
##  4) print/validate methods
##
## ----------------------------------------
print.TestSuiteTestResultData <- function(x) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestSuiteTestResultData]
  ##
  ##@in   x  : [TestSuiteTestResultData] the object on which to invoke this method.
  ##@ret     : [TestSuiteTestResultData] returned invisible
  ##
  ##@codestatus : untested

  return(invisible(x))
}


show.TestSuiteTestResultData <- function(object) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestSuiteTestResultData]
  ##
  ##@in  object : [TestSuiteTestResultData] the object on which to invoke this method
  ##@ret        : [NULL] returned invisible
  ##
  ##@codestatus : untested

  return(invisible(NULL))
}


verifyObject.TestSuiteTestResultData <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestSuiteTestResultData]
  ##
  ##@in   obj : [TestSuiteTestResultData] the object on which to invoke this method
  ##@ret      : [logical]  TRUE if obj is valid...
  ##
  ##@codestatus : untested

  return(TRUE)
}



.defineTestSuiteTestResultDataMethods <- function(where=environment()) {
  ##@bdescr
  ## Initialization Function
  ## Not to be called by user(s).
  ## Method definitions for the class 'TestSuiteTestResultData'.
  ##@edescr
  ##
  ##@class : [TestSuiteTestResultData]


  if (.GLOBAL$getDebug()) {
    
    cat(".defineTestSuiteTestResultDataMethods ... ")
  }
  
  ##  1) constructor methods
  setMethod("initialize", c("TestSuiteTestResultData"), 
            .initialize.TestSuiteTestResultData, where=where)
  
  defineMethod(".constructTestSuiteTestResultData",
               c("TestSuiteTestResultData"), 
               .constructTestSuiteTestResultData.empty, where=where)

  ##  2) accessor methods
  defineMethod("getErrors", c("TestSuiteTestResultData"),
               getErrors.TestSuiteTestResultData, where=where)
  
  ##  3) compute methods
  
  ##  4) print/verify methods
  setMethod("print", signature("TestSuiteTestResultData"),
            print.TestSuiteTestResultData, where=where)
  defineMethod("show", c("TestSuiteTestResultData"),
               show.TestSuiteTestResultData, where=where)
  defineMethod("verifyObject", c("TestSuiteTestResultData"),
               verifyObject.TestSuiteTestResultData, where=where)


  if (.GLOBAL$getDebug()) {
    
    cat("o.k.\n")
  }
}
