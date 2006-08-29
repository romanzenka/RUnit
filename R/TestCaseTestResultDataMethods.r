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



## ----------------------------------------
##
##  4) print/validate methods
##
## ----------------------------------------
print.TestCaseTestResultData <- function(x) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestCaseTestResultData]
  ##
  ##@in   x   : [TestCaseTestResultData] the object on which to invoke this method
  ##@ret      : [TestCaseTestResultData] returned invisible
  ##
  ##@codestatus : untested

  return(invisible(x))
}


show.TestCaseTestResultData <- function(object) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestCaseTestResultData]
  ##
  ##@in   object : [TestCaseTestResultData] the object on which to invoke this method
  ##@ret         : [NULL] returned invisible
  ##
  ##@codestatus : untested

  return(invisible(NULL))
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

  return(FALSE)
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
  
  defineMethod(".constructTestCaseTestResultData", c("TestCaseTestResultData"), 
               .constructTestCaseTestResultData.empty, where=where)

  
  ##  2) accessor methods
  ##  3) compute methods

 
  ##  4) print/verify methods
  defineMethod("print", c("TestCaseTestResultData"),
               print.TestCaseTestResultData, where=where)
  defineMethod("show", c("TestCaseTestResultData"),
               show.TestCaseTestResultData, where=where)
  
  defineMethod("verifyObject", c("TestCaseTestResultData"),
               verifyObject.TestCaseTestResultData,where=where)


  if (.GLOBAL$getDebug()) {
    
    cat("o.k.\n")
  }
}
