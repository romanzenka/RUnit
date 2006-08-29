##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2006 Matthias Burger
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
newTestSuite <- function(...) {
  ##@bdescr
  ##  standard construction function
  ##@edescr
  ##
  ##@class    : [TestSuite]
  ##
  ##@in  ...  : [ANY]
  ##@ret      : [TestSuite] new class object
  ##
  ##@codestatus : untested

  this <- new("TestSuite")
  

  ##  postcondition
  ASSERT(verifyObject(this),
         paste("validation of constructed '",is(this)[1],
               "' class object failed:\n",
               .GLOBAL$getLastErrorMsg(), sep=""))
  return(this)
}


.initialize.TestSuite <- function(.Object, ...) {
  ##@bdescr
  ##  standard constructor method
  ##@edescr
  ##

  ##@class    : [TestSuite]
  ##
  ##@in  .Object : [TestSuite] class object
  ##@ret         : [TestSuite] new class object
  ##
  ##@codestatus : untested

  return(.constructTestSuite(.Object, ...))
}


.constructTestSuite.empty <- function(obj) {
  ##@bdescr
  ##  construct a TestSuite class object
  ##@edescr
  ##
  ##@class  : [TestSuite]
  ##@ipstatus : no:NA
  ##
  ##@in obj   : [TestSuite]
  ##@ret      : [TestSuite]
  ##
  ##@codestatus : internal

  return(obj)
}


.constructTestSuite.complete <- function(obj, para1, para2, para3,
                                         para4, para5, para6) {
  ##@bdescr
  ##  construct a TestSuite class object
  ##@edescr
  ##
  ##@class  : [TestSuite]
  ##@ipstatus : no:NA
  ##
  ##@in  obj            : [TestSuite]
  ##@in  name           : [character] name of the test suite
  ##@in  dirs           : [character] vector of paths to search for test case files 
  ##@in  testFileRegexp : [character] regular expression 
  ##@in  testFuncRegexp : [character] regular expression 
  ##@in  rngKind        : [character] RNG method  
  ##@in  rngNormalKind  : [character] RNG version 
  ##@ret      : [TestSuite]
  ##
  ##@codestatus : internal
  
  obj@name <- name
  obj@dirs <- dirs
  obj@testFileRegexp <- testFileRegexp
  obj@testFuncRegexp <- testFuncRegexp
  obj@rngKind <- rngKind
  obj@rngNormalKind <- rngNormalKind


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
isValidTestSuite.TestSuite <- function(obj) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@ipstatus : no:NA
  ##@class    : [TestSuite]
  ##@in   obj   : [TestSuite] the object on which to invoke this method.
  ##@ret : [logical] ...
  ##
  ##@codestatus : untested

  return()
}


runTestSuite.TestSuite <- function(obj, para1) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@ipstatus : no:NA
  ##@class    : [TestSuite]
  ##@in   obj   : [TestSuite] the object on which to invoke this method.
  ##@in   para1 : [logical]  
  ##@ret : [RUnitTestData] ...
  ##
  ##@codestatus : untested

  return()
}



## ----------------------------------------
##
##  4) print/validate methods
##
## ----------------------------------------
show.TestSuite <- function(object) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [TestSuite]
  ##
  ##@in   object : [TestSuite] the object on which to invoke this method
  ##@ret         : [NULL] returned invisible
  ##
  ##@codestatus : untested

  return(invisible(NULL))
}


printObject.TestSuite <- function(x) {
  ##@bdescr
  ##  generic print method
  ##@edescr
  ##
  ##@class : [TestSuite]
  ##
  ##@in  x  : [TestSuite] the object on which to invoke the method
  ##@ret    : [TestSuite] returned invisible
  ##
  ##@codestatus : untested

  cat("\nAn object of type 'TestSuite'")
  cat("\n=======================")
  ##  add slot print calls

  return(invisible(x))
}


verifyObject.TestSuite <- function(obj) {
  ##@bdescr
  ##  generic validation method
  ##@edescr
  ##
  ##@class : [TestSuite]
  ##
  ##@in  obj  : [TestSuite] the object on which to invoke the method
  ##@ret      : [logical] TRUE if obj is valid
  ##
  ##@codestatus : untested

  return(FALSE)
}



.defineTestSuiteMethods <- function(where=environment()) {
  ##@bdescr
  ## Initialization Function
  ## Not to be called by user(s).
  ## Method definitions for the class 'TestSuite'.
  ##@edescr
  ##
  ##@class : [TestSuite]



  ##  1) constructor methods
  setMethod("initialize", c("TestSuite"), 
            .initialize.TestSuite, where=where)
  
  defineMethod(".constructTestSuite", c("TestSuite"), 
               .constructTestSuite.empty, where=where)
  
  ##  2) accessor methods
  ##  3) compute methods
  defineMethod("isValidTestSuite", c("TestSuite"),
               isValidTestSuite.TestSuite, where=where)
  defineMethod("runTestSuite", c("TestSuite", "logical"),
               runTestSuite.TestSuite, where=where)
  
  ##  4) print/verify methods
  defineMethod("show", c("TestSuite"),
               show.TestSuite, where=where)
  defineMethod("print",  c("TestSuite"),
               printObject.TestSuite,where=where)
  defineMethod("verifyObject", c("TestSuite"),
               verifyObject.TestSuite,where=where)



}
