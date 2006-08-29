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
newSourceFileTestResultData <- function(...) {
  ##@bdescr
  ##  standard construction function
  ##@edescr
  ##
  ##@class    : [SourceFileTestResultData]
  ##
  ##@in  ...  : [ANY]
  ##@ret      : [SourceFileTestResultData] new class object
  ##
  ##@codestatus : untested

  this <- new("SourceFileTestResultData", ...)

  ##  postcondition
  ASSERT(verifyObject(this),
         paste("validation of constructed '",is(this)[1],"' class object failed:\n",
               .GLOBAL$getLastErrorMsg(), sep=""))
  return(this)
}


.initialize.SourceFileTestResultData <- function(.Object, ...) {

  ##@bdescr
  ##  standard constructor method
  ##@edescr
  ##
  ##@class    : [SourceFileTestResultData]
  ##
  ##@in  .Object : [SourceFileTestResultData] class object
  ##@ret         : [SourceFileTestResultData] new class object
  ##
  ##@codestatus : untested

  return(.constructSourceFileTestResultData(.Object, ...))
}


.constructSourceFileTestResultData.empty <- function(obj) {
  ##@bdescr
  ##  construct a SourceFileTestResultData class object
  ##@edescr
  ##
  ##@class  : [SourceFileTestResultData]
  ##
  ##@in obj   : [SourceFileTestResultData]
  ##@ret      : [SourceFileTestResultData]
  ##
  ##@codestatus : internal

  return(obj)
}



## ----------------------------------------
##
##  2) accessor methods
##
## ----------------------------------------
getErrors.SourceFileTestResultData <- function(obj) {
  ##@bdescr
  ##  getter
  ##@edescr
  ##
  ##@class    : [SourceFileTestResultData]
  ##
  ##@in   obj : [SourceFileTestResultData] the object on which to invoke this method
  ##@ret      : [] ...
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
print.SourceFileTestResultData <- function(x) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [SourceFileTestResultData]
  ##
  ##@in   x   : [SourceFileTestResultData] the object on which to invoke this method
  ##@ret      : [SourceFileTestResultData] returned invisible
  ##
  ##@codestatus : untested

  return(invisible(x))
}


show.SourceFileTestResultData <- function(object) {
  ##@bdescr
  ## 
  ##@edescr
  ##
  ##@class    : [SourceFileTestResultData]
  ##
  ##@in   object : [SourceFileTestResultData] the object on which to invoke this method
  ##@ret         : [NULL] returned invisible
  ##
  ##@codestatus : untested

  return(invisible(NULL))
}


verifyObject.SourceFileTestResultData <- function(obj) {
  ##@bdescr
  ##  generic validation method
  ##@edescr
  ##
  ##@class : [SourceFileTestResultData]
  ##
  ##@in  obj  : [SourceFileTestResultData] the object on which to invoke the method
  ##@ret      : [logical] TRUE if obj is valid
  ##
  ##@codestatus : untested

  return(FALSE)
}



.defineSourceFileTestResultDataMethods <- function(where=environment()) {
  ##@bdescr
  ## Initialization Function
  ## Not to be called by user(s).
  ## Method definitions for the class 'SourceFileTestResultData'.
  ##@edescr
  ##
  ##@class : [SourceFileTestResultData]


  if (.GLOBAL$getDebug()) {
    
    cat(".defineSourceFileTestResultDataMethods ... ")
  }

  ##  1) constructor methods
  setMethod("initialize", c("SourceFileTestResultData"), 
            .initialize.SourceFileTestResultData, where=where)
  
  defineMethod(".constructSourceFileTestResultData",
               c("SourceFileTestResultData"), 
               .constructSourceFileTestResultData.empty, where=where)

    
  ##  2) accessor methods
  defineMethod("getErrors", c("SourceFileTestResultData"),
               getErrors.SourceFileTestResultData, where=where)

    
  ##  3) compute methods
 
  ##  4) print/verify methods
  defineMethod("print", c("SourceFileTestResultData"),
               print.SourceFileTestResultData, where=where)
  defineMethod("show", c("SourceFileTestResultData"),
               show.SourceFileTestResultData, where=where)
 
  defineMethod("verifyObject", c("SourceFileTestResultData"),
               verifyObject.SourceFileTestResultData, where=where)

  
  if (.GLOBAL$getDebug()) {
    
    cat("o.k.\n")
  }
}
