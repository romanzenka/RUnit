##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003-2006  Thomas Koenig, Matthias Burger, Klaus Juenemann
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
##
##  S4 implementation
##
##  $Id$
##

##  4) print/validation methods
##
.printObject.TestSuiteResult <- function(obj) {
  ##@bdescr
  ##  print method
  ##@edescr
  ##
  ##@class : [TestSuiteResult]
  ##
  ##@in  obj : [TestSuiteResult]
  ##@ret     : [NULL] 
  ##
  ##@codestatus : untested

  ##  currently not implemented, provided
  ##  only for method dispatch
  cat("\nAn object of class 'TestSuiteResult':")
  cat("\n--------------------------------")
  cat("\n  provides slots")
  cat("\n  slot testFileRegexp: ", obj@testFileRegexp)
  cat("\n  slot testFuncRegexp: ", obj@testFuncRegexp)
  cat("\n  slot testFunctionNum:", obj@testFunctionNum)
  cat("\n  slot deactivatedNum: ", obj@deactivatedNum)
  cat("\n  slot errorNum:       ", obj@errorNum)
  cat("\n  slot failureNum:     ", obj@failureNum)
  cat("\n  slot testFileResults (Array)")
  cat("\n (currently print not implemented")
  
  return(NULL)
}


.verifyObject.TestSuiteResult <- function(obj) {
  ##@bdescr
  ##  verification method
  ##@edescr
  ##
  ##@class : [TestSuiteResult]
  ##
  ##@in  obj : [TestSuiteResult]
  ##@ret     : [logical] TRUE iff object is a valid class member
  ##
  ##@codestatus : untested

  ##  currentyl no logic implemented, provided
  ##  only for method dispatch
  return(TRUE)
}


.defineTestSuiteResultMethods <- function(where=environment())
{
  ##@bdescr
  ## Initialization Function
  ## Not to be called by user(s).
  ## Method definitions for the class 'TestSuiteResult'.
  ##@edescr
  ##
  ##@class     : [TestSuiteResult]

  if (GLOBAL$getDebug()) {
    cat(".defineTestSuiteResultMethods ... ")
  }

  ##  default generics
  setMethod("verifyObject",  signature=c("TestSuiteResult"),
            .verifyObject.TestSuiteResult, where=where, valueClass="logical",
            sealed=TRUE)
  setMethod("printObject", signature=c("TestSuiteResult"), 
            .printObject.TestSuiteResult, where=where)

  if (GLOBAL$getDebug()) {
    cat("o.k.\n")
  }
}
