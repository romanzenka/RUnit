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
.printObject.TestResult <- function(obj) {
  ##@bdescr
  ##  print method
  ##@edescr
  ##
  ##@class : [TestResult]
  ##
  ##@in  obj : [TestResult]
  ##@ret     : [NULL] 
  ##
  ##@codestatus : untested

  ##  currently not implemented, provided
  ##  only for method dispatch
  cat("\nAn object of class 'TestResult':")
  cat("\n--------------------------------")
  cat("\n  provides no slots")
  
  return(NULL)
}


.verifyObject.TestResult <- function(obj) {
  ##@bdescr
  ##  verification method
  ##@edescr
  ##
  ##@class : [TestResult]
  ##
  ##@in  obj : [TestResult]
  ##@ret     : [logical] TRUE iff object is a valid class member
  ##
  ##@codestatus : untested

  ##  currentyl no logic implemented, provided
  ##  only for method dispatch
  return(TRUE)
}


.defineTestResultMethods <- function(where=environment())
{
  ##@bdescr
  ## Initialization Function
  ## Not to be called by user(s).
  ## Method definitions for the class 'TestResult'.
  ##@edescr
  ##
  ##@class     : [TestResult]

  if (GLOBAL$getDebug()) {
    cat(".defineTestResultMethods ... ")
  }

  ##  default generics
  setMethod("verifyObject",  signature=c("TestResult"),
            .verifyObject.TestResult, where=where, valueClass="logical",
            sealed=TRUE)
  setMethod("printObject", signature=c("TestResult"),
            .printObject.TestResult, where=where, sealed=TRUE)

  if (GLOBAL$getDebug()) {
    cat("o.k.\n")
  }
}
