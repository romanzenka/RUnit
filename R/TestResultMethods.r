##
##  RUnit
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
