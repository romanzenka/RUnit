##
##  RUnit
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
