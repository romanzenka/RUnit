##
##  RUnit
##  S4 implementation
##
##  $Id$
##

##  4) print/validation methods
##
.printObject.TestFileResult <- function(obj) {
  ##@bdescr
  ##  print method
  ##@edescr
  ##
  ##@class : [TestFileResult]
  ##
  ##@in  obj : [TestFileResult]
  ##@ret     : [NULL] 
  ##
  ##@codestatus : untested

  ##  currently not implemented, provided
  ##  only for method dispatch
  cat("\nAn object of class 'TestFileResult':")
  cat("\n--------------------------------")
  cat("\n  provides slots")
  
  return(NULL)
}


.verifyObject.TestFileResult <- function(obj) {
  ##@bdescr
  ##  verification method
  ##@edescr
  ##
  ##@class : [TestFileResult]
  ##
  ##@in  obj : [TestFileResult]
  ##@ret     : [logical] TRUE iff object is a valid class member
  ##
  ##@codestatus : untested

  ##  currentyl no logic implemented, provided
  ##  only for method dispatch
  return(TRUE)
}


.defineTestFileResultMethods <- function(where=environment())
{
  ##@bdescr
  ## Initialization Function
  ## Not to be called by user(s).
  ## Method definitions for the class 'TestFileResult'.
  ##@edescr
  ##
  ##@class     : [TestFileResult]

  if (GLOBAL$getDebug()) {
    cat(".defineTestFileResultMethods ... ")
  }

  ##  default generics
  setMethod("verifyObject",  signature=c("TestFileResult"),
            .verifyObject.TestFileResult, where=where, valueClass="logical",
            sealed=TRUE)
  setMethod("printObject", signature=c("TestFileResult"),
            .printObject.TestFileResult, where=where, sealed=TRUE)

  if (GLOBAL$getDebug()) {
    cat("o.k.\n")
  }
}
