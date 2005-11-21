##
##
##  RUnit
##  S4 implementation
##
##  $Id$
##

.defineTestFileResultClass <- function(where=environment())
{
  ##@bdescr
  ## This function contains the definition of the 'TestFileResult' class.
  ## 
  ## 
  ##@edescr
  ##
  ##@class [TestFileResult] : storing all result information for one test suite

  if (GLOBAL$getDebug()) {
    cat(".defineTestFileResultClass ... ")
  }

  setClass("TestFileResult",
           representation("TestResult",
                          testFunctionArray = "TestFunctionResultArray"),
           sealed   = GLOBAL$SEALED,
           where    = where,
           validity = NULL)

  if (GLOBAL$getDebug()) {
    cat("o.k.\n")
  }
}
