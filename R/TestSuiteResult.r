##
##  RUnit
##  S4 implementation
##
##  $Id$
##
.defineTestSuiteResultClass <- function(where=environment())
{
  ##@bdescr
  ## This function contains the definition of the '' class.
  ## 
  ## 
  ##@edescr
  ##
  ##@class [] : storing all result information for one test suite

  if (GLOBAL$getDebug())
  {
    cat(".defineTestSuiteResultClass ... ")
  }

  setClass("TestSuiteResult",
           representation("TestResult",
                          directories     = "character",
                          testFileRegexp  = "character",
                          testFuncRegexp  = "character",
                          testFunctionNum = "integer",
                          deactivatedNum  = "integer",
                          errorNum        = "integer",
                          failureNum      = "integer",
                          testFileResults = "TestFileResultArray"),
           sealed   = GLOBAL$SEALED,
           where    = where,
           validity = NULL)

  if (GLOBAL$getDebug())
  {
    cat("o.k.\n")
  }
}
