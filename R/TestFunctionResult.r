##
##
##  RUnit
##  S4 implementation
##
##  $Id$
##

.defineTestFunctionResultClass <- function(where=environment())
{
  ##@bdescr
  ## This function contains the definition of the 'TestFunctionResult' class.
  ## 
  ## 
  ##@edescr
  ##
  ##@class [TestFunctionResult] : storing all result information for one test function

  if (GLOBAL$getDebug())
  {
    cat(".defineTestFunctionResultClass ... ")
  }

  setClass("TestFunctionResult",
           representation("TestResult",
                          kind = "character",
                          time = "numeric",
                          message = "character"),
           sealed   = GLOBAL$SEALED,
           where    = where,
           validity = NULL)

  if (GLOBAL$getDebug())
  {
    cat("o.k.\n")
  }
}
