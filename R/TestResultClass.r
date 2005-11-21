##
##
##
.defineTestResultClass <- function(where=environment())
{
  ##@bdescr
  ## This function contains the definition of the 'TestResult' class.
  ## 
  ## 
  ##@edescr
  ##
  ##@class [TestResult] : storing all result information for one test suite

  if (GLOBAL$getDebug())
  {
    cat(".defineTestResultClass ... ")
  }

  setClass("TestResult",
           representation("VIRTUAL"),
           sealed   = GLOBAL$SEALED,
           where    = where,
           validity = NULL)

  if (GLOBAL$getDebug())
  {
    cat("o.k.\n")
  }
}
