##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003, 2004  Thomas Koenig, Matthias Burger, Klaus Juenemann
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

printTextProtocol <- function(testData,
                              fileName = "",
                              separateFailureList = TRUE,
                              showDetails = TRUE) {
  ##@bdescr
  ## Report generator
  ## Creates text-based test protocol from RUnit TestResult object.
  ## The output  format is compatible with (X)Emacs ouline mode.
  ##@edescr
  ##
  ##@in  testData            : [TestResult] S4 class object
  ##@in  fileName            : [character] string, fulll path + file name to be written to
  ##@in  separateFailureList : [logical] if TRUE (default) add a failure list
  ##@in  showDetails         : [logical] if TRUE (default) add detailed trackbacks for each error incurred
  ##@ret                     : [logical] TRUE if execution completed wo error
  
  ## Check input arguments
  flag <- isValidTestResult(testData)
  if(!isTRUE(flag)) {
    stop(flag)
  }
  if (! (is.character(fileName) && length(fileName)==1) )  {
    stop("Argument 'fileName' has to be one character string.")
  }
  if (! (is.logical(separateFailureList) && length(separateFailureList) == 1) ) {
    stop("Argument 'separateFailureList' has to be one logical.")
  }
  if (! (is.logical(showDetails) && length(showDetails) == 1) ) {
    stop("Argument 'showDetails' has to be one logical.")
  }

  ## local helper functions
  lcat <- function(...) cat(..., file=fileName, append=TRUE)
  lcatn <- function(x) lcat(x, "\n")
  nSuites <- length(testData@testSuiteData)
  
  ## header
  cat("RUNIT TEST PROTOCOL --", date(), "\n", file = fileName)
  lcat("-----------------------------------------------\n")
  
  lcat("\n* Summary\n")
  lapply(toText(testData, 0), lcatn)
  lcat("\n\n")
  
  ## optional list of all errors, failures and deactivations
  if(separateFailureList &&
     length(testFuncsByState(testData, c("ERROR", "FAILURE", "DEACTIVATED"))) > 0) {
    lcat("* Errors and Failures\n")
    lapply(testData@testSuiteData, function(ts) {
      problems <- testFuncsByState(ts, c("ERROR", "FAILURE", "DEACTIVATED"))
      if( (nSuites > 1) && length(problems) > 0) {
        lcat("** ", toText(ts@testSuite, 0), "\n")
        lapply(problems, function(x) lapply(toText(x, 0), lcatn))
      }
    })
  }
  lcat("\n\n")

  ## optional: print out all details that we have
  if(showDetails) {
    lcat("* Details\n\n")
    lapply(testData@testSuiteData, function(ts) {
      tmp <- toText(ts,2)

      ## add *s for outline mode
      tmp[1] <- paste("**", tmp[1])
      tmp <- sub("^Test file", "*** Test file", tmp)
      
      lapply(tmp, lcatn)
      lcat("\n")
    })

    ## print some important system information
    lcat("\n* System Information\n")
    lcat(testData@systemInfo$sessionInfo$R.version$version.string, "\n")
    lcat("Installed packages: ",
         c(testData@systemInfo$sessionInfo$basePkgs,
           names(testData@systemInfo$sessionInfo$otherPkgs)), "\n")

  }

  return(invisible(TRUE))
}




getErrors <- function(obj) {
  ## provided for backwards compatibility

  flag <- isValidTestResult(obj)
  if(!isTRUE(flag)) {
    stop(flag)
  }
  
  count <- countResult(obj)
  ret <- list(nErr=0, nDeactivated=0, nFail=0, nTestFunc=0)
  
  ret$nErr <- as.numeric(count["ERROR"])
  ret$nDeactivated <- as.numeric(count["DEACTIVATED"])
  ret$nFail <- as.numeric(count["FAILURE"])
  ret$nTestFunc <- as.numeric(sum(count))
  return(ret)
}

