######################################################################
##
## library: EpiR.tools
## =====================================
##
## runit.r
## =====================================
## very simply framework for unit tests, strongly inspired by Javas junit
## It has 3 components:
## - The TestLogger object for storing information and creating the test protocol
## - A set of functions for finding and executing the test functions
## - A set of functions to be used inside the test cases to test something
##
## Dependencies
## =====================================
## absolutely nothing :-)
##
## Version
## =====================================
##  $Id$
##
## Copyright (c) 2003  Epigenomics AG Berlin - All rights reserved
## THIS IS PROPRIETARY SOURCE CODE of Epigenomics AG Berlin
##
######################################################################


## Convenience functions to handle test suites
defineTestSuite <- function(name, dirs, testFileRegexp="^runit.+\.r$", testFuncRegexp="^test.+") {
  return(list(name=name,
              dirs=dirs,
              testFileRegexp=testFileRegexp,
              testFuncRegexp=testFuncRegexp))
}


isValidTestSuite <- function(ts) {
  if(!setequal(names(ts), c("name", "dirs", "testFileRegexp", "testFuncRegexp"))) {
    return(FALSE)
  }
  for(i in 1:length(ts)) {
    if(!is.character(ts[[i]])) {
      return(FALSE)
    }
  }
  return(TRUE)
}





## -------- SECOND COMPONENT ----------------
## The next couple of functions are responsible
## for finding and executing the test functions.

.setUp <- function() {
  ##@bdescr
  ##  Internal Function.
  ##  Default function to be executed once for each test case before the test case gets executed.
  ##  This function can be adopted to specific package requirements for a given project.
  ##  Need to replace this default with a new function definition.
  ##  Function cannot take arguments and does not have a return value.
  ##@edescr

  return()
}


.tearDown <- function() {
  ##@bdescr
  ##  Internal Function.
  ##  Default function to be executed once for each test case after the test case got executed.
  ##  This function can be adopted to specific package requirements for a given project.
  ##  Need to replace this default with a new function definition.
  ##  Function cannot take arguments and does not have a return value.
  ##@edescr

  return()
}



.executeTestCase <- function(funcName, envir, setUpFunc, tearDownFunc)
{
  ##@bdescr
  ##  Internal Function.
  ##  Execute individual test case, record logs and change state of global TestLogger object.
  ##@edescr
  ##@in  funcName : [character] name of test case function

  func <- get(funcName, envir=envir)
  ## anything else than a function is ignored.
  if(mode(func) != "function") {
    return()
  }


  ##  sanity check:
  ##  exclude functions where in the function body 'runit' is called
  ##  this causes recursions or argument mismatch errors
  ##  and breaks the execution loop
  ##  body returns a 'call' expression, we need a character string
  funcBody <- deparse(body(func))
  findIdx <- grep("runit\\(", funcBody)
  if (length(findIdx) > 0) {
    .testLogger$addError(testFuncName=funcName,
                        msg="test function body contains call to 'runit'. Not evaluated.\n")
    return()
  }

  ## reset book keeping variables in .testLogger
  .testLogger$isFailure <<- FALSE
  .testLogger$checkNo <<- 0

  ## ordinary test function execution:
  setUpFunc()
  timing <- try(system.time(func()))
  if (inherits(timing, "try-error")) {
    if(.testLogger$isFailure) {
      .testLogger$addFailure(testFuncName=funcName,
                             failureMsg=geterrmessage(),
                             checkNum = .testLogger$checkNo)
    }
    else {
      .testLogger$addError(testFuncName=funcName, errorMsg=geterrmessage())
    }
  }
  else {
    .testLogger$addSuccess(testFuncName=funcName, secs=round(timing[3], 2))
  }
  tearDownFunc()
  return()
}

.sourceTestFile <- function(absTestFileName, testFuncRegexp)
{
  ##@bdescr
  ## This function sources a file, finds all the test functions in it, executes them
  ## and reports the results to the TestLogger.
  ## No return value, called for its side effects on TestLogger object
  ##@edescr
  ##
  ##@in absTestFileName : [character] the absolute name of the file to test
  ##@in testFuncRegexp : [character] a regular expression identifying the names of test functions

  .testLogger$setCurrentSourceFile(absTestFileName)
  if (!file.exists(absTestFileName)) {
    message <- paste("Test case file ", absTestFileName," not found.")
    .testLogger$addError(testFuncName=absTestFileName,
                        errorMsg=message)
    return()
  }

  ##  catch syntax errors in test case file
  res <- try(source(absTestFileName, local=TRUE))
  if (inherits(res, "try-error")) {
    message <- paste("Syntax error in ",absTestFileName,":",geterrmessage())
    .testLogger$addError(funcName=absTestFileName,
                        msg=message)
    return()
  }

  testFunctions <- ls(pattern=testFuncRegexp)
  for (funcName in testFunctions) {
    .executeTestCase(funcName, envir=environment(), setUpFunc=.setUp, tearDownFunc=.tearDown)
  }
}




runTestSuite <- function(testSuites, useOwnErrorHandler=TRUE) {
  ##@bdescr
  ## This is the main function of the runit framework. It finds all the relevant
  ## test files and triggers all the required action. At the end it creates a test
  ## protocol file. Very importantly, the random number generator is set to standard
  ## method before each file is sourced. This garantees that each test case can rely
  ## on the default, even if the random number generator is being reconfigured in some
  ## test case.
  ##@edescr
  ##
  ##@in  testSuites     : [list] list of test suite lists
  ##@ret                :

  ##  record work space objects before test case execution
  lsObjects <- ls(envir = .GlobalEnv, all.names=TRUE)

  oldErrorHandler <- getOption("error")
  ## initialize TestLogger
  assign(".testLogger", newTestLogger(useOwnErrorHandler), envir = .GlobalEnv)

  ## main loop
  if(isValidTestSuite(testSuites)) {
    testSuites <- list(testSuites)
  }
  for (i in seq(length=length(testSuites))) {
    testSuite <- testSuites[[i]]
    if(!isValidTestSuite(testSuite)) {
      stop("Invalid test suite. Test run aborted.")
    }
    .testLogger$setCurrentTestSuite(testSuite)
    testFiles <- list.files(testSuite$dirs,
                            pattern = testSuite$testFileRegexp,
                            full.names=TRUE)
    for(testFile in testFiles) {
      ## set a standard random number generator.
      RNGkind(kind="Marsaglia-Multicarry", normal.kind="Kinderman-Ramage")
      .sourceTestFile(testFile, testSuite$testFuncRegexp)
    }
  }

  ret <- .testLogger$getTestData()

  ## clean up: remove objects that have been created during the test runs
  lsCurrentObjects <- ls(envir = .GlobalEnv, all.names=TRUE)
  ##  start in the local environment and then progressively search all frame
  ##  till the object is found
  rm(list=lsCurrentObjects[!(lsCurrentObjects %in% lsObjects)], inherits=TRUE)
  ## reinstall error handler
  options(error=oldErrorHandler)
  return(ret)
}

runTestFile <- function(absFileName, useOwnErrorHandler=TRUE, testFuncRegexp="^test.+") {
  fn <- basename(absFileName)
  nn <- strsplit(fn, "\\.")[[1]][1]
  dn <- dirname(absFileName)
  ts <- defineTestSuite(name=nn, dirs=dn, testFileRegexp=fn, testFuncRegexp=testFuncRegexp)
  return(runTestSuite(ts))
}
