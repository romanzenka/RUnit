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

defineTestSuite <- function(name, dirs, testFileRegexp="^runit.+\\.r$", testFuncRegexp="^test.+")
{
  ##@bdescr
  ##  Convenience functions to handle test suites
  ##@edescr

  ret <- list(name=name,
              dirs=dirs,
              testFileRegexp=testFileRegexp,
              testFuncRegexp=testFuncRegexp)
  class(ret) <- "RUnitTestSuite"
  return(ret)
}


isValidTestSuite <- function(testSuite)
{
  if(!identical(class(testSuite), "RUnitTestSuite"))
  {
    return(FALSE)
  }
  if(!setequal(names(testSuite), c("name", "dirs", "testFileRegexp", "testFuncRegexp")))
  {
    return(FALSE)
  }
  for(i in 1:length(testSuite))
  {
    if(!is.character(testSuite[[i]]))
    {
      return(FALSE)
    }
  }
  return(TRUE)
}

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

  ## safe execution of setup function
  res <- try(setUpFunc())
  if (inherits(res, "try-error")) {
    message <- paste("Error executing .setUp before",funcName, ":", geterrmessage())
    .testLogger$addError(testFuncName=paste(".setUp (before ", funcName, ")", sep=""),
                         errorMsg=message)
    return()
  }

  ## ordinary test function execution:
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

  ## safe execution of tearDown function
  res <- try(tearDownFunc())
  if (inherits(res, "try-error")) {
    message <- paste("Error executing .tearDown after",funcName, ":", geterrmessage())
    .testLogger$addError(testFuncName=paste(".tearDown (after ", funcName, ")", sep=""),
                         errorMsg=message)
    return()
  }

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
    message <- paste("Error while sourcing ",absTestFileName,":",geterrmessage())
    .testLogger$addError(testFuncName=absTestFileName,
                        errorMsg=message)
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
  assign(".testLogger", .newTestLogger(useOwnErrorHandler), envir = .GlobalEnv)

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
      RNGkind(kind="Mersenne-Twister", normal.kind="Inversion")
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
  ts <- defineTestSuite(name=nn, dirs=dn, testFileRegexp=paste("^", fn, "$", sep=""),
                        testFuncRegexp=testFuncRegexp)
  return(runTestSuite(ts))
}
