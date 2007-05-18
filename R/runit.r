##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003-2007  Thomas Koenig, Matthias Burger, Klaus Juenemann
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

defineTestSuite <- function(name, dirs, 
                            testFileRegexp="^runit.+\\.[rR]$",
                            testFuncRegexp="^test.+",
                            rngKind="Marsaglia-Multicarry",
                            rngNormalKind="Kinderman-Ramage")
{
  ##@bdescr
  ##  Convenience functions to handle test suites
  ##@edescr
  ##
  ##@in  name           : [character] test suite title used in protocol
  ##@in  dirs           : [character] vector of paths to search for test case files
  ##@in  testFileRegexp : [character] regular expression string to match file names
  ##@in  testFuncRegexp : [character] regular expression string to match test case functions within all test case files
  ##@in  rngKind        : [character] name of the RNG version, see RNGversion()
  ##@in  rngNormalKind  : [character] name of the RNG version for the rnorm, see RNGversion()
  ##@ret                : [RUnitTestSuite] S3 class (list) object, ready for test runner
  ##
  ##@codestatus : testing
  
  ret <- list(name=name,
              dirs=dirs,
              testFileRegexp=testFileRegexp,
              testFuncRegexp=testFuncRegexp,
              rngKind=rngKind,
              rngNormalKind=rngNormalKind)

  class(ret) <- "RUnitTestSuite"
  return(ret)
}


isValidTestSuite <- function(testSuite)
{
  ##@bdescr
  ##  Helper function
  ##  checks 'RUnitTestSuite' class object features
  ##@edescr
  ##
  ##@in   testSuite : [RUnitTestSuite] S3 class (list) object, input object for test runner
  ##@ret            : [logical] TRUE if testSuite is valid
  ##
  ##@codestatus : testing
  
  if(!identical(class(testSuite), "RUnitTestSuite"))
  {
    warning(paste("'testSuite' object is not of class 'RUnitTestSuite'."))
    return(FALSE)
  }
  if(!setequal(names(testSuite), c("name", "dirs", "testFileRegexp", "testFuncRegexp",
                                   "rngKind", "rngNormalKind")))
  {
    warning("'testSuite' object does not conform to S3 class definition.")
    return(FALSE)
  }
  for(i in seq(along=testSuite))
  {
    if(!is.character(testSuite[[i]]))
    {
      warning(paste("'testSuite' object does not conform to S3 class definition.\n",
                    names(testSuite)[i],"has to be of type 'character'."))
      return(FALSE)
    }
  }
  if (!all(file.exists(testSuite[["dirs"]])))
  {
    warning(paste("specifed directory",testSuite[["dirs"]],"not found."))
    return(FALSE)
  }
  
  ##  RNGkind has an internal list of valid names which cannot be accessed
  ##  programatically. Furthermore, users can define their own RNG and select that one
  ##  so we have to leave it to RNGkind() to check if the arguments are valid.
  if (length(testSuite[["rngKind"]]) != 1) {
    warning(paste("specifed 'rngKind' may only contain exatly one name."))
    return(FALSE)
  }
  if (length(testSuite[["rngNormalKind"]]) != 1) {
    warning(paste("specifed 'rngNormalKind' may only contain exatly one name."))
    return(FALSE)
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
  ##
  ##@codestatus : internal
  
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
  ##
  ##@codestatus : internal
  
  return()
}


.executeTestCase <- function(funcName, envir, setUpFunc, tearDownFunc)
{
  ##@bdescr
  ##  Internal Function.
  ##  Execute individual test case, record logs and change state of global TestLogger object.
  ##@edescr
  ##
  ##@in  funcName     : [character] name of test case function
  ##@in  envir        : [environment]
  ##@in  setUpFunc    : [function]
  ##@in  tearDownFunc : [function]
  ##@ret              : [NULL]
  ##
  ##@codestatus : internal
  
  ##  write to stdout for logging

  func <- get(funcName, envir=envir)
  ## anything else than a function is ignored.
  if(mode(func) != "function") {
    return()
  }

  cat("\n\nExecuting test function",funcName," ... ")

  ## safe execution of setup function
  res <- try(setUpFunc())
  if (inherits(res, "try-error")) {
    message <- paste("Error executing .setUp before",funcName, ":", geterrmessage())
    .testLogger$addError(testFuncName=paste(".setUp (before ", funcName, ")", sep=""),
                         errorMsg=message)
    return()
  }

  ## reset book keeping variables in .testLogger
  .testLogger$cleanup()
  ## ordinary test function execution:
  timing <- try(system.time(func()))
  if (inherits(timing, "try-error")) {
    if(.testLogger$isFailure()) {
      .testLogger$addFailure(testFuncName=funcName,
                             failureMsg=geterrmessage())
    }
    else if(.testLogger$isDeactivated()) {
      .testLogger$addDeactivated(testFuncName=funcName)
    }
    else {
      .testLogger$addError(testFuncName=funcName,
                           errorMsg=geterrmessage())
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

  cat(" done successfully.\n\n")
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
  ##
  ##@codestatus : internal

  .testLogger$setCurrentSourceFile(absTestFileName)
  if (!file.exists(absTestFileName)) {
    message <- paste("Test case file ", absTestFileName," not found.")
    .testLogger$addError(testFuncName=absTestFileName,
                        errorMsg=message)
    return()
  }
  

  sandbox <- new.env(parent=.GlobalEnv)
  ##  will be destroyed after function closure is left
  
  ##  catch syntax errors in test case file
  res <- try(sys.source(absTestFileName, envir=sandbox))
  if (inherits(res, "try-error")) {
    message <- paste("Error while sourcing ",absTestFileName,":",geterrmessage())
    .testLogger$addError(testFuncName=absTestFileName,
                        errorMsg=message)
    return()
  }
  ##  test file provides definition of .setUp/.tearDown
  if (exists(".setUp", envir=sandbox, inherits=FALSE)) {
    .setUp <- get(".setUp", envir=sandbox)
  }
  if (exists(".tearDown", envir=sandbox, inherits=FALSE)) {
    .tearDown <- get(".tearDown", envir=sandbox)
  }
  testFunctions <- ls(pattern=testFuncRegexp, envir=sandbox)
  for (funcName in testFunctions) {
    .executeTestCase(funcName, envir=sandbox, setUpFunc=.setUp, tearDownFunc=.tearDown)
  }
  
}


runTestSuite <- function(testSuites, useOwnErrorHandler=TRUE) {
  ##@bdescr
  ## This is the main function of the runit framework. It finds all the relevant
  ## test files and triggers all the required actions. At the end it creates a test
  ## protocol data object. 
  ## IMPORTANT to note, the random number generator is (re-)set to the default
  ## methods specifed in defineTestSuite() before each new test case file is sourced. 
  ## This garantees that each new test case set defined together in on file can rely
  ## on the default, even if the random number generator version is being reconfigured in some
  ## previous test case file(s).
  ##@edescr
  ##
  ##@in  testSuites         : [list] list of test suite lists
  ##@in  useOwnErrorHandler : [logical] TRUE (default) : use the runit error handler
  ##@ret                    : [list] 'RUnitTestData' S3 class object
  ##
  ##@codestatus : testing
  
  ##  preconditions
  if (!is.logical(useOwnErrorHandler)) {
    stop("argument 'useOwnErrorHandler' has to be of type logical.")
  }
  if (length(useOwnErrorHandler) != 1) {
    stop("argument 'useOwnErrorHandler' has to be of length 1.")
  }
  if (is.na(useOwnErrorHandler)) {
    stop("argument 'useOwnErrorHandler' may not contain NA.")
  }
  
  
  ##  record RNGkind and reinstantiate on exit
  rngDefault <- RNGkind()
  on.exit(RNGkind(kind=rngDefault[1], normal.kind=rngDefault[2]))
  
  oldErrorHandler <- getOption("error")
  ## reinstall error handler
  on.exit(options(error=oldErrorHandler), add=TRUE)
  
  ## initialize TestLogger
  assign(".testLogger", .newTestLogger(useOwnErrorHandler), envir = .GlobalEnv)

  ## main loop
  if(isValidTestSuite(testSuites)) {
    testSuites <- list(testSuites)
  }
  for (i in seq(length=length(testSuites))) {
    testSuite <- testSuites[[i]]
    if(!isValidTestSuite(testSuite)) {
      errMsg <- paste("Invalid test suite",testSuite$name,". Test run aborted.")
      stop(errMsg)
    }
    .testLogger$setCurrentTestSuite(testSuite)
    testFiles <- list.files(testSuite$dirs,
                            pattern = testSuite$testFileRegexp,
                            full.names=TRUE)
    for(testFile in testFiles) {
      ## set a standard random number generator.
      RNGkind(kind=testSuite$rngKind, normal.kind=testSuite$rngNormalKind)
      
      .sourceTestFile(testFile, testSuite$testFuncRegexp)
    }
  }

  ret <- .testLogger$getTestData()
  
  return(ret)
}


runTestFile <- function(absFileName, useOwnErrorHandler=TRUE, 
                        testFuncRegexp="^test.+",
                        rngKind="Marsaglia-Multicarry",
                        rngNormalKind="Kinderman-Ramage") {
  ##@bdescr
  ##  Convenience function.
  ##@edescr
  ##
  ##@in  absFileName        : [character] complete file name of test cases code file
  ##@in  useOwnErrorHandler : [logical] if TRUE RUnits error handler will be used
  ##@in  testFuncRegexp     : [character]
  ##@ret                    : [list] 'RUnitTestData' S3 class object
  ##
  ##@codestatus : testing
  
  ##  preconditions
  ##  all error checking and hanling is delegated to function runTestSuite
  
  fn <- basename(absFileName)
  nn <- strsplit(fn, "\\.")[[1]][1]
  dn <- dirname(absFileName)
  ts <- defineTestSuite(name=nn, dirs=dn, 
                        testFileRegexp=paste("^", fn, "$", sep=""),
                        testFuncRegexp=testFuncRegexp,
                        rngKind=rngKind,
                        rngNormalKind=rngNormalKind)
                        
  return(runTestSuite(ts, useOwnErrorHandler=useOwnErrorHandler))
}
