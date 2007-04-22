##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003-2006  Thomas Koenig, Matthias Burger, Klaus Juenemann
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

##   ret <- newTestSuite(name, dirs, testFileRegexp, testFuncRegexp,
##                       rngKind, rngNormalKind)
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
  ##@ret              : [TestCaseTestResultData]
  ##
  ##@codestatus : internal
  
  ##  write to stdout for logging


  cat("\n\nExecuting test function",funcName," ... ")

  testCase <- newTestCaseTestResultData(funcName)
  
  ## safe execution of setup function
  res <- try(setUpFunc())
  if (inherits(res, "try-error")) {
    message <- paste("Error executing .setUp before",funcName, ":", geterrmessage())    
    testCase@setUpError <- TRUE
    testCase@errorMsg <- message
    return(testCase)
  }

  
  ## reset book keeping variable
  testCaseCheckCount <<- 0

  # shut up warnings:
  optWarn <- getOption("warn")
  options(warn=-1)
  ##  esure reset to previous default on exit
  on.exit(options(warn=optWarn))
  
  
  warnCount <- 0
  warnMessageStack <- list()
  warnHandler <- function(e) {
    warnCount <<- 1 + warnCount
    warnMessageStack[[warnCount]] <<- conditionMessage(e)
  }
  
  ## ordinary test function execution:
  func <- get(funcName, envir=envir, mode="function")
  timing <- tryCatch(withCallingHandlers(system.time(func()),
                                    warning=warnHandler),
                     error=function(e) e)

  ##if (inherits(timing, "try-error")) {
  if (inherits(timing, "error")) {
    if (inherits(timing, "RUnitFailure")) {
    #if(.testLogger$isFailure()) {
    #  .testLogger$addFailure(testFuncName=funcName,
    #                         failureMsg=geterrmessage())
      testCase@failure=TRUE
      testCase@errorMsg <- paste(timing)
    }
    #else if(.testLogger$isDeactivated()) {
    else if(inherits(timing, "RUnitDeactivated")) {
      #.testLogger$addDeactivated(testFuncName=funcName)
      testCase@deactivated <- TRUE
      testCase@errorMsg <- paste(timing)
    }
    else {
      #.testLogger$addError(testFuncName=funcName,
      #                     errorMsg=geterrmessage())
      testCase@error <- TRUE
      testCase@errorMsg <- paste(timing)
    }
  }
  else {
    #.testLogger$addSuccess(testFuncName=funcName, secs=round(timing[3], 2))
    ##  strip attributes
    testCase@execTime <- as.vector(timing[3])
  }

  ##  add check counter
  testCase@checkNum <- as.integer(testCaseCheckCount)
  ##  add warning stack
  testCase@warnMessageStack <- warnMessageStack
  
  ## safe execution of tearDown function
  res <- try(tearDownFunc())
  if (inherits(res, "try-error")) {
    message <- paste("Error executing .tearDown after",funcName, ":", geterrmessage())
    testCase@tearDownError <- TRUE
    testCase@errorMsg <- paste(res)
    return(testCase)
  }

  cat(" done successfully.\n\n")
  return(testCase)
}


.sourceTestFile <- function(absTestFileName, testFuncRegexp, envir=environment())
{
  ##@bdescr
  ## This function sources a file, finds all the test functions in it, executes them
  ## and reports the results to the TestLogger.
  ## No return value, called for its side effects on TestLogger object
  ##@edescr
  ##
  ##@in absTestFileName : [character] the absolute name of the file to test
  ##@in testFuncRegexp  : [character] a regular expression identifying the names of test functions
  ##@ret                : [SourceFileTestResultData]
  ##
  ##@codestatus : internal

  #browser()
  testResult <- newSourceFileTestResultData()
  testResult@sourceFileName <- absTestFileName
  
  if (!file.exists(absTestFileName)) {
    message <- paste("Test case file ", absTestFileName," not found.")    
    testResult@errorMsg <- message
    testResult@error <- TRUE
    return(testResult)
  }

  ## init counter
  ## has to be done here to be in the parent frame hiearchy of the
  ## of the calling check* functions
  testCaseCheckCount <- 0
  
  ##  record existing object listing
  existingObj <- ls()
  ##  catch syntax errors in test case file
  res <- try(source(absTestFileName, local=TRUE))
  testFunctions <- ls(pattern=testFuncRegexp)
  ##  remove existingObj from vector of test functions in case of match
  testFunctions <- testFunctions[-unlist(sapply(existingObj, function(x) grep(x, testFunctions)))]
  
  if (inherits(res, "try-error")) {
    message <- paste("Error while sourcing ",absTestFileName,":",geterrmessage())
    testResult@errorMsg <- message
    testResult@error <- TRUE
    return(testResult)
  }

  testCaseArray <- newTestCaseTestResultDataArray()
  
  for (ci in  seq(along=testFunctions)) {
    funcName <- testFunctions[ci]
    func <- get(funcName, envir=envir)
    ## anything else than a function is ignored.
    if(mode(func) != "function") {
      ##     cat("\n ", funcName," is not of mode function. skipped.\n")
      next;
    }

    testCaseArray[ci] <- .executeTestCase(funcName, envir=environment(),
                                          setUpFunc=.setUp, tearDownFunc=.tearDown)
    ##  FIXME: this is an redundant entry and should be avoided
    ##  add source file name
    testCaseArray[ci]@sourceFileName <- absTestFileName
  }
  testResult@testCaseResult <- testCaseArray
  return(testResult)
}



runTestSuite <- function(testSuites, useOwnErrorHandler=TRUE) {
  ##@bdescr
  ## This is the main function of the runit framework. It finds all the relevant
  ## test files and triggers all the required action. At the end it creates a test
  ## protocol file. 
  ## IMPORTANT to note, the random number generator is (re-)set to the default
  ## methods specifed in defineTestSuite() before each new test case file is sourced. 
  ## This garantees that each test case can rely
  ## on the default, even if the random number generator version is being reconfigured in some
  ## test case file(s).
  ##@edescr
  ##
  ##@in  testSuites         : [list] list of test suite lists
  ##@in  useOwnErrorHandler : [logical] TRUE (default) : use the runit error handler
  ##@ret                    : [list] 'RUnitTestData' S3 class object
  ##
  ##@codestatus : testing
  
  ##  preconditions
  if (!is.logical(useOwnErrorHandler)) {
    stop(runitError("argument 'useOwnErrorHandler' has to be of type logical."))
  }
  if (length(useOwnErrorHandler) != 1) {
    stop(runitError("argument 'useOwnErrorHandler' has to be of length 1."))
  }
  if (is.na(useOwnErrorHandler)) {
    stop(runitError("argument 'useOwnErrorHandler' may not contain NA."))
  }
  
  
  ##  record RNGkind and reinstantiate on exit
  rngDefault <- RNGkind()
  on.exit(RNGkind(kind=rngDefault[1], normal.kind=rngDefault[2]))
  
  oldErrorHandler <- getOption("error")
  ## reinstall error handler
  on.exit(options(error=oldErrorHandler))
  

  ## main loop
  if(isValidTestSuite(testSuites)) {
    testSuites <- list(testSuites)
  }
  
  testResult <- newTestResultData()
  testResult@name <- "RUnit Test"

  testSuitesResultArray <- newTestSuiteTestResultDataArray()
  
  for (i in seq(length=length(testSuites))) {
    testSuite <- testSuites[[i]]
    testSuitesResultArray[i] <- newTestSuiteTestResultData()
    testSuitesResultArray[i]@name <- testSuite$name

    ##  FIXME: class dispatch issue after defining S4 method 
##     if(!isValidTestSuite(testSuite)) {
##       errMsg <- paste("Invalid test suite",testSuite$name,". Test run aborted.")
##       testSuitesResultArray[i]@error <- TRUE
##       testSuitesResultArray[i]@errorMsg <- errMsg
##       stop(runitError(errMsg))
##     }
    
    testFiles <- list.files(testSuite$dirs,
                            pattern = testSuite$testFileRegexp,
                            full.names=TRUE)
    #for(testFile in testFiles) {
    for(ti in seq(along=testFiles)) {
      ## set a standard random number generator.
      RNGkind(kind=testSuite$rngKind, normal.kind=testSuite$rngNormalKind)
      
       testSuitesResultArray[i]@sourceFileResult[ti] <- .sourceTestFile(testFiles[ti], testSuite$testFuncRegexp)
    }
  }

  testResult@testResultData <-  testSuitesResultArray
  
  return(testResult)
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
