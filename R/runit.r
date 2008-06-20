##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003-2008  Thomas Koenig, Matthias Burger, Klaus Juenemann
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




runTestSuite <- function(testSuites) {
  ##@bdescr
  ## This is the main function of the runit framework. It finds all the relevant
  ## test files and triggers all the required actions. At the end it creates an
  ## object of type 'TestResult' containing all the data collected during the 
  ## test run. From this object a test protocol can be generated.
  ## IMPORTANT to note, the random number generator is (re-)set to the default
  ## methods specifed in defineTestSuite() before each new test file is sourced. 
  ## This garantees that each new test case set defined together in on file can rely
  ## on the default, even if the random number generator version is being
  ## reconfigured in some previous test file(s).
  ##@edescr
  ##
  ##@in  testSuites : [list | TestSuite] single or list of TestSuite object(s)
  ##@ret            : [TestResult] 'TestResult' S4 class object
  
  ##  record RNGkind and reinstantiate on exit
  rngDefault <- RNGkind()
  on.exit(RNGkind(kind=rngDefault[1], normal.kind=rngDefault[2]))

  assign(".runit_running", TRUE, envir=.GlobalEnv)
  on.exit(rm(".runit_running", envir=.GlobalEnv))
  ## create list if single TestSuite object has been passed
  if(is(testSuites, "TestSuite")) {
    testSuites <- list(testSuites)
  }

  ## initialise result object
  testResult = new("TestResult")
  
  ## main loop executes one test suite after the other
  ## (might need to change in case of parallel execution)
  for (i in seq_along(testSuites)) {
    testSuite <- testSuites[[i]]
    ## FIXME: is this check necessary? 
    if(!isValidTestSuite(testSuite)) {
      errMsg <- paste("Invalid test suite",testSuite@name,". Test run aborted.")
      stop(errMsg)
    }

    ## initialise TestSuiteResult object
    testSuiteResult <- new("TestSuiteResult", testSuite=testSuite,
                           execStart=Sys.time())
    
    ## find test files, loop over and run them
    testFiles <- list.files(testSuite@dirs,
                            pattern = testSuite@testFileRegexp,
                            full.names=TRUE)
    for(testFile in testFiles) {
      ## set a standard random number generator.
      RNGkind(kind=testSuite@rngKind, normal.kind=testSuite@rngNormalKind)
      ## source and run test file, add result to current test suite result
      testSuiteResult@testFileData[[testFile]] <- 
        .sourceTestFile(testFile, testSuite@testFuncRegexp)
    }

    testSuiteResult@execStop <- Sys.time()
    ## add TestSuiteResult object to overall result object
    testResult@testSuiteData[[testSuite@name]] <- testSuiteResult
    
  } # end of loop over test suites

  ## FIXME: remove next if-block before release
  resultState <- isValidTestResult(testResult)
  if(!isTRUE(resultState)) {
    stop(paste("Internal RUnit problem, invalid TestResult object produced:",
               resultState, sep="\n"))
  }
  return(testResult)
}


runTestFile <- function(absFileName, 
                        testFuncRegexp="^test.+",
                        rngKind="Marsaglia-Multicarry",
                        rngNormalKind="Kinderman-Ramage") {
  ##@bdescr
  ##  Convenience function: creates and runs test suite made from just one file
  ##@edescr
  ##
  ##@in  absFileName : [character] complete file name of test file
  ##@ret             : [TestResult] 'TestResult' S4 class object

  
  ##  all error checking and hanling is delegated to function runTestSuite
  fn <- basename(absFileName)
  nn <- strsplit(fn, "\\.")[[1]][1]
  dn <- dirname(absFileName)
  ts <- defineTestSuite(name=nn, dirs=dn, 
                        testFileRegexp=paste("^", fn, "$", sep=""),
                        testFuncRegexp=testFuncRegexp,
                        rngKind=rngKind,
                        rngNormalKind=rngNormalKind)
                        
  return(runTestSuite(ts))
}


## --------------------------------------------------------------
## Internal helper functions (invisible outside RUnit namespace)
## --------------------------------------------------------------

.sourceTestFile <- function(absTestFileName, testFuncRegexp)
{
  ##@bdescr
  ## This function sources a file in a sandbox environment,
  ## finds all the test functions in it, executes them
  ## and stores the results in an object of type 'TestFileResult'.
  ##@edescr
  ##
  ##@in absTestFileName : [character] the absolute name of the file to test
  ##@in testFuncRegexp  : [character] regexp identifying names of test functions
  ##@ret                : [TestFileResult] object containing collected test data

  ## initialise result object
  testFileResult <- new("TestFileResult", testFileName=absTestFileName,
                        sourceState="OK",
                        execStart=Sys.time())

  ## create separate sandbox environment for test functions execution
  ## (which is destroyed after this function is left)
  sandbox <- new.env(parent=.GlobalEnv)
  
  ## try to source the file, catch and report error in case of failure
  res <- try(sys.source(absTestFileName, envir=sandbox))
  if (inherits(res, "try-error")) {
    testFileResult@sourceState <- geterrmessage()
  } else {  # sourcing of test file successfull ...
    
    ## check if test file provides definition of .setUp/.tearDown
    if (exists(".setUp", envir=sandbox, inherits=FALSE)) {
      .setUp <- get(".setUp", envir=sandbox)
    }
    if (exists(".tearDown", envir=sandbox, inherits=FALSE)) {
      .tearDown <- get(".tearDown", envir=sandbox)
    }

    ## Find test functions, loop over and run them
    testFunctions <- ls(pattern=testFuncRegexp, envir=sandbox)
    for (funcName in testFunctions) {
      testFileResult@testFunctionData[[funcName]] <-
        .executeTestFunc(funcName, envir=sandbox,
                         setUpFunc=.setUp, tearDownFunc=.tearDown)
    }
  }
  testFileResult@execStop <- Sys.time()
  return(testFileResult)
}


.executeTestFunc <- function(funcName, envir, setUpFunc, tearDownFunc)
{
  ##@bdescr
  ##  Execute individual test function, record logs and change state of global TestLogger object.
  ##@edescr
  ##
  ##@in  funcName     : [character] name of test function
  ##@in  envir        : [environment] env where test function is executed
  ##@in  setUpFunc    : [function] to be executed before each test function
  ##@in  tearDownFunc : [function] to be executed before each test function
  ##@ret              : [TestFunctionResult] data collected during test run


  ## put the test function into a mangled symbol in order
  ## to be able to identify the call in a traceback
  runit_test_func__  <- get(funcName, envir=envir)
  ## anything else than a function is ignored.
  if(mode(runit_test_func__) != "function") {
    return(NULL)
  }

  ## FIXME: it should be possible to shut this up
  ## FIXME: what about a proper progress reporter?
  cat("\n\nExecuting test function",funcName," ... ")

  ## Initilize result object
  testFuncResult <- new("TestFunctionResult",
                        testFunctionName = funcName,
                        state="OK",
                        numChecks=0,
                        numWarns=0)
  
  ## Definition of handler functions
  checkCounter <- 0
  checkCountHandler <- function(e) {
    checkCounter <<- 1 + checkCounter
  }

  ## FIXME: not only count warnings, but optionally also store warning messages
  warnCounter <- 0
  warnHandler <- function(e) {
    warnCounter <<- 1 + warnCounter
     return(e)
  }

  traceRecorder <- function(e) {
    tb <- as.character(sys.calls())
    ltr <- length(tb)
    if(ltr > 0) {
      cutoff <- grep("^runit_test_func__", tb)
      if( (length(cutoff) != 1) || cutoff > (ltr-2)) {
        warning("Internal runit problem: cannot make sense from traceback")
      } else {
        testFuncResult@traceBack <<- tb[-c(1:cutoff, ltr-1,ltr)]
      }
    }
    return(e)
  }

  deactHandler <- function(e) {
    testFuncResult@state <<- "DEACTIVATED"
    testFuncResult@msg <<- e$msg
    return(e)
  }

  
  failureHandler <- function(e) {
    testFuncResult@state <<- "FAILURE"
    testFuncResult@msg <<- e$msg
    testFuncResult@traceBack <<- e$checkCall
    return(e)
  }

  errorHandler <- function(e) {
    testFuncResult@state <<- "ERROR"
    testFuncResult@msg <<- geterrmessage()  
    return(e)
  }
  
  ## Safe execution of setup function
  res <- try(setUpFunc())
  if (inherits(res, "try-error")) {
    testFuncResult@state <- "ERROR"
    testFuncResult@msg <- paste(".setUp (before ", funcName, ") failed: ",
                                geterrmessage(), sep="")
    return(testFuncResult)
  }

  ## Execution of the test function
  execStart <- proc.time()
  tryCatch(withCallingHandlers(system.time(runit_test_func__()),
                               checkCountSignal=checkCountHandler,
                               warning=warnHandler,
                               error=traceRecorder),
           deactSignal=deactHandler,
           failureSignal=failureHandler,
           error=errorHandler)

  testFuncResult@execTime <- (proc.time() - execStart)[3]
  testFuncResult@numChecks <- checkCounter
  testFuncResult@numWarns <- warnCounter

  
  ## When tearDown fails, a possible error in the actual test function 
  ## is overwritten! Is this really a good behaviour?
  res <- try(tearDownFunc())
  if (inherits(res, "try-error")) {
    testFuncResult@state <- "ERROR"
    testFuncResult@msg <- paste(".tearDown (after ", funcName, ") failed:",
                                geterrmessage(), sep="")
    return(testFuncResult)
  }
  
  cat(" done successfully.\n\n")
  return(testFuncResult)
}



.setUp <- function() {
  ##@bdescr
  ## Internal Function.
  ## Default function to be executed once for each test case BEFORE the test
  ## case gets executed.  This function can be adopted to specific package
  ## requirements for a given project. Need to replace this default with a
  ## new function definition. Function cannot take arguments and does not
  ## have a return value.
  ##@edescr

  return()
}


.tearDown <- function() {
  ##@bdescr
  ## Similar to .setUp but is executed AFTER the test case gets executed
  ##@edescr
   
  return()
}



