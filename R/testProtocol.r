printTextProtocol <- function(testData,
                              fileName = "",
                              skipDetails = TRUE,
                              traceBackCutOff=9) {

  ## just a convenience function
  pr <- function(..., sep=" ", nl=TRUE) {
    if(nl) {
      cat(... , "\n", file = fileName, append=TRUE, sep=sep)
    }
    else {
      cat(... , file = fileName, append=TRUE, sep=sep)
    }
  }

  ## get singular or plural right
  sop <- function(number, word, plext="s") ifelse(number == 1, paste(number, word),
                                  paste(number, paste(word, plext, sep="")))


  ## header part
  cat("RUNIT TEST PROTOCOL --", date(), "\n", file = fileName)
  pr("***********************************************")
  if(length(testData) == 0) {
    pr("no test cases :-(")
    return()
  }

  nTestFunc <- 0
  nErr <- 0
  nFail <- 0
  for(i in seq(length=length(testData))) {
    nTestFunc <- nTestFunc + testData[[i]]$nTestFunc
    nErr <- nErr + testData[[i]]$nErr
    nFail <- nFail + testData[[i]]$nFail
  }

  pr("Number of test functions:", nTestFunc)
  pr("Number of errors:", nErr)
  pr("Number of failures:", nFail, "\n\n")



  ## summary of test suites
  pr(sop(length(testData), "Test Suite"), ":")
  for(tsName in names(testData)) {
    pr(tsName, " - ", sop(testData[[tsName]]$nTestFunc, "test function"), ", ",
       sop(testData[[tsName]]$nErr, "error"), ", ",
       sop(testData[[tsName]]$nFail, "failure"), sep="")
    if(testData[[tsName]]$nErr + testData[[tsName]]$nFail > 0) {
      srcFileRes <- testData[[tsName]]$sourceFileResults
      for(i in seq(length=length(srcFileRes))) {
        testFuncNames <- names(srcFileRes[[i]])
        for(j in seq(length=length(testFuncNames))) {
          funcList <- srcFileRes[[i]][[testFuncNames[j]]]
          if(funcList$kind == "error") {
            pr("ERROR in ", testFuncNames[j], ": ", funcList$msg, nl=FALSE, sep="")
          }
          else if(funcList$kind == "failure") {
            pr("FAILURE in ", testFuncNames[j], ": ", funcList$msg,
               sep="", nl=FALSE)
          }
        }
      }
    }
  }


  ## if no details are required, we are done.
  if(skipDetails) return()

  pr("\n\n\nDetails")

  ## loop over all test suites
  for(tsName in names(testData)) {
    tsList <- testData[[tsName]]
    pr("***************************")
    pr("Test Suite:", tsName)
    pr("Test function regexp:", tsList$testFuncRegexp)
    pr("Test file regexp:", tsList$testFileRegexp)
    if(length(tsList$dirs) == 0) {
      pr("No directories !")
    }
    else {
      if(length(tsList$dirs) == 1) {
        pr("Involved directory:")
      }
      else {
        pr("Involved directories:")
      }
      for(dir in tsList$dirs) {
        pr(dir)
      }
      res <- tsList$sourceFileResults
      testFileNames <- names(res)
      if(length(res) == 0) {
        pr("no test files")
      }
      else {
        ## loop over all source files
        for(testFileName in testFileNames) {
          pr("---------------------------")
          pr("Test file:", testFileName)
          testFuncNames <- names(res[[testFileName]])
          if(length(testFuncNames) == 0) {
            pr("no test functions")
          }
          else {
            ## loop over all test functions in the test file
            for(testFuncName in testFuncNames) {
              testFuncInfo <- res[[testFileName]][[testFuncName]]
              if(testFuncInfo$kind == "success") {
                pr(testFuncName, ":", " ... OK (", testFuncInfo$time, " seconds)", sep="")
              }
              else {
                if(testFuncInfo$kind == "error") {
                  pr(testFuncName, ": ERROR !! ", sep="")
                }
                else if (testFuncInfo$kind == "failure") {
                  pr(testFuncName, ": FAILURE !! (check number ", testFuncInfo$checkNo, ")", sep="")
                }
                else {
                  pr(testFuncName, ": unknown error kind", sep="")
                }
                pr(testFuncInfo$msg, nl=FALSE)
                if(length(testFuncInfo$traceBack) > 0) {
                  pr("   Call Stack:")
                  for(i in traceBackCutOff:length(testFuncInfo$traceBack)) {
                    pr("   ", 1+i-traceBackCutOff, ": ", testFuncInfo$traceBack[i], sep="")
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
