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

printHTMLProtocol <- function(testData,
                              fileName = "",
                              separateFailureList = TRUE,
                              traceBackCutOff=9) {

  ##@bdescr
  ##  Report generator
  ##  Extracts the log information stored in the 'RUnitTestData' test run object
  ##  and generates a well formated output.
  ##@edescr
  ##
  ##@in  testData            : [RUnitTestData] S3 class object
  ##@in  fileName            : [character]
  ##@in  separateFailureList : [logical] if TRUE (default) add a list of all failures
  ##@in  traceBackCutOff     : [integer] number of steps back in the trace back stack to be displayed

  ## --------------------------------
  ##  CHECK OF INPUT DATA
  ## --------------------------------
  if (!is(testData, "RUnitTestData"))
  {
    stop("Argument 'testData' must be of class 'RUnitTestData'.")
  }

  if (!is.character(fileName))
  {
    stop("Argument 'fileName' has to be of type character.")
  }
  if (length(fileName) != 1)
  {
    stop("Argument 'fileName' must contain exactly one element.")
  }

  if (!is.logical(separateFailureList))
  {
    stop("Argument 'separateFailureList' has to be of type logical.")
  }
  if (length(separateFailureList) != 1)
  {
    stop("Argument 'separateFailureList' must contain exactly one element.")
  }

  if (!is.numeric(traceBackCutOff))
  {
    stop("Argument 'traceBackCutOff' has to be of type logical.")
  }
  if (length(traceBackCutOff) != 1)
  {
    stop("Argument 'traceBackCutOff' must contain exactly one element.")
  }
  if (traceBackCutOff < 0 || traceBackCutOff > 100)
  {
    stop("Argument 'traceBackCutOff' out of valid range [0, 100].")
  }

  ## --------------------------------
  ## HELPER FUNCTIONS
  ## --------------------------------

  ## get singular or plural right
  sop <- function(number, word, plext="s")
  {
    ifelse(number == 1, paste(number, word),
           paste(number, paste(word, plext, sep="")))
  }

  pr <- function(...) {
    writeRaw(paste(...), htmlFile=fileName)
    writeRaw("<br/>", htmlFile=fileName)
  }


  writeP <- function(string, para="") {
    writeBeginTag("p", para=para, htmlFile=fileName)
    writeRaw(string, htmlFile=fileName)
    writeEndTag("p", htmlFile=fileName)
    writeCR(htmlFile=fileName)
  }
  writeLi <- function(..., para="") {
    writeBeginTag("li", para=para, htmlFile=fileName)
    writeRaw(paste(...), htmlFile=fileName)
    writeEndTag("li", htmlFile=fileName)
  }
  createTestFuncRef <- function(testSuite, srcFileName, testFuncName,
                                asAnchor=FALSE) {
    tmp <- paste(testSuite, srcFileName, testFuncName, sep="_")
    if(asAnchor) {
      return(paste("#", gsub("\/", "_", tmp), sep=""))
    }
    else {
      return(gsub("\/", "_", tmp))
    }
  }

  printTraceBack <- function(traceBack) {
    if(length(traceBack) > 0) {
      writeRaw("Call Stack:<br/>", htmlFile=fileName)

      if(traceBackCutOff > length(testFuncInfo$traceBack)) {
        writeRaw("(traceBackCutOff argument larger than length of trace back: full trace back printed)<br/>", htmlFile=fileName)
        writeBeginTag("ol", htmlFile=fileName)
        for(i in 1:length(traceBack)) {
          writeBeginTag("li", htmlFile=fileName)
          writeRaw(traceBack[i], htmlFile=fileName)
          writeEndTag("li", htmlFile=fileName)
        }
      }
      else {
        writeBeginTag("ol", htmlFile=fileName)
        for(i in traceBackCutOff:length(traceBack)) {
          writeBeginTag("li", htmlFile=fileName)
          writeRaw(traceBack[i], htmlFile=fileName)
          writeEndTag("li", htmlFile=fileName)
        }
      }
      writeEndTag("ol", htmlFile=fileName)
    }
  }

  errorStyle <- "color:red"
  deactivatedStyle <- "color:black"


  ## --------------------------------------------
  ## PART 1: TITLE AND BASIC ERROR INFORMATION
  ## --------------------------------------------

  ## title
  title <- paste("RUNIT TEST PROTOCOL", date(), sep="--")
  writeHtmlHeader(title, htmlFile=fileName)
  writeHtmlSection(title, 1, htmlFile=fileName)

  if(length(testData) == 0) {
    writeP("no test cases :-(")
    return()
  }
  ## basic Info
  errInfo <- getErrors(testData)
  writeP(paste("Number of test functions:", errInfo$nTestFunc))
  if(errInfo$nDeactivated > 0) {
    writeP(paste("Number of deactivated test functions:", errInfo$nDeactivated))
  }
  writeP(paste("Number of errors:", errInfo$nErr),
         para=ifelse(errInfo$nErr == 0, "", paste("style", errorStyle, sep="=")))
  writeP(paste("Number of failures:", errInfo$nFail),
         para=ifelse(errInfo$nFail == 0, "", paste("style", errorStyle, sep="=")))
  writeHtmlSep(htmlFile=fileName)

  ## --------------------------------
  ## PART 2: TABLE OF TEST SUITES
  ## --------------------------------

  ## summary of test suites
  writeHtmlSection(sop(length(testData), "Test suite"), 3, htmlFile=fileName)
  ## table of test suites
  if(errInfo$nDeactivated > 0) {
    writeBeginTable(c("Name", "Test functions", "Deactivated", "Errors", "Failures"),
                    width="80%",
                    htmlFile=fileName,
                    columnWidth=c("20%", "20%", "20%", "20%", "20%"))
    for(tsName in names(testData)) {
      rowString <- c(paste("<a href=\"#", tsName, "\">", tsName, "</a>", sep=""),
                     testData[[tsName]]$nTestFunc,
                     testData[[tsName]]$nDeactivated,
                     testData[[tsName]]$nErr,
                     testData[[tsName]]$nFail)
      rowCols <- c("", "",
                   ifelse(testData[[tsName]]$nDeactivated==0, "", "yellow"),
                   ifelse(testData[[tsName]]$nErr==0, "", "red"),
                   ifelse(testData[[tsName]]$nFail==0, "", "red"))

      writeTableRow(row=rowString, bgcolor=rowCols, htmlFile=fileName)
    }
    writeEndTable(htmlFile=fileName)
  }
  else {  ## skip 'deactivated' column if no functions have been deactivated
    writeBeginTable(c("Name", "Test functions", "Errors", "Failures"),
                    width="60%",
                    htmlFile=fileName,
                    columnWidth=c("30%", "30%", "20%", "20%"))
    for(tsName in names(testData)) {
      rowString <- c(paste("<a href=\"#", tsName, "\">", tsName, "</a>", sep=""),
                     testData[[tsName]]$nTestFunc,
                     testData[[tsName]]$nErr,
                     testData[[tsName]]$nFail)
      rowCols <- c("", "",
                   ifelse(testData[[tsName]]$nErr==0, "", "red"),
                   ifelse(testData[[tsName]]$nFail==0, "", "red"))

      writeTableRow(row=rowString, bgcolor=rowCols, htmlFile=fileName)
    }
    writeEndTable(htmlFile=fileName)
  }
  writeHtmlSep(htmlFile=fileName)

  ## ------------------------------------------------
  ## PART 3: ERROR, FAILURE AND DEACTIVATED TABLES
  ## -------------------------------------------------

  ## error table
  if(separateFailureList && (errInfo$nErr > 0)) {
    writeHtmlSection("Errors", 3, htmlFile=fileName)
    writeBeginTable(c("Test suite : test function", "message"),
                    htmlFile=fileName,
                    columnWidth=c("30%", "70%"))
    for(tsName in names(testData)) {
      if(testData[[tsName]]$nErr > 0) {
        srcFileRes <- testData[[tsName]]$sourceFileResults
        srcFileNames <- names(srcFileRes)
        for(i in seq(length=length(srcFileRes))) {
          testFuncNames <- names(srcFileRes[[i]])
          for(j in seq(length=length(testFuncNames))) {
            funcList <- srcFileRes[[i]][[testFuncNames[j]]]
            if(funcList$kind == "error") {
              lnk <- paste("<a href=\"",
                           createTestFuncRef(tsName, srcFileNames[i],
                                             testFuncNames[j], asAnchor=TRUE),
                           "\">",
                           paste(tsName, testFuncNames[j], sep=" : "),
                           "</a>", sep="")
              writeTableRow(row=c(lnk, funcList$msg),
                            htmlFile=fileName)
            }
          }
        }
      }
    }
    writeEndTable(htmlFile=fileName)
    writeHtmlSep(htmlFile=fileName)
  }

  ## failure table
  if(separateFailureList && (errInfo$nFail > 0)) {
    writeHtmlSection("Failures", 3, htmlFile=fileName)
    writeBeginTable(c("Test suite : test function", "message"),
                    htmlFile=fileName,
                    columnWidth=c("30%", "70%"))
    for(tsName in names(testData)) {
      if(testData[[tsName]]$nFail > 0) {
        srcFileRes <- testData[[tsName]]$sourceFileResults
        srcFileNames <- names(srcFileRes)
        for(i in seq(length=length(srcFileRes))) {
          testFuncNames <- names(srcFileRes[[i]])
          for(j in seq(length=length(testFuncNames))) {
            funcList <- srcFileRes[[i]][[testFuncNames[j]]]
            if(funcList$kind == "failure") {
              lnk <- paste("<a href=\"",
                           createTestFuncRef(tsName, srcFileNames[i],
                                             testFuncNames[j], asAnchor=TRUE),
                           "\">",
                           paste(tsName, testFuncNames[j], sep=" : "),
                           "</a>", sep="")
              writeTableRow(row=c(lnk, funcList$msg),
                            htmlFile=fileName)
            }
          }
        }
      }
    }
    writeEndTable(htmlFile=fileName)
    writeHtmlSep(htmlFile=fileName)
  }


  ## deactivated table
  if(separateFailureList && (errInfo$nDeactivated > 0)) {
    writeHtmlSection("Deactivated", 3, htmlFile=fileName)
    writeBeginTable(c("Test suite : test function", "message"),
                    htmlFile=fileName,
                    columnWidth=c("30%", "70%"))
    for(tsName in names(testData)) {
      if(testData[[tsName]]$nDeactivated > 0) {
        srcFileRes <- testData[[tsName]]$sourceFileResults
        srcFileNames <- names(srcFileRes)
        for(i in seq(length=length(srcFileRes))) {
          testFuncNames <- names(srcFileRes[[i]])
          for(j in seq(length=length(testFuncNames))) {
            funcList <- srcFileRes[[i]][[testFuncNames[j]]]
            if(funcList$kind == "deactivated") {
              lnk <- paste("<a href=\"",
                           createTestFuncRef(tsName, srcFileNames[i],
                                             testFuncNames[j], asAnchor=TRUE),
                           "\">",
                           paste(tsName, testFuncNames[j], sep=" : "),
                           "</a>", sep="")
              writeTableRow(row=c(lnk, funcList$msg),
                            htmlFile=fileName)
            }
          }
        }
      }
    }
    writeEndTable(htmlFile=fileName)
    writeHtmlSep(htmlFile=fileName)
  }

  ## --------------------------------
  ## PART 4: DETAILS
  ## --------------------------------

  writeHtmlSection("Details", 3, htmlFile=fileName)

  ## loop over all test suites
  for(tsName in names(testData)) {
    tsList <- testData[[tsName]]
    writeBeginTag("p", htmlFile=fileName)

    writeBeginTag("a", para=paste("name=\"", tsName, "\"", sep=""),
                  htmlFile=fileName)
    writeHtmlSection(paste("Test Suite:", tsName), 5, htmlFile=fileName)
    writeEndTag("a", htmlFile=fileName)

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
        writeBeginTag("ul", htmlFile=fileName)
        for(testFileName in testFileNames) {
          writeBeginTag("li", htmlFile=fileName)
          writeLink(target=testFileName,
                    name=paste("Test file:", testFileName),
                    htmlFile=fileName)


          testFuncNames <- names(res[[testFileName]])
          if(length(testFuncNames) == 0) {
            pr("no test functions")
          }
          else {
            ## loop over all test functions in the test file
            writeBeginTag("ul", htmlFile=fileName)
            for(testFuncName in testFuncNames) {
              writeBeginTag("li", htmlFile=fileName)
              testFuncInfo <- res[[testFileName]][[testFuncName]]
              anchorName <- createTestFuncRef(tsName, testFileName, testFuncName)
              writeBeginTag("a", para=paste("name=\"", anchorName, "\"", sep=""),
                            htmlFile=fileName)
              if(testFuncInfo$kind == "success") {
                pr(paste(testFuncName, ":", " ... OK (", testFuncInfo$time,
                         " seconds)", sep=""))
                writeEndTag("a", htmlFile=fileName)
              }
              else {
                if(testFuncInfo$kind == "error") {
                  writeBeginTag("u", para=paste("style", errorStyle, sep="="),
                                htmlFile=fileName)
                  writeRaw(paste(testFuncName, ": ERROR !!  ", sep=""),
                           htmlFile=fileName)
                  writeEndTag("u", htmlFile=fileName)
                  writeEndTag("a", htmlFile=fileName)
                }
                else if (testFuncInfo$kind == "failure") {
                  writeBeginTag("u", para=paste("style", errorStyle, sep="="),
                                htmlFile=fileName)
                  writeRaw(paste(testFuncName, ": FAILURE !! (check number ",
                                 testFuncInfo$checkNum, ")  ", sep=""),
                           htmlFile=fileName)
                  writeEndTag("u", htmlFile=fileName)
                  writeEndTag("a", htmlFile=fileName)
                }
                else if (testFuncInfo$kind == "deactivated") {
                  writeRaw(paste(testFuncName, ": DEACTIVATED, ", sep=""),
                           htmlFile=fileName)
                  writeEndTag("a", htmlFile=fileName)
                }
                else {
                  writeLi(paste(testFuncName, ": unknown error kind", sep=""))
                  writeEndTag("a", htmlFile=fileName)
                }
                pr(testFuncInfo$msg)
                printTraceBack(testFuncInfo$traceBack)
              }
              writeEndTag("li", htmlFile=fileName)
            }
            writeEndTag("ul", htmlFile=fileName)
          }
          writeEndTag("li", htmlFile=fileName)
        }
        writeEndTag("ul", htmlFile=fileName)
      }
    }
    writeHtmlSep(htmlFile=fileName)
  }



  ver <- cbind(unlist(version))
  colnames(ver) <- ""
  pr("\n\n --------------------------------\n")
  write.table(ver, sep="\t", quote=FALSE, append=TRUE,
              col.names=FALSE, file=fileName)

  ## finish html document
  writeHtmlEnd(htmlFile=fileName)
}
