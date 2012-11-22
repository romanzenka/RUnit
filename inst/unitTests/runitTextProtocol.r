##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003-2012  Thomas Koenig, Matthias Burger, Klaus Juenemann
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; version 2 of the License.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
##
##  $Id$


cat("\n\nRUnit test cases for 'textProtocol' function\n\n")

.setUp <- function() {
	##  copy baseenv() logger
	tmp <- get(".testLogger", envir = .GlobalEnv)
	testCaseDir <- file.path(system.file(package="RUnit"), "examples")
	testSuiteInternal <- defineTestSuite("RUnit Self Test", testCaseDir, "correctTestCase.r")
	testData2 <<- runTestSuite(testSuiteInternal, useOwnErrorHandler=FALSE)
	
	timeStamp <- format(Sys.time(), "%y%m%d-%H%M")
	testProtocolFile <<- file.path(tempdir(), paste(timeStamp, "test_printHTMLProtocol.txt", sep="_"))
	
	assign(".testLogger", tmp, envir = .GlobalEnv)
	
}


.tearDown <- function() {
	
	rm(list=c("testData2", "testProtocolFile"), envir = .GlobalEnv)
}


testRUnit.printTextProtocol <- function()
{
	
  ret <- printTextProtocol(testData2, fileName=testProtocolFile)
  checkTrue( file.exists(testProtocolFile))
  
  
  ##  input argument error handling
  ##  missing 'testData' object
  checkException(printTextProtocol())

  ##  wrong class
  checkException(printTextProtocol("dummy"))
  
  myTestData <- vector(mode="list", 3)
  for (i in seq_along(myTestData)) {
	  myTestData[[i]]$nErr <- i
	  myTestData[[i]]$nDeactivated <- i - 1
	  myTestData[[i]]$nTestFunc <- i*13
	  myTestData[[i]]$nFail <- i + 3
  }
  class(myTestData) <- "myTestData"
  checkException(printTextProtocol(myTestData))
  
  
  testData <- list()
  class(testData) <- "RUnitTestData"

  ##  fileName arg errors
  ##  wrong type
  checkException(printTextProtocol(testData, fileName=numeric(1)))
  ##  wrong length
  checkException(printTextProtocol(testData, fileName=character(0)))
  checkException(printTextProtocol(testData, fileName=character(2)))
  ##  NA
  checkException(printTextProtocol(testData, fileName=as.character(NA)))
  
  ##  separateFailureList arg errors
  ##  wrong type
  checkException(printTextProtocol(testData, separateFailureList=numeric(0)))
  ##  wrong length
  checkException(printTextProtocol(testData, separateFailureList=logical(0)))
  checkException(printTextProtocol(testData, separateFailureList=logical(2)))
  ##  NA
  checkException(printTextProtocol(testData, separateFailureList=as.logical(NA)))

  ##  showDetails arg errors
  ##  wrong type
  checkException(printTextProtocol(testData, showDetails=numeric(0)))
  ##  wrong length
  checkException(printTextProtocol(testData, showDetails=logical(0)))
  checkException(printTextProtocol(testData, showDetails=logical(2)))
  ##  NA
  checkException(printTextProtocol(testData, showDetails=as.logical(NA)))
}


testRUnit.print <- function()
{
	checkTrue( is.null(print(testData2)))
}

testRUnit.summary <- function()
{
	checkTrue( summary(testData2))
}
