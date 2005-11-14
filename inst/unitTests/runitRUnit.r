##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003, 2004, 2005  Thomas Koenig, Matthias Burger, Klaus Juenemann
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
##
##  $Id$


cat("\n\nRUnit test cases for 'RUnit:check' functions\n\n")


testRUnit.checkEquals <- function()
{
  ##@bdescr
  ## test case for function checkEquals of class: none
  ##@edescr

  checkTrue( TRUE)
  checkException( checkTrue(FALSE))
  
  checkTrue( checkEquals(9,9))
  
  checkTrue( checkEquals( numeric(1), numeric(1)))
  checkTrue( checkEquals( character(1), character(1)))

  checkTrue( checkEquals( matrix(1, 3,5), matrix(1, 3,5)))
  checkTrue( checkEquals( matrix(1, 5000,5), matrix(1, 5000,5)))

  checkTrue( checkEquals( expression(2), expression(2)))
  checkTrue( checkEquals( list(100), list(100)))
}


testRUnit.checkEqualsNumeric <- function()
{
  ##@bdescr
  ## test case for function checkEqualsNumeric of class: none
  ##@edescr

  checkTrue( checkEqualsNumeric( 9,9))
  checkException( checkEqualsNumeric( 9, 10))
  
}


testRUnit.checkTrue <- function()
{
  ##@bdescr
  ## test case for function checkTrue of class: none
  ##@edescr

  checkException( checkTrue( FALSE))
  checkEquals( checkTrue( TRUE), TRUE)

  ##  named arguments
  namedArg <- TRUE
  names(namedArg) <- "Yes"
  checkEquals( checkTrue( namedArg), TRUE)
  
  namedArg <- FALSE
  names(namedArg) <- "No"
  checkException( checkTrue( namedArg))


  ##  incorrect length
  checkException( checkTrue( c(TRUE, TRUE)))
  checkException( checkTrue( c(FALSE, TRUE)))
  checkException( checkTrue( logical(0)))
  checkException( checkTrue( logical(2)))
  
}


testRUnit.checkException <- function()
{
  ##@bdescr
  ## test case for function checkException of class: none
  ##@edescr
  
  checkException( checkTrue( FALSE))
  checkException( checkTrue( ))
  checkException( checkEquals( ))
  checkException( checkEquals( 24))
  checkException( checkEquals( 24, 24, tol="dummy"))
  checkException( checkEqualsNumeric( ))
  checkException( checkEqualsNumeric( 24))
  checkException( checkEqualsNumeric( 24, 24, tol="dummy"))

  
}


testRUnit.DEACTIVATED <- function()
{
  ##@bdescr
  ## test case for function DEACTIVATED of class: none
  ##@edescr

  checkException( DEACTIVATED())
}


testRUnit.isValidTestSuite <- function()
{
  ##@bdescr
  ## test case for function isValidTestSuite of class: none
  ##@edescr
  
  ##  correct working
  testSuite <- defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), testFileRegexp="correctTestCase.r")
  checkTrue( isValidTestSuite(testSuite))
  
  ##  error handling
  ##  has to be S3 class 'RUnitTestSuite'
  testSuiteFail <- testSuite
  class(testSuiteFail) <- "NotUnitTestSuite"
  checkTrue( !isTRUE(isValidTestSuite(testSuiteFail)))
  
  ##  expecting list elements
  testSuiteFail <- testSuite
  testSuiteFail[["dirs"]] <- NULL
  checkTrue( !isTRUE(isValidTestSuite(testSuiteFail)))
  
  ##  has to be character
  testSuiteFail <- testSuite
  testSuiteFail[["name"]] <- list()
  checkTrue( !isTRUE(isValidTestSuite(testSuiteFail)))
 
  testSuiteFail <- testSuite
  testSuiteFail[["dirs"]] <- list()
  checkTrue( !isTRUE(isValidTestSuite(testSuiteFail)))
  
  testSuiteFail <- testSuite
  testSuiteFail[["testFileRegexp"]] <- list()
  checkTrue( !isTRUE(isValidTestSuite(testSuiteFail)))
  
  testSuiteFail <- testSuite
  testSuiteFail[["testFuncRegexp"]] <- list()
  checkTrue( !isTRUE(isValidTestSuite(testSuiteFail)))
  
  
  ##  director has to exist
  testSuiteFail <- testSuite
  testSuiteFail[["dirs"]] <- "doesNotExist"
  checkTrue( !isTRUE(isValidTestSuite(testSuiteFail)))
}
  

testRUnit.runTestFile <- function()
{
  ##@bdescr
  ## test case for function runTestFile of class: none
  ##@edescr

  testFile <- file.path(system.file("examples", package="RUnit"), "correctTestCase.r")
  checkTrue( file.exists(testFile))
  
  ##res <- runTestFile(testFile)
  
  ##  error handling
  ##  useOwnErrorHandler
  ##  type logical
  checkException( runTestFile(testFile, useOwnErrorHandler=integer(1)))
}


testRUnit.runTestSuite <- function()
{
  ##@bdescr
  ## test case for function runTestSuite of class: none
  ##@edescr

  testSuite <- defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), testFileRegexp="correctTestCase.r")

  ##res <- runTestSuite(testSuite)
  
  ##  error handling
  ##
  ##  useOwnErrorHandler
  ##  type logical
  checkException( runTestSuite(tS, useOwnErrorHandler=integer(1)))
  ##  length 1
  checkException( runTestSuite(tS, useOwnErrorHandler=logical(0)))
  checkException( runTestSuite(tS, useOwnErrorHandler=logical(2)))
  checkException( runTestSuite(tS, useOwnErrorHandler=as.logical(NA)))
  
  ##  testSuite
  

}