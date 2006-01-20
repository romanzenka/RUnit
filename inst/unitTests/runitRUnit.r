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
##
##  $Id$


cat("\n\nRUnit test cases for 'RUnit:check' functions\n\n")


testRUnit.checkEquals <- function()
{
  ##@bdescr
  ## test case for function checkEquals of class: none
  ##@edescr

  checkTrue( TRUE)

  
  checkTrue( checkEquals(9,9))
  
  checkTrue( checkEquals( numeric(1), numeric(1)))
  checkTrue( checkEquals( character(1), character(1)))

  checkTrue( checkEquals( matrix(1, 3,5), matrix(1, 3,5)))
  checkTrue( checkEquals( matrix(1, 5000,5), matrix(1, 5000,5)))

  checkTrue( checkEquals( expression(2), expression(2)))
  checkTrue( checkEquals( list(100), list(100)))
  
  sysTime <- as.POSIXct(Sys.time())
  checkEquals( sysTime, sysTime)
  
  ##  check exception
  checkException( checkTrue(FALSE))
  
  ##  nested type not supported
  sysTime <- as.POSIXct(Sys.time())
  checkException( checkEquals( list(a=2, list(time=sysTime)), list(a=2, time=list(sysTime))))
}


testRUnit.checkEqualsNumeric <- function()
{
  ##@bdescr
  ## test case for function checkEqualsNumeric of class: none
  ##@edescr

  checkTrue( checkEqualsNumeric( 9,9))
  rvec <- rnorm(132)
  checkTrue( checkEqualsNumeric( matrix(rvec, 12, 11), matrix(rvec, 12, 11)))
  checkTrue( checkEqualsNumeric( rvec, rvec))

  ##  check exception
  ##  numeric difference
  checkException( checkEqualsNumeric( 9, 10))
  checkException( checkEqualsNumeric( list(9), list(10)))
  checkException( checkEqualsNumeric( matrix(9), matrix(10)))
  rvec2 <- rnorm(132)
  checkException( checkEqualsNumeric( matrix(rvec, 12, 11), matrix(rvec2, 12, 11)))

  ##  type not supported
  checkException( checkEqualsNumeric( list(rvec), list(rvec)))
}


testRUnit.checkIdentical <- function()
{
  ##@bdescr
  ## test case for function checkIdentical of class: none
  ##@edescr

  checkIdentical( TRUE, TRUE)
  checkIdentical( FALSE, FALSE)

  checkIdentical( as.integer(2), as.integer(2))
  checkIdentical( as.character(2), as.character(2))
  checkIdentical( as.complex(2), as.complex(2))
  checkIdentical( as.numeric(2), as.numeric(2))
  checkIdentical( as.expression("2+4"), as.expression("2+4"))
  checkIdentical( as.expression(2+4), as.expression(2+4))

  sysTime <- as.POSIXlt(Sys.time())
  checkIdentical( sysTime, sysTime)

  ##  S3 objects (ie. lists with attributes)
  ##  from ?lm Example
  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
  trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
  group <- gl(2,10,20, labels=c("Ctl","Trt"))
  weight <- c(ctl, trt)
  lm.D9 <- lm(weight ~ group)
  checkIdentical( lm.D9, lm(weight ~ group))


  ##  S4 objects
  if (isTRUE(require(methods))) {
    setClass("track",
             representation(x="numeric", y="numeric"),
             where=.GlobalEnv)
    on.exit(removeClass("track", where=.GlobalEnv))

    s4Obj <- try(new("track"))
    checkIdentical( s4Obj, new("track"))
    rm(s4Obj)
  }

  
  ##  exception handling
  ##  type mismatches
  checkException( checkIdentical( as.integer(2), as.numeric(2)))
  checkException( checkIdentical( as.integer(2), as.character(2)))
  checkException( checkIdentical( as.integer(2), as.list(2)))
  checkException( checkIdentical( as.integer(2), as.complex(2)))
  checkException( checkIdentical( as.integer(2), as.expression(2)))

  ##  value mismatches
  checkException( checkIdentical( as.integer(2), as.integer(3)))
  checkException( checkIdentical( as.character(2), as.character(3)))
  checkException( checkIdentical( as.complex(2), as.complex(3)))
  checkException( checkIdentical( as.numeric(2), as.numeric(3)))
  checkException( checkIdentical( as.expression("2+4"), as.expression("2+3")))

  sysTime <- as.POSIXlt(Sys.time())

  checkException( checkIdentical( sysTime, as.POSIXlt(Sys.time(), tz="GMT")))

  ##  S3 objects (ie. lists with attributes)
  ##  from ?lm Example
 

  lm.D9base <- lm(weight ~ group - 1)
  checkException( checkIdentical( lm.D9base, lm.D9))

  ##  S4 objects
  if (isTRUE(require(methods))) {
    setClass("track2",
             representation(x="numeric", y="numeric"),
             prototype(x=as.numeric(1:23), y=as.numeric(23:1)),
             where=.GlobalEnv)
    on.exit(removeClass("track2", where=.GlobalEnv))

    s4Obj <- try(new("track2"))
    s4ObjDiff <- s4Obj
    s4ObjDiff@y <- s4ObjDiff@x
    checkException( checkIdentical( s4Obj, s4ObjDiff))
  }

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
