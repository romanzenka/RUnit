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
##
##  $Id$


cat("\n\nRUnit test cases for 'HTMLProtocol' function\n\n")


testRUnit.printHTMLProtocol <- function()
{
  ##  input argument error handling
  ##  missing 'testData' object
  checkException(printHTMLProtocol())

  ##  wrong class
  checkException(printHTMLProtocol("dummy"))

  
  ##  fileName arg errors
  testData <- list()
  class(testData) <- "RUnitTestData"
  ##  wrong type
  checkException(printHTMLProtocol(testData, fileName=numeric(1)))
  ##  wrong length
  checkException(printHTMLProtocol(testData, fileName=character(0)))
  checkException(printHTMLProtocol(testData, fileName=character(2)))

  
  ##  separateFailureList arg errors
  ##  wrong type
  checkException(printHTMLProtocol(testData, separateFailureList=numeric(0)))
  ##  wrong length
  checkException(printHTMLProtocol(testData, separateFailureList=logical(0)))
  checkException(printHTMLProtocol(testData, separateFailureList=logical(2)))
}

