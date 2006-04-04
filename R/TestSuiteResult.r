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
##  S4 implementation
##
##  $Id$
##
.defineTestSuiteResultClass <- function(where=environment())
{
  ##@bdescr
  ## This function contains the definition of the '' class.
  ## 
  ## 
  ##@edescr
  ##
  ##@class [] : storing all result information for one test suite

  if (GLOBAL$getDebug())
  {
    cat(".defineTestSuiteResultClass ... ")
  }

  setClass("TestSuiteResult",
           representation("TestResult",
                          directories     = "character",
                          testFileRegexp  = "character",
                          testFuncRegexp  = "character",
                          testFunctionNum = "integer",
                          deactivatedNum  = "integer",
                          errorNum        = "integer",
                          failureNum      = "integer",
                          testFileResults = "TestFileResultArray"),
           sealed   = GLOBAL$SEALED,
           where    = where,
           validity = NULL)

  if (GLOBAL$getDebug())
  {
    cat("o.k.\n")
  }
}
