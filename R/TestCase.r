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


.defineTestCaseClass <- function(where=environment())
{
  ##@bdescr
  ## Definition of the TestCase class
  ## 
  ##@edescr
  ##
  ##@class : [TestCase]
  ##
  ##@slot  functionName   : [character] test function name
  ##@slot  sourceFileName : [character] full source file name (abs. path + file)

  if (.GLOBAL$getDebug()) {  
    cat(".defineTestCaseClass ... ")
  }
  

  setClass("TestCase",
           representation(functionName     = "character",
                          sourceFileName   = "character"),
           validity = NULL,
           sealed   = .GLOBAL$getSealed(),
           where    = where
           )
  
  if (.GLOBAL$getDebug()) {
    cat("o.k.\n")
  }
}

