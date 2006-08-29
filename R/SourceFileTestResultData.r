##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2006 Matthias Burger, Thomas Koenig
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


.defineSourceFileTestResultDataClass <- function(where=environment()) {
  ##@bdescr
  ##
  ##  Initialisation function
  ##  Not to be called by the user(s).
  ##  Initialization for class 'SourceFileTestResultData'.
  ##  
  ##@edescr
  ##
  ##@class : [SourceFileTestResultData]
  ##
  ##@slot  name           : [character] test suite name 
  ##@slot  dir            : [character]  
  ##@slot  rngKind        : [character] RNG method  
  ##@slot  rngNormalKind  : [character] RNG version 
  ##@slot  numTestFunc    : [integer]  
  ##@slot  numDeactivated : [integer]  
  ##@slot  numFailed      : [integer]  
  ##@slot  numError       : [integer]  
  ##@slot  testCaseResult : [TestCaseTestResultDataArray]  
  ##@slot  testFileRegexp : [character]  
  ##@slot  testFuncRegexp : [character]  


  if (.GLOBAL$getDebug()) {
    
    cat(".defineSourceFileTestResultDataClass ... ")
  }
  
  setClass("SourceFileTestResultData",
           representation(name           = "character",
                          dir            = "character",
                          rngKind        = "character",
                          rngNormalKind  = "character",
                          numTestFunc    = "integer",
                          numDeactivated = "integer",
                          numFailed      = "integer",
                          numError       = "integer",
                          testCaseResult = "TestCaseTestResultDataArray",
                          testFileRegexp = "character",
                          testFuncRegexp = "character"),
           prototype(rngKind        = "Mersenne-Twister",
                     rngNormalKind  = "Inversion",
                     testFileRegexp = "^runit.+\\.[rR]$",
                     testFuncRegexp = "^test.+"),
           validity = NULL,
           sealed   = TRUE,
           where    = where)
  
  if (.GLOBAL$getDebug()) {
    
    cat("o.k.\n")
  }
}
