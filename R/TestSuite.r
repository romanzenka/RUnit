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

##  $Id$


.defineTestSuiteClass <- function(where=environment()) {
  ##@bdescr
  ##
  ##  Initialisation function
  ##  Not to be called by the user(s).
  ##  Initialization for class 'TestSuite'.
  ##  
  ##@edescr
  ##
  ##@class : [TestSuite]
  ##
  ##@slot  name           : [character] test suite name 
  ##@slot  dirs           : [character] vector of paths to search for test case files 
  ##@slot  testFileRegexp : [character] regular expression 
  ##@slot  testFuncRegexp : [character] regular expression 
  ##@slot  rngKind        : [character] RNG method  
  ##@slot  rngNormalKind  : [character] RNG version 


  if (.GLOBAL$getDebug()) {
    
    cat(".defineTestSuiteClass ... ")
  }
    
  setClass("TestSuite",
           representation(name           = "character",
                          dirs           = "character",
                          testFileRegexp = "character",
                          testFuncRegexp = "character",
                          rngKind        = "character",
                          rngNormalKind  = "character"
                          ),

           prototype(), 
           validity = NULL,
           sealed   = TRUE,
           where    = where)

  
  if (.GLOBAL$getDebug()) {
    
    cat("o.k.\n")
  }
}
