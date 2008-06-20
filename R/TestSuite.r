##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003-2008  Thomas Koenig, Matthias Burger, Klaus Juenemann
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



## --- Validation method

isValidTestSuite <- function(object) {
  ## @bdescr
  ## checks validity of TestSuite object
  ## @edescr
  ##
  ##@in object : [TestSuite] object of type 'TestSuite' to be checked
  ##@ret       : [logical | character] TRUE, if object is valid, error message otherwise
  
  if(!is(object, "TestSuite")) {
    return("Object must be of type TestSuite")
  }
  if(length(object@name) != 1) {
    return("Test suite must have exactly one name")
  }
  if(length(object@dirs) == 0) {
    return("Test suite must contain at least one directory")
  }

  ## If directories defined in the test suite don't exist, we just warn
  ## but let the user still create the TestSuite object.
  if (!all(file.exists(object@dirs))) {
    warning(paste("Directory",object@dirs,"specified in test suite",
                  object@name, "cannot be found."))
  }
  if (length(object@testFileRegexp) != 1) {
    return("Test suite must contain exactly 'testFileRegexp'")
  }
  if (length(object@testFuncRegexp) != 1) {
    return("Test suite must contain exactly 'testFuncRegexp'")
  }
  if (length(object@rngKind) != 1) {
    return("Specifed 'rngKind' may only contain exactly one name.")
  }
  if (length(object@rngNormalKind) != 1) {
    return("Specifed 'rngNormalKind' may only contain exactly one name.")
  }
  return(TRUE)
}

## --- Class definition
setClass("TestSuite",
         representation("RUnitBase", 
                        name="character",  # name of test suite
                        dirs="character",  # directories (full path) where test files are located
                        testFileRegexp="character", # regexp that identifies test files
                        testFuncRegexp="character", # regexp that identifies test functions
                        rngKind="character",        # kind of random number generator
                        rngNormalKind="character"), # kind of normal random number generator
         prototype = list(testFileRegexp="^runit.+\\.[rR]$",
           testFuncRegexp="^test.+",
           rngKind="Marsaglia-Multicarry",
           rngNormalKind="Kinderman-Ramage"),
         validity=isValidTestSuite)  # called if 'new' is called with at least one slot argument



## --- Constructor

defineTestSuite <- function(name, dirs, 
                            testFileRegexp="^runit.+\\.[rR]$",
                            testFuncRegexp="^test.+",
                            rngKind="Marsaglia-Multicarry",
                            rngNormalKind="Kinderman-Ramage") {

  return(new("TestSuite",
             name=name,dirs=dirs,
             testFileRegexp=testFileRegexp,
             testFuncRegexp=testFuncRegexp,
             rngKind=rngKind,
             rngNormalKind=rngNormalKind))          
}


## --- Display methods

setMethod("toText", c("TestSuite", "numeric"),
          function(obj, verbosity) {
            ## see 00Init.r for documentation
            text <- paste("RUnit test suite '", obj@name, "'", sep="")
            if(verbosity >= 2) {
              text <- append(text, paste("  Test function regexp:",
                                         obj@testFuncRegexp))
              text <- append(text, paste("  Test file regexpobjp:",
                                         obj@testFileRegexp))
              text <- append(text, paste("  Random number generator:",
                                         obj@rngKind))
              text <- append(text, paste("  Normal random number generator:",
                                         obj@rngNormalKind))
              if(length(obj@dirs) > 0) {
                text <- append(text, "  Directories:")
                text <- append(text, sapply(obj@dirs,
                                            function(d) paste("    ", d, sep="")))
              }
            }
            names(text) <- NULL
            return(text)
          })
