######################################################################
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


.First.lib <- function(lib, package, where)
{
  ##@bdescr
  ## Internal Function.
  ## Not to be called by users.
  ##
  ##@edescr
  
  ##  load required packages
  errMsg <- paste("\nLoading required package 'methods' failed. RUnit could not be loaded.",
                  "\nCheck your library installation path.\n")
  require(methods) || stop(errMsg)

  runitVersion <- packageDescription("RUnit", lib.loc=lib, fields="Version")
}


.onLoad <- function(lib, pkg)
{
  ##@bdescr
  ## Internal Function.
  ## Not to be called by users.
  ##
  ## has the same role as .First.lib in case a package provides a namespace
  ## when .First.lib is disregarded
  ## does not yield conflicts when loaded into R < 1.7.0 this function is simply ignored
  ## library calls are executed via import() statements in the inst/NAMESPACE file
  ##@edescr

  ##  load required packages
  errMsg <- paste("\nLoading required package 'methods' failed. RUnit could not be loaded.",
                  "\nCheck your library installation path.\n")
  require(methods) || stop(errMsg)

  
  runitVersion <- packageDescription("RUnit", lib.loc=lib, fields="Version")
}




##  define generics used in RUnit
setGeneric("countResult", function(obj) {
  ##@bdescr
  ## returns number of successes, failures, errors and deactivations as
  ## a 4-element vector. The sum of elements equals the number of test
  ## functions in this file. The element names of the result vector
  ## indicate the meaning of the elements
  ##@edescr
  standardGeneric("countResult")
})

setGeneric("testFuncsByState", function(obj, state) {
  ##@bdescr
  ## Returns a list of all TestFunctionResult objects with given
  ## states. The names of the list are the corresponding test file
  ## names (i.e. the names are not unique).
  ##@edescr
  ##@in object :  object from which to retrieve the TestFunctionResult objects
  ##@in state  : [character] one or more states 
  ##@ret       : [list] all TestFunctionResult objects having one of the passed states
  standardGeneric("testFuncsByState")
})
           
setGeneric("sourceErrors", function(obj) {
   ##@bdescr
  ## Returns a list of all TestFileResult objects where sourcing failed.
  ## The names of the list are the corresponding test suite
  ## names (i.e. the names are not unique).
  ##@edescr
  ##@in obj : object from which to retrieve the TestFileResult objects
  ##@ret    : [list] all TestFileResult objects where sourcing failed
  standardGeneric("sourceErrors")
})


setGeneric("toText", function(obj, verbosity) {
  ##@bdescr
  ## Creates a textual representation and returns it as character vector.
  ## verbosity=0: only basic info is returned (used for print and show)
  ## verbosity=1: more but not all details are returned (used for summary)
  ## verbosity=2: all details are returned (used for printTextProtocol)
  ##@edescr
  ##@in obj : object from which to create the text
  ##@in verbosity : [numeric] flag controlling level of detail
  ##@ret : [character] char-vector containing the text
  standardGeneric("toText")
})

setGeneric("details", function(obj) {
  ##@bdescr
  ## writes all available data for an object to the command line
  ##@edescr
  ##@in obj : object from which to create the output

  standardGeneric("details")
})

setGeneric("isValid", function(obj) {
  ##@bdescr
  ## checks object against defined contracts
  ##@edescr
  ##@in obj : object to check

  standardGeneric("isValid")
})


##  this ought to be a generic already
#setGeneric("show", function(object) standardGeneric("show"))

## --- Def of RUnitBase class and implementation of standard output methods
##     in terms of 'toText' generic function
setClass("RUnitBase", representation("VIRTUAL"))

setMethod("toText", c("RUnitBase", "numeric"),
          function(obj, verbosity) {
            stop("toText not defined for RUnitBase")
          })
            
setMethod("show", "RUnitBase",
          function(object) {
            sapply(toText(object, verbosity=0), function(x) cat(x, "\n"))
            return(invisible(NULL))
          })

setMethod("summary", "RUnitBase",
          function(object) {
            sapply(toText(object, verbosity=1), function(x) cat(x, "\n"))
            return(invisible(NULL))
          })

setMethod("details", "RUnitBase",
          function(obj) {
            sapply(toText(obj, verbosity=2), function(x) cat(x, "\n"))
            return(invisible(NULL))
          })

setMethod("isValid", "RUnitBase",
          function(obj) {
            sapply(toText(obj, verbosity=2), function(x) cat(x, "\n"))
            return(invisible(NULL))
          })
