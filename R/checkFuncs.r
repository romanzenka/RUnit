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

runitInternalError <- function (message, call = NULL)
{
  ##@bdescr
  ##  derived from 'simpleError' in package base
  ##  signal error generated via Runit except for check* function calls
  ##  i.e. unexpected errors to distinguish from others
  ##@edescr
  ##
  ##@in  meassge : [character]
  ##@in  call    : []
  class <- c("RUnitInternalError", "error", "condition")
  structure(list(message = as.character(message), call = call),
            class = class)
}


runitError <- function (message, call = NULL)
{
  ##@bdescr
  ##  derived from 'simpleError' in package base
  ##  signal error generated via Runit check* function calls
  ##  i.e. expected errors to distinguish from others
  ##@edescr
  ##
  ##@in  meassge : [character]
  ##@in  call    : []
  class <- c("RUnitError", "error", "condition")
  structure(list(message = as.character(message), call = call),
            class = class)
}


runitFailure <- function (message, call = NULL)
{
  ##@bdescr
  ##  derived from 'simpleError' in package base
  ##  signal failure generated via Runit check* function calls
  ##  i.e. expected failure to distinguish from others
  ##@edescr
  ##
  ##@in  meassge : [character]
  ##@in  call    : []
  class <- c("RUnitFailure", "error", "condition")
  structure(list(message = as.character(message), call = call),
            class = class)
}


runitDeactivated <- function (message, call = NULL)
{
  ##@bdescr
  ##  derived from 'simpleError' in package base
  ##  signal deactivated test case generated via Runit DEACTIVATED function call.
  ##@edescr
  ##
  ##@in  meassge : [character]
  ##@in  call    : []
  class <- c("RUnitDeactivated", "error", "condition")
  structure(list(message = as.character(message), call = call),
            class = class)
}


checkEquals <- function(target, current, msg="",
                        tolerance = .Machine$double.eps^0.5, checkNames=TRUE, ...)
{
  ##@bdescr
  ## checks if two objects are equal, thin wrapper around 'all.equal'
  ## with tolerance one can adjust to and allow for numerical imprecission

  ##@edescr
  ##@in  target    : [ANY] one thing to be compared
  ##@in  current   : [ANY] the second object to be compared
  ##@in  msg       : [character] an optional message to further identify and document the call
  ##@in  tolerance : [numeric] directly passed to 'all.equal', see there for further documentation
  ##@in  checkNames: [logical] iff TRUE do not strip names attributes from current and target prior to the comparison
  ##@ret           : [logical] TRUE iff check was correct
  ##
  ##@codestatus : testing

  
  if(!is.numeric(tolerance)) {
    stop(runitError("tolerance has to be a numeric value"))
  }
  if (length(tolerance) != 1) {
    stop(runitError("tolerance has to be a scalar"))
  }
  if(exists(".testLogger", envir=.GlobalEnv)) {
    .testLogger$incrementCheckNum()
  }
  if (!identical(TRUE, checkNames)) {
    names(target)  <- NULL
    names(current) <- NULL
  }
  result <- try(all.equal(target, current, tolerance=tolerance, ...))
  if (inherits(result, "try-error")) {
    if(exists(".testLogger", envir=.GlobalEnv)) {
      .testLogger$setError()
    }
    stop(result)
  }
  if (!identical(result, TRUE)) {
    if(exists(".testLogger", envir=.GlobalEnv)) {
      .testLogger$setFailure()
    }
    stop(runitFailure(paste(paste(result, collapse="\n"), msg), call=sys.call()))
  }
  else {
    return(TRUE)
  }
}


checkEqualsNumeric <- function(target, current, msg="", tolerance = .Machine$double.eps^0.5, ...)
{
  ##@bdescr
  ## checks if two objects are equal, thin wrapper around 'all.equal.numeric'
  ## with tolerance one can adjust to and allow for numerical imprecission
  ##@edescr
  ##@in target    : [ANY] one thing to be compared
  ##@in current   : [ANY] the second object to be compared
  ##@in tolerance : [numeric] directly passed to 'all.equal', see there for further documentation
  ##@in msg       : [character] an optional message to further identify and document the call
  ##
  ##@ret          : [logical] TRUE, if objects 'target' and 'current' are equal w.r.t. specified numerical tolerance, else a stop signal is issued 
  ##
  ##@codestatus : testing
  
  if(!is.numeric(tolerance)) {
    stop(runitError("tolerance has to be a numeric value"))
  }
  if (length(tolerance) != 1) {
    stop(runitError("tolerance has to be a scalar"))
  }
  if(exists(".testLogger", envir=.GlobalEnv)) {
    .testLogger$incrementCheckNum()
  }
  ##  R 2.3.0: changed behaviour of all.equal
  ##  strip attributes before comparing current and target
  result <- try(all.equal.numeric(as.vector(target), as.vector(current), tolerance=tolerance, ...))
  if (inherits(result, "try-error")) {
    if(exists(".testLogger", envir=.GlobalEnv)) {
      .testLogger$setError()
    }
    stop(result)
  }
  if (!identical(result, TRUE)) {
    if(exists(".testLogger", envir=.GlobalEnv)) {
      .testLogger$setFailure()
    }
    stop(runitFailure(paste(paste(result, collapse="\n"), msg), call=sys.call()))
  }
  else {
    return(TRUE)
  }
}


checkIdentical <- function(target, current, msg="")
{
  ##@bdescr
  ## checks if two objects are exactly identical, thin convenience wrapper around 'identical'
  ##
  ##@edescr
  ##@in target   : [ANY] one onject to be compared
  ##@in current  : [ANY] second object to be compared
  ##@in msg      : [character] an optional message to further identify and document the call
  ##
  ##@ret         : [logical] TRUE, if objects 'target' and 'current' are identical
  ##
  ##@codestatus : testing
  
  if(exists(".testLogger", envir=.GlobalEnv)) {
    .testLogger$incrementCheckNum()
  }
  
  ##  strip attributes before comparing current and target
  result <- try(identical(target, current))
  if (inherits(result, "try-error")) {
    if(exists(".testLogger", envir=.GlobalEnv)) {
      .testLogger$setError()
    }
    stop(result)
  }
  if (!identical(TRUE, result)) {
    if(exists(".testLogger", envir=.GlobalEnv)) {
      .testLogger$setFailure()
    }
    stop(runitFailure(paste(paste(result, collapse="\n"), msg), call=sys.call()))
  }
  else {
    return(TRUE)
  }
}


checkTrue <- function(expr, msg="")
{
  ##@bdescr
  ## checks whether or not something is true
  ##@edescr
  ##
  ##@in expr : [expression] the logical expression to be checked to be TRUE
  ##@in msg  : [character] optional message to further identify and document the call
  ##
  ##@ret     : [logical] TRUE, if the expression in a evaluates to TRUE, else a stop signal is issued 
  ##
  ##@codestatus : testing
  
  if(exists(".testLogger", envir=.GlobalEnv)) {
    .testLogger$incrementCheckNum()
  }

  ##  allow named logical argument expr
  result <- try(eval(expr))
  names(result) <- NULL
  if (inherits(result, "try-error")) {
    if(exists(".testLogger", envir=.GlobalEnv)) {
      .testLogger$setError()
    }
    stop(result)
  }
  if (!identical(result, TRUE)) {
    if(exists(".testLogger", envir=.GlobalEnv)) {
      .testLogger$setFailure()
    }
    stop(runitFailure(paste("Test not TRUE.\n", msg), call=sys.call()))
  }
  else {
    return(TRUE)
  }
}


checkException <- function(expr, msg="", silent=FALSE)
{
  ##@bdescr
  ## checks if a function call creates an error. The passed function must be parameterless.
  ## If you want to check a function with arguments, call it like this:
  ## 'checkException(function() func(args...))'
  ##
  ##  adding argument silent was suggested by Seth Falcon <sfalcon@fhcrc.org>
  ##  who provided a patch.
  ##@edescr
  ##@in  func   : [parameterless function] the function to be checked
  ##@in  msg    : [character] an optional message to further identify and document the call
  ##@in  silent : [logical] passed on to try, iff TRUE error messages will be suppressed 
  ##
  ##@ret        : [logical] TRUE, if evaluation of the expression results in a 'try-error', else a stop signal is issued 
  ##
  ##@codestatus : testing
  
  if(exists(".testLogger", envir=.GlobalEnv)) {
    .testLogger$incrementCheckNum()
  }

  if (!inherits(try(eval(expr, envir = parent.frame()), silent=silent), "try-error")) {
    if(exists(".testLogger", envir=.GlobalEnv)) {
      .testLogger$setFailure()
    }
    stop(runitError(paste("Error not generated as expected.\n", msg), call=sys.call()))
  }
  else {
    return(TRUE)
  }
}


DEACTIVATED <- function(msg="")
{
  ##@bdescr
  ##  Convenience function, for maintaining test suites.
  ##  If placed in an existing test case call
  ##  the test will be executed normally until (so all code will be checked and
  ##  errors or failures reported as usual) occurance of the call
  ##  after which execution will leave the test case.
  ##  An entry for a seperate table in the log will be added
  ##  for this test case.
  ##
  ##@edescr
  ##@in a   : [expression] the logical expression to be checked to be TRUE
  ##@in msg : [character] optional message to further identify and document the call
  ##
  ##@codestatus : testing

  if(exists(".testLogger", envir=.GlobalEnv)) {
    .testLogger$setDeactivated(paste(msg, "\n", sep=""))
  }
  stop(runitDeactivated(msg, call=sys.call()))
}
