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



ASSERT <- function(condition, msg=NULL){
  ##@bdescr
  ## this function doesn't do anything when the condition is true. If it is not true,
  ## this is written to the log file and 'stop' is called.
  ##@edescr
  ##
  ##@in  condition : [expression] the condition to check
  ##@in  msg       : [character] an optional message
  ##@ret           : [NULL] if the expression in condition evaluates to TRUE, else a stop signal is issued

  ##  allow named logical argument a
  condition.eval <- eval(condition)
  names(condition.eval) <- NULL

  if(identical(condition.eval, TRUE)){
    return(invisible())
  }
  ## create a default error message, if nothing was given
  if(is(msg,"NULL")) {
    
    msg <- paste("Documentation Error: No error message given by the developer.\n",
                 "Please ask the developer or look in the source of the function.\n",
                 "Hint: you can also use traceback() to get the call stack.")
  }
  msg <- paste(deparse(substitute(condition)), msg, "\n")
  .GLOBAL$handleError("FAILED ASSERTION", msg)
}


setFatalError <- function(msg){
  ##@bdescr
  ## this function writes the message and the traceback to the log file and then calls stop
  ##@edescr
  ##
  ##@in  msg : [character] string, the error message
  ##@ret     : [NULL]
  ##
  ##@codestatus : testing

  .GLOBAL$handleError("FATAL ERROR", msg)
}


setError <- function(msg){
  ##@bdescr
  ##  writes the message and the traceback to the log file and then returns
  ##@edescr
  ##
  ##@in  msg  : [character] string, the error message
  ##@ret      : [NULL]
  ##
  ##@codestatus : testing
  
  .GLOBAL$handleError("ERROR", msg)
}


setWarning <- function(msg){
  ##@bdescr
  ##  reports a warning and then returns
  ##@edescr
  ##
  ##@in  msg : [string] the error message
  ##@ret     : [NULL]
  ## .GLOBAL$handleError("WARNING", msg)  ## too noisy
  ##
  ##@codestatus : testing
  
  warning(msg)
}

