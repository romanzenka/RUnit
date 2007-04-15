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



## This is the ONE global variable in RUnit.
## If not null, it holds the closure that controls the global state of the
## package.
.GLOBAL <- NULL

## This function initializes the .GLOBAL object that controls the global state
## of the package.
## The features administrated by this object are:
## * debug modus
## * error status
## * log file
## * progress status
##
## The behaviour can be controlled through three environment variables:
## RUNIT_DEBUG and RUNIT_LOGFILE
##
## DETAILS
## -------------------------------------
##  1.) debug modus
##  debug modus is controlled through the logical variable .GLOBAL$DEBUG.
##  The variable can be set by .GLOBAL$setDebug([TRUE|FALSE]). The content
##  of the variable can be get by .GLOBAL$getDebug()
##  It is set to true only if the environment variable RUNIT_DEBUG has the
##  value TRUE
##
##  2.) sealed flag
##  The logical variable GLOBAL$SEALED,where=where is supposed to be used for
##  the sealed argument in setClass calls. It is controlled through the
##  environment variable RUNIT_SEALED and has the value FALSE unless
##  RUNIT_SEALED is set to TRUE. Setting the sealed flag to TRUE, RUNIT has
##  problems with method dispatch.
##
##  3.) error status, available functions:
##  - .GLOBAL$getErrorStatus(): returns the current error status ('noError',
##   'error' or 'fatal').
##  - .GLOBAL$clearErrorStatus(): sets error status to 'noError'.
##  - .GLOBAL$getLastErrorMsg(): returns the last error message.
##  - .GLOBAL$getErrorMsgStack():  returns the complete stack of error messages.
##  - .GLOBAL$clearErrorMsgStack(): deletes the  stack of error messages and
##    sets error status to 'noError'.
##  - .GLOBAL$handleError(errorStatus, msg): only used by global error handling
##    functions
##
##  4.) log file, available functions:
##  - .GLOBAL$log(msg): writes a log messages 'msg'
##  - .GLOBAL$getLogFileName(): returns the name of the log file
##  - .GLOBAL$getLogString(): returns the string where all the logmessages have
##    been stored in (makes sense in CACHE mode only)
##  - .GLOBAL$clearLogString(): deletes  the string where all the logmessages
##    have been stored in.
##
##  The behaviour of the log file is configured through the environment variable
##  RUNIT_LOGFILE:
##  If it is unset log messages (including error
##  messages) are immediately written to the standard output.
##  If it has the value  OFF all log information is ignored.
##  If it has the value CACHE all log information is stored in an
##  internal message string and can be retrieved from the .GLOBAL
##  closure at any time. Otherwise the program tries to create a
##  logfile whose name has the value of the logfile. If such a file cannot
##  be created the program switches to CACHE mode. If such a file is
##  already exists, everything is appended to the end.
##
##  5.) progress status
##  - .GLOBAL$log(msg): set the progress status (a number between 0 and 1)


.initGLOBAL <- function(where=environment()){


  ## ----------------------------------------
  ## private helper functions
  ## ----------------------------------------

  readLogicalEnv <- function(envName, default=TRUE){
    ##@bdescr
    ## reads an environment variable and transforms it to a logical value.
    ## If the named shell variable is not defined or not set (empty string)
    ## return value is the value defined by argument default.
    ## Returns TRUE iff the value of the environment variable is TRUE
    ## otherwise FALSE is returned.
    ##@edescr
    ##@ret : [logical] TRUE iff envName is defined and set to 'TRUE'
    ##
    ##@codestatus : internal
    
    x <- as.character(Sys.getenv(envName))

    if (identical(x, "")) {
      
      ##  no environment variable defined
      return(as.logical(default))
      
    } else if (identical(x, "TRUE")) {
      
      return(TRUE)
      
    } else {
      
      return(FALSE)
    }
  }

  initLog <- function(where=environment()) {
    ##@bdescr
    ## creates an initialisation string for the log file (or log string)
    ##@edescr
    ##@ret : [character] the initialisation string
    ##
    ##@codestatus : internal
    
    ret <- paste("RUnit log file, date:", date(), ", user: ",
                 as.character(Sys.getenv("USER")), "\n")
    ret <- paste(ret, "******************************************************************\n\n", sep="")
    return(ret)
  }

  initLogFile<-function(where=environment()) {
    ##@bdescr
    ## tries to initializes the log file
    ##@edescr
    ##@ret : the name of the log file or NULL (if log messages are ignored) or 'CACHE' (if log messages are cached)
    ##
    ##@codestatus : internal
    
    envLogFile <- as.character(Sys.getenv("RUNIT_LOGFILE"))
    if (identical(envLogFile, "OFF")) {
      return(NULL)  ## in this case log messages are ignored
      
    } else if(identical(envLogFile, "CACHE")) {
      return(envLogFile)  ## use cached mode, i.e. write everything to an internal string
      
    } else if(identical(envLogFile, "")) {
      return(envLogFile) ## write to shell
      
    } else {
      if(file.exists(envLogFile)){
        if(file.access(envLogFile, mode=2) < 0){  ## file not writable
          msg<-paste("unable to write to log file", envLogFile, ", switching to CACHE mode")
          errorMsgStack <<- append(errorMsgStack, msg)
          errorStatus <<- "error"
          logString <<- paste(logString, "---------------------", sep="\n")
          logString <<- paste(logString, "ERROR OCCURED, message:",  sep="\n")
          logString <<- paste(logString, msg, sep="\n")
          logString <<- paste(logString, "---------------------", sep="\n")
          return("CACHE")
        }
        cat("\n\n ", initLog(), append=TRUE, file=envLogFile)
        cat("appended to previously existing file\n", append=TRUE, file=envLogFile)
        
      } else {
        success <- file.create(envLogFile)
        if(!success){  ## file can't be created, best we can do ist to switch to cached mode
          msg <- paste("unable to create log file", envLogFile, ", switching to CACHE mode")
          errorMsgStack <<- append(errorMsgStack, msg)
          errorStatus <<- "error"
          logString <<- paste(logString, "---------------------", sep="\n")
          logString <<- paste(logString, "ERROR OCCURED, message:",  sep="\n")
          logString <<- paste(logString, msg, sep="\n")
          logString <<- paste(logString, "---------------------", sep="\n")
          return("CACHE")
        }
        cat(initLog(), append=TRUE, file=envLogFile)
      }
      return(envLogFile)
    }
  }

 ## ------------------
  ## private data
  ## ------------------


  ## error
  errorMsgStack <- vector()
  errorStatus <- "noError"  ## can have the values 'noError', 'error' or 'fatal'


  ## debug (logical)
  DEBUG <- readLogicalEnv("RUNIT_DEBUG", default=FALSE) 
  SEALED <- readLogicalEnv("RUNIT_SEALED", default=FALSE)
  
  ## log file
  logString <- initLog()  ## in 'CACHE' mode everything is stored here
  logFile <- "CACHE"
  logFile <- initLogFile()

  ## progress bar
  progressFunctionName <- NULL

  ## ----------------------------------------
  ## Error Handling
  ## ----------------------------------------

  errorHandler <- function(where=environment()) {
    ##@bdescr
    ##  this function organizes the error handling
    ##@edescr
    ##@in  where : [environment]
    ##@ret       : []
    ##
    ##@codestatus : internal
    
    dump.frames();
    frameLimit <- length(last.dump) - 1
    log("\nCall Stack:")
    for(i in seq(length=frameLimit)) {
      log(paste(i, ": ", names(last.dump)[i]))
    }
  }
  options(error=errorHandler)  ## this creates a call stack for all calls to stop

  ## ----------------------------------------
  ## public interface
  ## ----------------------------------------

  handleError <- function(errorStat,  msg){
    ##@bdescr
    ##  this function organizes the error handling
    ##@edescr
    ##@in  errorStat : [character] string
    ##@in  msg       : [character] an optional message
    ##@ret           : [NULL|signal]
    ##
    ##@codestatus : testing
    
    errorStatus <<- errorStat
    errMsg <- NULL
    if(is.null(msg)) {
      errMsg <- errorStat
    }
    else {
      errMsg <- paste(errorStat, ":", msg)
    }
    errorMsgStack <<- append(errorMsgStack, errMsg)

    dump.frames();
    frameLimit <- length(last.dump)-1
    culprit <- names(last.dump)[frameLimit-1]
    log("---------------------")
    log(errorStat)
    log(paste("Location:", culprit))
    if(!is.null(msg)){
      log("Message:")
      log(msg)
    }


    if(identical(errorStatus, "FAILED ASSERTION")  | identical(errorStatus, "FATAL ERROR")) {
      stop(errMsg, call. = FALSE)
    } else {  ## in this case we do not call stop, so we still have to print the call stack
      log("\nCall Stack:")
      for(i in seq(along=frameLimit)) {
        log(paste(i, ": ", names(last.dump)[i]))
      }
      log("---------------------")
      return(invisible(NULL))
    }
  }


  clearErrorStatus <- function(where=environment()){
    ##@bdescr
    ##  this function clears the error status flag
    ##@edescr
    ##
    ##@in  where     : [environment] where to find and clear the flag
    ##@ret           : [NULL] used for its side effect
    ##
    ##@codestatus : testing
    
    errorStatus <<- "noError"
    return(invisible())
  }

  clearErrorMsgStack <- function(where=environment()){
    ##@bdescr
    ##  this function clears the error message stack
    ##@edescr
    ##
    ##@in  where     : [environment] where to find and clear the stack
    ##@ret           : [NULL] used for its side effect
    ##
    ##@codestatus : testing
    
    errorMsgStack <<- vector(mode="character")
    clearErrorStatus(where=where)
    return(invisible())
  }

  getErrorStatus <- function(where=environment()){
    ##@bdescr
    ##  returns the current error status 
    ##@edescr
    ##
    ##@in  where     : [environment] where to find the error status flag
    ##@ret           : [character] message
    ##
    ##@codestatus : untested
    
    return(errorStatus)
  }

  getErrorMsgStack <- function(where=environment()){
    ##@bdescr
    ##  returns the current error message stack
    ##@edescr
    ##
    ##@in  where     : [environment] where to find the stack
    ##@ret           : [character] vector of messages
    ##
    ##@codestatus : testing
    
    return(errorMsgStack)
  }

  getLastErrorMsg <- function(where=environment()){
    ##@bdescr
    ##  returns the last occured error message
    ##  or an empty string if no error has occured since last init.
    ##@edescr
    ##
    ##@in  where     : [environment] where to find the stack
    ##@ret           : [character] messages
    ##
    ##@codestatus : internal
    
    if(length(errorMsgStack) < 1) {
      return("")
    }
    return(errorMsgStack[length(errorMsgStack)])
  }

  log <- function(msg) {
    ##@bdescr
    ##  adds the provided string to the logstring
    ##@edescr
    ##
    ##@in  msg     : [character] vector of message(s)
    ##@ret         : [character] messages
    ##
    ##@codestatus : testing
    
    msg <- paste(msg, collapse="\n")
    if(identical(logFile, NULL)) {
      return(character(1))
    }
    else if(identical(logFile, "CACHE")){
      logString <<- paste(logString, msg, sep="\n")
    }
    else {
      cat(msg, "\n", append=TRUE, file=logFile)
    }
  }

  clearLogString <- function(where=environment()){
    ##@bdescr
    ##  reinitializes the global logstring
    ##@edescr
    ##
    ##@in  where     : [environment] where to find the logstring
    ##@ret           : [NULL] used for its side effect on global variable
    ##
    ##@codestatus : testing
    
    logString <<- initLog()
    return(invisible())
  }

  getLogString <- function(where=environment()){
    ##@bdescr
    ##  returns the current log string
    ##@edescr
    ##
    ##@in  where     : [environment] where to find the logstring
    ##@ret           : [character] current log
    ##
    ##@codestatus : testing
    
    return(paste(logString, collapse="\n"))
  }

  getLogFileName <- function(where=environment()) {
    ##@bdescr
    ##  returns the log file name
    ##@edescr
    ##
    ##@in  where     : [environment] where to find the logstring
    ##@ret           : [character] log file name
    ##
    ##@codestatus : testing
    
    return(logFile)
  }

  initProgress <- function(name) {
    ##@bdescr
    ##  this function initializes progress flag 
    ##@edescr
    ##
    ##@in  name      : [character] 
    ##@ret           : [NULL] 
    ##
    ##@codestatus : testing
    
    ASSERT(is(name, "character"), "'name' has to be of type 'character'.")
    ASSERT(length(name) == 1, "'name' has to be of length 1.")
    
    progressFunctionName <<- name
    
    return(invisible())
  }

  setProgress <- function(progress){
    ##@bdescr
    ##  this function adds the progress status for the currently defined process
    ##  set via initProgress to the global log stack
    ##@edescr
    ##
    ##@in  progress : [character] 
    ##@ret          : [NULL] 
    ##
    ##@codestatus : testing
    
    ASSERT(is(progress, "character"), "'progress' has to be of type 'character'.")
    ASSERT(length(progress) == 1, "'progress' has to be of length 1.")
    
    log(paste("PROGRESS of", progressFunctionName, ":", progress))
    return(invisible())
  }

  getDebug <- function()
  {
    ##@bdescr
    ##  returns the debug mode
    ##@edescr
    ##
    ##@ret : [logical] debug mode
    ##
    ##@codestatus : testing
    
    return(DEBUG)
  }

  setDebug <- function(debugMode)
  {
    ##@bdescr
    ##  sets the debug mode
    ##@edescr
    ##
    ##@in  debugMode : [logical] flag for debug mode
    ##@ret           : [NULL] used for its side effect on .GLOBAL
    ##
    ##@codestatus : testing
    
    ASSERT(is(debugMode,"logical") && (length(debugMode) == 1),
           "only logicals with length equals one allowed");
    ASSERT( !is.na(debugMode), "missing value not allowed.")
    
    DEBUG <<- debugMode

    return(invisible())
  }
  

  getSealed <- function() {
    ##@bdescr
    ##  returns the class definition sealed status
    ##@edescr
    ##
    ##@ret : [logical] sealed status
    ##
    ##@codestatus : internal
    
    return(SEALED)
  }

  if(!is.null(.GLOBAL)) {
    setFatalError("initGLOBAL called although '.GLOBAL' object had already been initialised")
  }
  

  return(invisible(list(getDebug=getDebug,
                        setDebug=setDebug,
                        getSealed=getSealed,
                        handleError=handleError,
                        clearErrorStatus=clearErrorStatus,
                        clearErrorMsgStack=clearErrorMsgStack,
                        getErrorStatus=getErrorStatus,
                        getLastErrorMsg=getLastErrorMsg,
                        getErrorMsgStack=getErrorMsgStack,
                        log=log,
                        clearLogString=clearLogString,
                        getLogString=getLogString,
                        getLogFileName=getLogFileName,
                        initProgress=initProgress,
                        setProgress=setProgress)))
}





