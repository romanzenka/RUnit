##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003-2006  Thomas Koenig, Matthias Burger
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


defineSetMethod <- function(methodName,
                            paraTypes,
                            definition,
                            where = environment(),
                            useAsDefault = TRUE
                            )
{
  ##@bdescr
  ## The purpose of this function is to make set method definitions a bit more convenient.
  ## It is important to note that the signature of the supplied method definition must exactly
  ## coincide with the signature supplied in the signature argument.
  ## This is the main function for the definition of set functions in RUnit.
  ## The function checks, that the method name does not conflict with S3 generics.
  ## The internal R name of the set function is 'methodName<-', e.g. setX<-.
  ##@edescr
  ##
  ##@in methodName   : [character] name of the method
  ##@in paraTypes    : [character] vector, types of parameters
  ##@in definition   : [function] definition of the method
  ##@in where        : [environment] where the method should be defined, by default set to .GlobalEnv
  ##@in useAsDefault : [logical] pass argument to call setGeneric, see help documentation there
  ##
  ##@codestatus : untested

  ##  preconditions delegated to internal function
  .defineMethodImpl(methodName,
                    paraTypes,
                    definition,
                    where        = where,
                    useAsDefault = useAsDefault,
                    compile      = FALSE,
                    setFunction  = TRUE
                    )
                    
}


defineMethod <- function(methodName,
                         paraTypes,
                         definition,
                         where        = environment(),
                         useAsDefault = TRUE,
                         valueClass   = NULL,
                         addEllipse   = FALSE
                         )
{
  ##@bdescr
  ## The purpose of this function is to make method definitions a bit more convenient.
  ## It is important to note that the signature of the supplied method definition must exactly
  ## coincide with the signature supplied in the signature argument.
  ## This is the main fanction for the definition of generic functions in RUnit.
  ## The function checks, that the method name does not conflict with S3 generics.
  ##@edescr
  ##
  ##@in methodName   : [character] name of the method
  ##@in paraTypes    : [character] vector, types of parameters
  ##@in definition   : [function] definition of the method
  ##@in where        : [environment] where the method should be defined, by default set to .GlobalEnv
  ##@in useAsDefault : [logical] pass argument to call setGeneric, see help documentation there
  ##@in addEllipse   : [logical] append ... to the signature of the function
  ##@in valueClass   : [character] type of return value (will be automically checked) by R
  ##
  ##@codestatus : untested

  .defineMethodImpl(methodName,
                    paraTypes,
                    definition,
                    where        = where,
                    useAsDefault = useAsDefault,
                    valueClass   = valueClass,
                    compile      = FALSE,
                    addEllipse   = addEllipse
                    )
                    
}


.defineMethodImpl <- function(methodName,
                              paraTypes,
                              definition,
                              where        = environment(),
                              useAsDefault = TRUE,
                              valueClass   = NULL,
                              compile      = FALSE,
                              addEllipse   = FALSE,
                              setFunction  = FALSE)
{
  ##@bdescr
  ## The purpose of this function is to make method definitions a bit more convenient.
  ## It is important to note that the signature of the supplied method definition must exactly
  ## coincide with the signature supplied in the signature argument.
  ##@edescr
  ##
  ##@in methodName   : [character] name of the method
  ##@in paraTypes    : [character] vector, types of parameters
  ##@in definition   : [function] definition of the method
  ##@in where        : [environment] where the method should be defined, by default set to .GlobalEnv
  ##@in useAsDefault : [logical] pass argument to call setGeneric, see help documentation there
  ##@in valueClass   : [character] type of return value (will be automically checked) by R
  ##@in compile      : [logical] if TRUE byte compile the function definition before supplying it to the setMethod call (highly experimental, not to be used by standard users)
  ##
  ##@in addEllipse   : [logical] should ... append at the end of the signature
  ##@in setFunction  : [logical] set function (TRUE) or normal function(FALSE)
  ##@ret             : [NULL] used for its side effect
  ##
  ##@codestatus : internal/testing

  ##  preconditions
  ASSERT(length(methodName) == 1, "'methodName' has to be of length 1.")
  ASSERT(is.character(methodName), "'methodName' has to be of type 'character'.")
  ## ASSERT(is.character(paraTypes))
  ASSERT(is.function(definition), "'definition' has to be of type 'function'.")
  ASSERT(is.environment(where), "'where' has to be of type 'environment'.")
  ASSERT(length(useAsDefault) == 1, "'useAsDefault' has to be of length 1.")
  ASSERT(is.logical(useAsDefault), "'useAsDefault' has to be of type 'logical'.")
  ASSERT(length(valueClass) <= 1, "'valueClass' has to be of length 1.")
  ASSERT(is.character(valueClass) || is.null(valueClass), "'valueClass' has to be either of type 'character' or NULL.")
  ASSERT(length(compile) == 1, "'compile' has to be of length 1.")
  ASSERT(is.logical(compile), "'compile' has to be of type 'logical'.")
  ASSERT(length(addEllipse) == 1, "'addEllipse' has to be of length 1.")
  ASSERT(is.logical(addEllipse), "'addEllipse' has to be of type 'logical'.")
  ASSERT(is(setFunction,"logical"), "'setFunction' has to be of type 'logical'.")
  ASSERT(length(setFunction) == 1, "'setFunction' has to be of length 1.")

  ## create a set function or a normal function
  if(setFunction) {
    
    if(length(paraTypes) < 2) {
      
      errTxt <- "Warning: set functions need at least two parameters\n"
      errTxt <- paste(errTxt,"for:",methodName,"only",length(paraTypes),"found.\n")
      errTxt <- paste(errTxt,"No generic S4 replacemethod defined!\n")
      setWarning(errTxt)
      return(NULL)

    } else {
      
      ## create the new method name
      methodName <- paste(methodName,"<-",sep="")
      ## create vector for variable names
      varname <- c("obj",paste("para",1:(length(paraTypes)-1),sep=""))
      ## last parameter must be value
      varname[length(varname)] <- "value"
    }
    
  } else {
    
    ##  change this parameter, if it is necessary (including the first)  
    if(length(paraTypes) > 1)
    {
      ## create vector for variable names
      varname <- c("obj",paste("para",1:(length(paraTypes)-1),sep=""))

    } else {
      
      varname <- "obj"
    }
  }
  ## create argument list for generic function
  arglist <- paste(varname,collapse=",")

  ## create signature of S4 function
  signat <-as.list(paraTypes)
  names(signat) <- varname

  
  ## check for S4 Generic
  if(.isGenericS3(methodName,where=where)) { ## check for  S3 generic
    
    genFormals <- formals(get(methodName,envir=where))
    errTxt <- "Warning : Tried to define a S4 method for S3 generic.\n"
    funcName <- as.character(substitute(definition))
    errTxt <- paste(errTxt,"S4 function definition (class",paraTypes[1],")\n")
    errTxt <- paste(errTxt,"clashes with\n")
    errTxt <- paste(errTxt,"S3 function name :", methodName,"\n")
    errTxt <- paste(errTxt,"No generic S4 method defined!\n\n")
    setWarning(errTxt)
    return(NULL)
    
  } else if(!isGeneric(methodName,where=where)) {
    
    ## should we add additional parameters
    retSig <- ""
    if(!is.null(valueClass)) {
      
      retSig <- paste(",valueClass=",valueClass,sep="")
    }

    ## add ... at the end of the parameter list
    if(addEllipse==TRUE) {
      
      arglist <- paste(arglist,",...",sep="")
    }
    funcString<-paste("setGeneric(\"", methodName,"\",",
                      "function(", arglist,"){\n  value <- standardGeneric(\"",
                      methodName, "\");\n  return(value);\n}",
                      ",useAsDefault=", as.character(useAsDefault),
                      ",where=topenv()",
                      retSig,
                      ")",sep="")
    
    eval(parse(text=funcString),envir=where)
    
  } else {
   
    ## get formals of generic
    genFormals <- formals(get(methodName,envir=where))
    
    ## do not count ...
    nrDefaults <- sum(names(genFormals) != "...")

    
     nrMissingArg <- nrDefaults- length(varname)
     if(nrMissingArg > 0) {
       
       signat <- c(signat,rep("missing",nrMissingArg))
       addPara <- paste("para",length(varname):(length(signat)-1),sep="")
       names(signat)[names(signat) == ""] <- addPara
     }
  }

  names(signat) <- NULL

  if(isTRUE(compile)) {
    
    ## highly experimental
    ASSERT(require(compiler), "loading package 'compiler' failed.")
    byteCode <- cmpfun(definition)
    setMethod(methodName, signature=signat, byteCode, where=where)

  } else {
    
    res <- try(setMethod(methodName, signature=signat, definition, where=where),silent=TRUE)

    if(class(res) == "try-error") {
      
      cat("Error in set Method.\n")
      cat("R Message was:\n",geterrmessage(),"\n")
      cat("Name of the method:",methodName,"\n")
      cat("Signature: ",methodName,"(",paste(unlist(signat),collapse=","),")\n")
      cat("No generic S4 method defined!\n")
    }
  }
  
  return(NULL)
}


.isGenericS3 <- function(funcName,where)
{
  ##@bdescr
  ## check for S3 generic from (http://www.maths.lth.se/help/R/setGenericS3/#[1]) but modified
  ##@edescr
  ##
  ##@in  funcName  : [character] name of the method/function to check
  ##@in  where     : [environment] passed on to get (?get)
  ##@ret           : [logical] TRUE iff any S3 generic method is found in the search path
  ##
  ##@codestatus : internal

  if(exists(funcName,where=where)) {
    
    ## get character representation of the function
    src <- as.character(deparse(body(get(funcName,envir=where))))
    ## grep for UseMethod
    return(any(regexpr("UseMethod", src) != -1))
  }
  return(FALSE)
}


.isGenericS4 <- function(funcName,where)
{
  ##@bdescr
  ## check for S4 generic (from http://www.maths.lth.se/help/R/setGenericS3/#[1]) but modified
  ##@edescr
  ##
  ##@in  funcName  : [character] name of the method/function to check
  ##@in  where     : [environment] passed on to get (?get)
  ##@ret           : [logical] TRUE iff any S4 generic method is found in the search path
  ##
  ##@codestatus : internal

  if(exists(funcName,where=where)) {
    
    ## get character representation of the function
    src <- as.character(deparse(body(get(funcName,envir=where))))
    ## grep for standardGeneric
    return(any(regexpr("standardGeneric", src) != -1))
  }
  return(FALSE)
}


.isObjectS4 <- function(obj, where)
{
  ##@bdescr
  ##  checks if provided object is a S4 class object
  ##
  ##  Matin Maechler R devel 7/02/2006
  ##  'There's a relatively simple check from R code which we've
  ##   using for str()'
  ##@edescr
  ##
  ##@in  obj       : [ANY] class object to be checked
  ##@in  where     : [environment] unused
  ##@ret           : [logical] TRUE iff any S4 generic method is found in the search path
  ##
  ##@codestatus : internal

  if (!is.null  (cl <- attr(obj, "class")) &&
                 (!is.null(attr(cl, "package")) ||
              cl == "classRepresentation")) {

    return(TRUE)
  }
  return(FALSE)
}


isEqual.undefined <- function(obj, para1) {
  ##@bdescr
  ##  comparison operator, checks equivalence, ignoring attributes
  ##@edescr
  ##
  ##@class    : []
  ##
  ##@in  obj   : [] class object
  ##@in  para1 : [] class object
  ##@ret       : [logical] TRUE if the two Arrays are equivalent
  ##
  ##@codestatus : testing
 
  ##  precondtions
  
  setFatalError("isEqual is not defined for this signature.")
}


isEqual.all.equal <- function(obj, para1) {
  ##@bdescr
  ##  comparison operator, checks equivalence, ignoring attributes
  ##  see methods("all.equal") for currently covered types for this
  ##  call
  ##  includes 'complex'
  ##@edescr
  ##
  ##@class    : []
  ##
  ##@in  obj   : [] class object
  ##@in  para1 : [] class object
  ##@ret       : [logical] TRUE if the two Arrays are equivalent
  ##
  ##@codestatus : testing
 
  ##  precondtions
  
  return(isTRUE(all.equal(obj, para1)))
}


.defineIsEqualBaseTypeMethod <- function(where=environment()) {

  defineMethod("isEqual", c("ANY", "ANY"),
               isEqual.undefined, where=where)

  defineMethod("isEqual", c("integer", "integer"),
               isEqual.all.equal, where=where)

  defineMethod("isEqual", c("numeric", "numeric"),
               isEqual.all.equal, where=where)

  defineMethod("isEqual", c("character", "character"),
               isEqual.all.equal, where=where)
              
  defineMethod("isEqual", c("complex", "complex"),
               isEqual.all.equal, where=where)

   defineMethod("isEqual", c("factor", "factor"),
               isEqual.all.equal, where=where)
               
   defineMethod("isEqual", c("formula", "formula"),
               isEqual.all.equal, where=where)
                 
  defineMethod("isEqual", c("list", "list"),
               isEqual.all.equal, where=where)

  defineMethod("isEqual", c("POSIXt", "POSIXt"),
               isEqual.all.equal, where=where)
               
  defineMethod("isEqual", c("matrix", "matrix"),
               isEqual.all.equal, where=where)

}
