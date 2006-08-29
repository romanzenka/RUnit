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



##  -------------------------
##
##  1) constructor methods
##
##  -------------------------
.initialize.Array <- function(.Object, ...)
{
  ##@bdescr
  ##  generic constructor method
  ##  defines slot 'valid' with value TRUE
  ##@edescr
  ##
  ##@class    : [Array]
  ##
  ##@in  .Object : [Array]
  ##@ret         : [Array]
  ##
  ##@codestatus : internal
  
  .Object@valid <- TRUE
  .Object
}



##  -------------------------
##
##  2) accessor methods
##
##  -------------------------
getNames.Array <- function(obj)
{
  ##@bdescr
  ##  utility function
  ##  returns the names of the data slot
  ##@edescr
  ##
  ##@class    : [Array]
  ##
  ##@in  obj : [Array] object of class 'Array'
  ##@ret     : [character vector] names of slot 'data'
  ##
  ##@codestatus : testing
  
  return(names(obj@data))
}


setNames.Array <- function(obj, value)
{
  ##@bdescr
  ##  utility function
  ##  sets the names of the data slot
  ##@edescr
  ##
  ##@class    : [Array]
  ##
  ##@in  obj   : [Array] class object 
  ##@in  value : [character vector] names of slot 'data'
  ##@ret       : [Array] class object
  ##
  ##@codestatus : untested
  
  ASSERT (is(value, "character"), "value has to be of type 'character'.")
  ASSERT( length(value) == getLength(obj),
         "value has to be of same length as the Array object.")
  names(obj@data) <- value

  return(obj)
}


getLength.Array <- function(obj)
{
  ##@bdescr
  ##  utility function
  ##  return the length of the data slot
  ##@edescr
  ##
  ##@class    : [Array]
  ##
  ##@in  obj : [Array] object of class 'Array'
  ##@ret     : [integer] length of slot 'data'
  ##
  ##@codestatus : testing
  
  ##  R concept, slot data is treated as a list and thus the standard function length()
  ##  works, but only if not overwritten by a class object.
  return(length(obj@data))
}


setElement.Array <- function(x,i,j,...,value)
{
  ##@bdescr
  ##  replacement method
  ##  default call for operator overloading
  ##@edescr
  ##
  ##@class    : [Array]
  ##
  ##@in x     : [Array] object of class 'Array'
  ##@in i     : [integer] row index
  ##@in j     : [integer] column index
  ##@in value : [real] value to be set
  ##@ret      : [Array]
  ##
  ##@codestatus : testing

  ##  precondition check
  ##  check only if value is not a basic type
  if (!is.vector(value)) {
    if(!verifyObject(value)) {
      
      msg <- paste("invalid",is(value)[1],"object: set method failed.")
      setFatalError(msg)
    }
  }
  
  return(setElement(x,i,value))
}


getElementByInteger.Array <- function(x,i,j,drop=FALSE,...)
{
  ##@bdescr
  ##  gets an element by an integer index (as the name of the function said)
  ##@edescr
  ##
  ##@class    : [Array]
  ##
  ##@in  x    : [Array] object of class 'Array'
  ##@in  i    : [integer] row index
  ##@in  drop : [logical] not yet used (!)
  ##@ret      : [ANY] an element of the array

  if (!missing(j)) {
    setFatalError("[i,j] not allowed for Array objects.")
  }
  
  if(length(i) == 1) {
    
    return(x@data[[i]])
    
  } else {
    
    ans <- new(class(x))
    for(j in seq(length=length(i))) {
      
      ans@data[[j]] <- x@data[[i[j]]]
    }
  }
  return(ans)
}


getElementByLogical.Array <-  function(x,i,j,drop=FALSE,...){
  ##@bdescr
  ## gets an element by a logical index (as the name of the function said)
  ##@edescr
  ##
  ##@class    : [Array]
  ##
  ##@in  x    : [Array] object of class 'Array'
  ##@in  i    : [integer] row index
  ##@in  drop : [logical] not yet used (!)
  ##@ret      : [ANY] an element of the array

  if (!missing(j)) {
    setFatalError("[i,j] not allowed for Array objects.")
  }
  
  ans <- new(class(x))
  if(length(i) == 1) {
    
    i <- rep(i, length(x@data))
  }
  ans@data <- x@data[i]
  return(ans)
}


getElementByNumeric.Array <- function(x,i,j,drop=FALSE,...)
{
  ##@bdescr
  ## gets an element by an numeric index (as the name of the function said)
  ##@edescr
  ##
  ##@class    : [Array]
  ##
  ##@in  x    : [Array] object of class 'Array'
  ##@in  i    : [integer] row index
  ##@in  drop : [logical] not yet used (!)
  ##@ret      : [ANY] an element of the array

  if (!missing(j)) {
    setFatalError("[i,j] not allowed for Array objects.")
  }
  
  if(length(i) == 1) {
    
    return(x@data[[i]])
    
  } else {
    
    ans <- new(class(x))
    for(j in seq(length=length(i))) {
      ans@data[[j]] <- x@data[[i[j]]]
    }
  }
  
  return(ans)
}


getElementByCharacter.Array <- function(x,i,j,drop=FALSE,...)
{
  ##@bdescr
  ## get an array element by character (as the name of the function said)
  ##@edescr
  ##
  ##@class    : [Array]
  ##
  ##@in  x    : [Array] object of class 'Array'
  ##@in  i    : [character] name of element
  ##@in  drop : [logical] not yet used (!)
  ##@ret      : [ANY] returns an array element
  ##
  ##@codestatus : 
  
  if (!missing(j)) {
    setFatalError("[i,j] not allowed for Array objects.")
  }
  
  ## check for exact match
  ## note an invariant of the array construct are
  ## unique names for the elements

  ## only one name given
  if(length(i) == 1) {
    idx <- which(names(x@data) == i)
    if(length(idx) == 0) {
      ASSERT(FALSE,
             paste("Couldn't find specified element name '",i,
                   "' in provided array: check your spelling.", sep=""))
    } else {
      return(eval(parse(text=paste("x@data$\"",i,"\"",sep=""))))
    }
  } else {
    ## more than one name given
    ## check for matches
    idx <- match(i,names(x@data))
    ASSERT(!any(is.na(idx)),
           paste("Couldn't find specified element name(s) '",
                 paste(i[is.na(idx)], collapse="', '"),
                 "' in provided array: check your spelling.", sep=""))
    ## more than one match -> return an array
    ans <- new(class(x));
    ans@data <- x@data[i];
  }
  
  return(ans);
}


setElement.Checked.Value.Array <- function(x,i,j,...,value)
{
  ##@bdescr
  ##  accessor method
  ##  set i'th chunk of array to provided value (array)
  ##  1) allows to set chunk to NULL
  ##  2) checks if class of provided value (array) for slot i matches
  ##     Array class of parent object.
  ##@edescr
  ##
  ##@class    : [Array]
  ##
  ##@in  x     : [Array]
  ##@in  i     : [integer|character]
  ##@in  value : [ANY]
  ##@ret       : [Array] object of class '*Array' with new asignment commited

  if (!missing(j)) {
    setFatalError("[i,j]<- not allowed for Array objects.")
  }
  
  ## check for NULL -> remove element
  if(is(value,"NULL")) {
    if(is(i,"character")) {
      
      ## check for absolut equal names!!!
      if(any(i == names(x@data))) {
        
        eval(parse(text=paste("x@data$\"",i,"\" <- value",sep="")))
      }
      
    } else {
      
      x@data[[i]] <- value
    }
    
  } else {
    
    ## get the type of the array elements
    arrayType <- gsub("Array$","",is(x)[1]);
    
    ASSERT(is(value,arrayType),"Type error in Array");
    
    ## check for absolut equal names
    if(is(i,"character")){
      ## check for absolut equal names!!!
      if(!any(i == names(x@data))) {
        
        eval(parse(text=paste("x@data$\"",i,"\" <- value",sep="")))
        return(x)
        
      } else {
        
        eval(parse(text=paste("x@data$\"",i,"\" <- value",sep="")))
        return(x)
      }
    }
    
    x@data[[i]] <- value
  }
  return(x)
}


isValid.Array <- function(obj)
{
  ##@bdescr
  ##  generic validation flag accessor
  ##@edescr
  ##
  ##@class    : [Array]
  ##
  ##@in  obj  : [Array] object of class 'Array'
  ##@ret      : [logical] is state valid, as specified by the slot 'valid'
  ##
  ##@codestatus : testing

  return(obj@valid)
}




##  -------------------------
##
##  3) compute/modify methods
##
##  -------------------------
applyFun.Array <- function(obj,para1, ...)
{
  ##@bdescr
  ## apply a function to every element of an Array
  ##@edescr
  ##
  ##@class : [Array]
  ##
  ##@in  obj   : [Array] an array
  ##@in  para1 : [function] a function, first parameter is one element of the array
  ##@in  ...   : [ANY] other parameters for the function
  ##@ret       : [list] a list of results
  ##
  ##@codestatus : testing
  
  ##  compute length once as this value is required several times
  objLength <- getLength(obj)
  
  ## create result vector
  res <- vector(mode="list",length=objLength)

  ## iterate over the array
  for(i in seq(length=objLength)) {
    
    res[[i]] <- para1(obj[i], ...)
  }

  ## check for equal length
  ## R removes elements of with NULL
  if(length(res) == objLength) {
    
    names(res) <- getNames(obj)
  }

  return(res)
}


applyFun2.Array <- function(obj,para1,para2, ...)
{
  ##@bdescr
  ## apply a function to two parallel arrays
  ##@edescr
  ##
  ##@class    : [Array]
  ##
  ##@in  obj   : [Array] the first array
  ##@in  para1 : [Array] the second array  
  ##@in  para2 : [function] a function, first parameter is an elemenet of the first array, second parameter is element of the second array
  ##@in  ...   : [ANY] other parameters for the function
  ##@ret       : [list] the list of results
  ##
  ##@codestatus : testing
  
  ##  compute length once as this value is required several times
  objLength <- getLength(obj)
  para1Length <- getLength(para1)
  
  ASSERT(objLength == para1Length, "Arrays must have the same length")
  
  ## create result vector
  res <- vector(mode="list",length=objLength)
  
  ## iterate over the array
  for(i in seq(length=objLength)) {
    
    res[[i]] <- para2(obj[i],para1[i], ...)
  }
  
  return(res)
}


applyFunV.Array <- function(obj,para1,para2, ...)
{
  ##@bdescr
  ## apply a function to an array and a vector
  ##@edescr
  ##
  ##@class : [Array]
  ##
  ##@in  obj   : [Array] the first array
  ##@in  para1 : [vector] an additional vector  
  ##@in  para2 : [function] a function, first parameter is an elemenet of the first array, second parameter is element of the second array
  ##@in  ...   : [ANY] other parameters for the function
  ##@ret       : [list] the list of results
  ##
  ##@codestatus : testing
  
  objLength <- getLength(obj)
  
  ASSERT(objLength == length(para1), "Array and vector must have the same length")
  
  ## create result vector
  res <- vector(mode="list",length=objLength)

  ## iterate over the array
  for(i in seq(length=objLength)) {
    
    res[[i]] <- para2(obj[i],para1[i], ...)
  }
  
  return(res)
}


reduce.Array <- function(obj,para1, ...)
{
  ##@bdescr
  ## take two elements of an array and creates one object by a binary operation
  ##@edscr
  ##
  ##@class : [Array]
  ##  
  ##@in  obj   : [Array] an array
  ##@in  para1 : [function] a function, first element is an element of the array
  ##@in  ...   : [ANY] other parameters for the function
  ##@ret       : [ANY] 
  ##
  ##@codestatus : testing
 
  
  objLength <- getLength(obj)
  
  ## no elements in array, return NULL
  if (objLength == 0) {
    
    return(NULL)
    
  } else if(objLength == 1) {
    
    ## return first element, if only one element in array
    return(obj[1])
    
  } else{
    
    ## do the calculation
    res <- obj[1];
    for(i in 2:objLength) {
      
      res <- para1(res,obj[i],...)
    }
    return(res)
  }
}


concat.Array <- function(obj,para1) {
  ##@bdescr
  ## combine two arrays of exactly the same type
  ## values of para1 will be append to obj
  ## the interface ensures type safety
  ##@edescr
  ##
  ##@class : [Array]
  ##
  ##@in obj       : [Array] the array on which to invoke the method
  ##@in para1     : [Array] the array to append
  ##@in recursive : [ANY] not used
  ##@ret          : [Array] the combined array
  ##
  ##@codestatus : testing

  ## append the data slot
  obj@data <- c(obj@data,para1@data)
  
  return(obj)
}


pushBack.Array <- function(obj,value){
  ##@bdescr
  ## append an element at the end of the array
  ##@edescr
  ##
  ##@class    : [Array]
  ##
  ##@in obj   : [Array] the array
  ##@in value : [ANY] the element to append
  ##@ret      : [Array] modified object
  ##
  ##@codestatus : testing

  obj@data <- c(obj@data,list(value))
  
  return(obj)
}


isEqual.Array <- function(obj, para1) {
  ##@bdescr
  ##  comparison operator, checks equivalence, ignoring attributes
  ##  relies on isEqual method implemented for the Array type class
  ##@edescr
  ##
  ##@class    : [Array]
  ##
  ##@in  obj   : [Array] class object
  ##@in  para1 : [Array] class object
  ##@ret       : [logical] TRUE if the two Arrays are equivalent
  ##
  ##@codestatus : testing
 
  ##  precondtions
  ASSERT( verifyObject(obj), 
         paste("validation of first input",is(obj)[1], "class object failed:\n",
         .GLOBAL$getLastError()))
  ASSERT( verifyObject(para1), 
         paste("validation of first input",is(para1)[1], "class object failed:\n",
         .GLOBAL$getLastError()))
  
  ##  applyFun returns list, for isEqual lsit of 1-element logical vectors
  ##  in case of empty Arrays list() is returned, which evaluates to all(list()) == TRUE
  ##  concistent with default R behaviour
  ret <- all(applyFun(obj, para1, isEqual))
  return(ret)
}
 
 


##  -------------------------
##
##  4) print/validate methods
##
##  -------------------------
printObject.Array <- function(x)
{
  ##@bdescr
  ##  generic print method
  ##@edescr
  ##
  ##@class    : [Array]
  ##
  ##@in  x   : [Array] object of class 'Array'
  ##@ret     : [Array] returned invisible
  ##
  ##@codestatus : testing
  
  msg <- paste("An object of class '",is(x)[1],"':",sep="")
  cat("\n",msg,sep="")
  cat("\n", rep("-", nchar(msg)),sep="")
  cat("\n provides")
  cat("\n Slot 'valid': ", x@valid)
  cat("\n Slot 'data':\n")
  objLength <- getLength(x)
  if (objLength > 0) {
    for(i in 1:objLength) {
      
      res <- print(x@data[[i]])
    }
    
  } else {
    
    cat("empty\n")
  }
  
  return(invisible(x))
}


verifyObject.Array <- function(obj)
{
  ##@bdescr
  ##  generic validation method
  ##@edescr
  ##
  ##@class    : [Array]
  ##
  ##@in obj  : [Array] object to be validated
  ##@ret     : [logical] TRUE if object is a valid class member or rather a member of a derived class
  ##
  ##@codestatus : testing

  if (length(obj@valid) != 1) {
    setError("invalid 'Array' object: slot 'valid' is not of length == 1.\n")
    return(FALSE)
  }
  if (!is(obj@data, "list")) {
    setError("invalid 'Array' object: slot 'data' is not of basic class 'list'.\n")
    return(FALSE)
  }
   
  return(TRUE)
}



.defineArrayMethods <- function(where=environment())
{
  ##@bdescr
  ## Initialization Function
  ## Not to be called by user(s).
  ## Method definitions for classes 'Array'.
  ##@edescr

  if (.GLOBAL$getDebug()) {
    
    cat(".defineArrayMethods ... ")
  }

  
  ##  1) constructor methods
  setMethod("initialize", signature("Array"), .initialize.Array, where=where)

  
  ##  2) accessor methods
  defineMethod("isValid", c("Array"), isValid.Array, where=where)

  defineMethod("getLength", "Array", getLength.Array, where = where)
  ##  operator for get by numeric mask
  setMethod("[",signature=signature("Array","numeric"),
            getElementByNumeric.Array, where = where)
  
  ##  operator for get by character mask
  setMethod("[",signature=signature("Array","character"),
            getElementByCharacter.Array, where = where)

  ##  operator for get by integer mask
  setMethod("[",signature=signature("Array","integer"),
            getElementByInteger.Array, where = where)
  
  ##  operator for get by logical mask
  setMethod("[",signature=signature("Array","logical"),
            getElementByLogical.Array, where = where)

  ##  register this replace function
  setReplaceMethod("[",c("Array"),setElement.Checked.Value.Array,
                   where = where)

  ##  names of array list
  defineMethod("getNames", "Array", getNames.Array, where = where)
  defineSetMethod("setNames", c("Array", "character"),
                  setNames.Array, where = where)

  
  ##  3) modify methods
  defineMethod("applyFun",c("Array","Array","function", "missing"),
               applyFun2.Array, where = where,addEllipse=TRUE)
                             
  defineMethod("applyFun",c("Array","vector","function"),applyFunV.Array,
               where = where,addEllipse=TRUE)
  defineMethod("applyFun",c("Array","function"),applyFun.Array,
               where = where,addEllipse=TRUE)
  
  ## reduce function
  defineMethod("reduce",c("Array","function"),reduce.Array,
               where = where,addEllipse=TRUE)

  defineMethod("isEqual", c("Array", "Array"), isEqual.Array,
               where = where)

  
  ##  4) print/validate methods
  setMethod("print",  signature("Array"), printObject.Array, where=where)
  defineMethod("verifyObject", c("Array"), verifyObject.Array, where=where)


  
  if (.GLOBAL$getDebug()) {
    
    cat("o.k.\n")
  }
}




##  ------------------------------------
##
##  5) class & method generator methods
##
##  ------------------------------------
.defineArrayClassInternal <- function(className, type, where=environment())
{
  ##bdescr
  ##  generic class and method definition routine
  ##  does not return anything, called for its side effects
  ##@edescr
  ##
  ##@in className : [character]
  ##@in type      : [character]
  ##@in where     : [environment]
  ##
  ##@codestatus : internal

  ##  preconditions
  ASSERT( is(className, "character"), "'className' has to be of type 'character'.")
  ASSERT( length(className) == 1, "'className' has to be of length 1.")
  ASSERT( is(type, "character"), "'type' has to be of type 'character'.")
  ASSERT( length(type) == 1, "'type' has to be of length 1.")


  ##
  ##  class definition
  ##  
  if (.GLOBAL$getDebug()) {
    
    cat("define",className,"Class ... ",sep="")
  }
  
  ## define a class , this is done in the global environment by default
  setClass(className,
           representation("Array"),
           prototype(valid = TRUE), 
           contains = as.character("Array"),
           validity = NULL,
           sealed   = TRUE,
           where    = where)
  
  if (.GLOBAL$getDebug()) {
    
    cat("o.k.\n")
  }
  

  
  ##
  ##  method definitions
  ##   
  
  ##  define method for verifyObject
  defineMethod("verifyObject", signature(className), function(obj) {

    ##  call parent method
    ok <- callNextMethod()  
    if (!ok) {
      setError("validation for parent class failed.")
      return(FALSE)
    }
    ##  overloaded length, length(obj@data)
    len <- getLength(obj)
    if (len == 0) {
      return(TRUE)
    }
    
    classDef <- getClass(class(obj))
    slotTypes <- getProperties(classDef)
    slotNames <- names(slotTypes)

    ##  check for basic R type
    ##  assumes at least one element is set
    if (is(obj[1], "vector")) {
      return(TRUE)
    }
    
    for (i in 1:len) {

      ##  test validity
      if (!verifyObject(obj[i]))
      {
        errMsg <- paste("invalid '",is(obj)[i],"' object: data for slot ",slotNames[i],
                        " is not a valid '", is(obj[i]),"' object:\n",
                        .GLOBAL$getLastErrorMsg(), sep="")
        setError(errMsg)
        return(FALSE)
      }
    }
    
    ##  all tests passed
    return(TRUE)
  },
               where = where)


  ## print method
  setMethod("print", signature(className), printObject.Array,
            where = where)

  ## combine arrays of same type
  defineMethod("concat",signature(className,className),concat.Array,
            where = where)

  ## append an element at the end of the array
  defineSetMethod("pushBack", c(className,type), 
                pushBack.Array, where=where)
  
}


defineArrayClass <- function(type, where=environment())
{
  ##@bdescr
  ## creates an array and a constructor
  ## and type safe [] operators
  ## seal avoid double definitions by setClass
  ## name of constructor is new+type+Array e.g. newintegerArray, new AnnotationArray
  ##@edescr
  ##
  ##
  ##@status   : [internal function]
  ##
  ##@in  type : [character] class name prefix, i.e. "<type>Array" will be the new class name
  ##@in where : [environment]
  ##@ret      : [character] complete class name, i.e. '<type>Array'
  ##
  ##@codestatus : untested
  
  ## create the class name
  className <- paste(type,"Array",sep="");

  ind <- which(getClasses() == className)
  if(length(ind) >= 1) {
    ## class already exists
    return(className)
  }
  
  .defineArrayClassInternal(className, type, where=where)
  
  ## generate constructor
  constrFunction <- paste("new",className," <- function(...){ return(new(\"",
                          type,"Array\",...))}",sep="")
  
  ## eval and parse in the global environment ? Good or bad ?
  eval(parse(text=constrFunction),envir=where)

  ## init function for constructors
  initFunc <-
    paste("initialize.",className," <- function(.Object,...){",
          "return(.construct",className,"(.Object,...))\n}",sep="")

  setMethod("initialize",className,
            eval(parse(text=initFunc),envir=where),where=where)

  listConstr <- paste(".construct",className,".list <- function(obj,para1){\n",
                      "ASSERT(all(lapply(para1,function(elem) is(elem,\"",type,
                      "\"))),\"Type Check failed\")\n",
                      "obj@data <- para1;\n",
                      "return(obj);\n}", sep="")

  defineMethod(paste(".construct",className,sep=""), c(className,"list"),
               eval(parse(text=listConstr),envir=where), where=where)

  
  emptyConstr <- paste(".construct",className,".empty <- function(obj){\n",
                       "return(obj)\n}",sep="")

  defineMethod(paste(".construct",className,sep=""), className,
               eval(parse(text=emptyConstr),envir=where), where=where)

  
  ## return the name of the created class
  return(className)
}
