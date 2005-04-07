##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003, 2004  Thomas Koenig, Matthias Burger, Klaus Juenemann
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


includeTracker <- function(fbody)
{
  ##@bdescr
  ##  Internal function
  ##
  ##@edescr
  
  ## get the signature
  sig <- fbody[1]


  ## get the block structure (important for if, for, while, else with one line)
  block <- sapply(fbody[-1],function(x) regexpr("[^ ]",x)[1], USE.NAMES=FALSE)

  ## vector of keywords
  loopKW <- c("for|while|repeat")


  ## list of keywords
  kwOpen <- c("for","while","repeat","if","else")
  kwEnd <- c("else")

  ## keyword at begin
  kwGrep <- paste("(",paste(kwOpen,sep="",collapse="|"),")",sep="")


  oneLiner <- function(code)
  {
    ##@bdescr
    ##  utility
    ##  search a caracter vector ie. the vector of lines of a function body
    ##  for block structures e.g. for|while|repeat|if|else { } code block
    ##@edescr
    ##
    ##@in  code : [character] vector of function body code lines
    ##@ret      : [logical] vector of length of code, indication which are one line control blocks
    
    return(sapply(code,
                  function(line)
                  {
                    opBr <- grep(paste("^[ ]*",kwGrep," .*[ ]+$",sep=""),line)
                    if(length(opBr) > 0)
                    {
                      return(TRUE)
                    }
                    return(FALSE)
                  },USE.NAMES=FALSE))
  }

  
  
  ## set Brackets
  setBrackets <- function(potLine,block,env)
  {
    ##@bdescr
    ##
    ##@edescr
    ##
    ##@in  potLine : [character] code text line
    ##@in  block   : [integer] vector
    ##@in  env     : [logical]
    ##@ret  : [list] with mathing element vectors: openBr & clodeBr
    
    oBr <- character(length(potLine))
    clBr <- character(length(potLine))
    
    lineIdx <- 1
    while(lineIdx < length(potLine))
    {
      if(potLine[lineIdx] && !(potLine[lineIdx+1]))
      {
        oBr[lineIdx] <- "{"
        if (!env[lineIdx+1])
        {
          clBr[lineIdx+2] <- paste(clBr[lineIdx+2],"}")
        }
        else
        {
          bbl <- block[lineIdx]
          endBlockIdx <- min(which((bbl >= block) & ((1:length(block)) > lineIdx)))
          clBr[endBlockIdx] <- paste(clBr[endBlockIdx],"}")
        }
      }
      if(potLine[lineIdx] && (potLine[lineIdx+1]))
      {
        oBr[lineIdx] <- "{"
        bbl <- block[lineIdx]
        endBlockIdx <- min(which((bbl >= block) & ((1:length(block)) > lineIdx)))
        clBr[endBlockIdx] <- paste(clBr[endBlockIdx],"}")
      }
      lineIdx <- lineIdx + 1
    }
    return(list(openBr=oBr, closeBr=clBr))
  }

  ## check for new environments
  env <- sapply(fbody[-1],
                function(code)
                {
                  envIdx <- grep("\\{$",code)
                  if(length(envIdx) > 0)
                  {
                    return(TRUE)
                  }
                  return(FALSE)
                },USE.NAMES=FALSE)
  
  ## check the block structure
  block <- sapply(fbody[-1],function(x)regexpr("[^ ]",x)[1],USE.NAMES=FALSE)

  ## is 4 a convention or a rule?
  block <- (block %/% 4) + 1

  ## check for if's, while's, etc. without new environment
  ol <- oneLiner(fbody[-1])

  ## create brackets for new environments
  br <- setBrackets(ol,block,env)

  ## create new Code
  newCode <- paste(as.vector(rbind(br$closeBr,fbody[-1],br$openBr)))
  newCode <- newCode[newCode != ""]

  ## include the breakpoint function
  bpVec <- sapply(newCode,
                 function(line)
                 {
                   nobp <- grep("^[ ]*(else |\\{|\\})",line)
                   if(length(nobp) == 0)
                   {
                     return("track$bp();")
                   }
                   return("")
                 },USE.NAMES=FALSE)

  for(i in seq(along=bpVec))
  {
    bpVec[i] <- gsub("\\(\\)",paste("(",i,")",sep=""),bpVec[i])
  }
  
  ## create the mainpulated body of the function
  newBody <- paste(bpVec,newCode)

  ## return signature and body
  return(list(modFunc=c(sig,newBody),newSource = newCode))
}



tracker <- function()
{
  ##@bdescr
  ##
  ##
  ##@edescr
  ##
  ##@ret  : [list] OO object with functions addFunc, getSourcee, init, bp, getTrackInfo
  
  ## object for information
  run <- list()

  ## current function index
  fIdx <- 0

  ## old time
  oldTime <- NULL;

  ## old src line
  oldSrcLine <- 0;
  
  addFunc <- function(fId,src,callExpr)
  {

    isThere <- which(fId == names(run));

    if(length(isThere) == 1)
    {
      fIdx <<- isThere
    }
    else
    {
      fIdx <<- fIdx +1;
      newFuncInfo <- list(src=src,run=integer(length(src)),time=numeric(length(src)),
                          graph=matrix(0,nrow=length(src),ncol=length(src)),
                          nrRuns=as.integer(0),funcCall=callExpr);
      if(fIdx==1)
      {
        
        run <<- list(newFuncInfo);
      }
      else
      {
        run <<- append(run,list(newFuncInfo));
      }
      
      names(run)[fIdx] <<- fId;
      
    }

    ## increment run number
    run[[fIdx]]$nrRuns <<- run[[fIdx]]$nrRuns + 1
    
    ## initialize local variables
    oldSrcLine <<- 0;
    oldTime <- NULL;
  }



  getTrackInfo <- function()
  {
    return(run);
  }
  
  
  init <- function()
  {
    run <<- list()
    fIdx <<- 0
  }

  bp <- function(nr)
  {
    run[[fIdx]]$run[nr]<<- run[[fIdx]]$run[nr] + 1

    ## cumulative processing time
    if(!is.null(oldTime))
    {
      dtime <-  proc.time()[1]- oldTime
      run[[fIdx]]$time[nr] <<- run[[fIdx]]$time[nr] + dtime
    }

    oldTime <<- proc.time()[1]
    ## graph
    if(oldSrcLine != 0)
    { 

      run[[fIdx]]$graph[oldSrcLine,nr] <<- run[[fIdx]]$graph[oldSrcLine,nr] + 1
    }

    ## store the old line
    oldSrcLine <<- nr
  }

  getSource <- function(nr)
  {
    return(run[[nr]]$src)
  }
  
  return(list(addFunc=addFunc,getSource=getSource,init=init,bp=bp,getTrackInfo=getTrackInfo))
}


inspect <- function(expr)
{
  ##@bdescr
  ##
  ##
  ##@edescr
  ##
  ##@in  expr : [call]
  
  ## getting the call and his parameter
  fCall <- as.character(substitute(expr));

  ## get the original call
  callExpr <- deparse(substitute(expr));
  
  ## getting the name of the function
  fname <- fCall[1];
  
  ## check for generic function
  if(isGeneric(fname))
  { 

    
    ## get type of arguments
    selType <- sapply(fCall[-1],
                      function(x)
                      {
                        if(exists(x,envir=sys.parent(sys.parent())))
                        {                        
                          varSig <- is(get(x,envir=sys.parent(sys.parent())))[1]
                        }
                        else
                        {
                          varSig <- is(eval(parse(text=x)))[1]
                        }
                        return(varSig)
                      },USE.NAMES=FALSE)

    
    ## we have to check for missing arguments
    formalArg <- names(formals(getGeneric(fCall[1])))

    ## number of missing arguments
    nrMissing <- length(formalArg) - length(selType)
    if(nrMissing > 0)
    {
      ## check for ...
      ellipseIdx <- which(formalArg=="...")
    
      if(length(ellipseIdx) != 0)
      {
        selType <- c(selType,rep("missing",nrMissing -1 ))
      }
      else
      {
        selType <- c(selType,rep("missing",nrMissing))
      }
    }
    ## select function
    selFunc <- selectMethod(fname,selType)

    ## deparse the function
    fbody <- deparse(selFunc@.Data,width.cutoff=500)

    ## create an identifier for the generic function
    fNameId <- paste("S4",fname,paste(selFunc@defined@.Data,collapse="/"),sep="/")
  }
  else
  {
    ## deparse the function
    fbody <- try(deparse(get(fname),width.cutoff=500))
    if (inherits(fbody, "try-error")) {
      ##  in case the function is defined
      ##  in the test case file
      fbody <- try(deparse(get(fname, envir=sys.parent()), width.cutoff=500))
      if (inherits(fbody, "try-error")) {
        stop("function not found.")
      }
    }
    
    ## create an identifier for the generic function
    fNameId <- paste("R/",fname,sep="")
  }  


 
  
  ## generate the new body of the function
  newFunc <- includeTracker(fbody)
  track$addFunc(fNameId,newFunc$newSource,callExpr)

  ## build the test function
  eval(parse(text=c("testFunc <- ",newFunc$modFunc)),envir=sys.frame())

  ## create function call
  newFunCall <- paste("testFunc(",paste(fCall[-1],collapse=","),")",sep="")

  parsedFunc <- try(parse(text=newFunCall))

  ## check for an error
  if(!inherits(parsedFunc,"try-error"))
  {
    ## call the new function
    res <- eval(parsedFunc,envir=parent.frame())
  }
  else
  {
    ## no parsing possible
    ## simple call without tracking
    res <- expr
  }


  ## do here some error checking
  
  return(res)
}


