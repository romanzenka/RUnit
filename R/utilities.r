######################################################################
##
## RUnit
## =====================================
##
## utilities.r
## =====================================
## contains some general helper tools
##
##
## Dependencies
## =====================================
##
##
## Version
## =====================================
##  $Id$
##
## Copyright (c) 2003-2004  Epigenomics AG Berlin - All rights reserved
## THIS IS PROPRIETARY SOURCE CODE of Epigenomics AG Berlin
##
######################################################################


getDescriptionInfo <- function(path=NULL, package=NULL)
{
  ##@bdescr
  ## utility function
  ## retrieves the information provided in the DESCRIPTION file of the specified package
  ## ATTENTION!
  ## relies on .path.package function which only searches the R_LIBS path
  ## if the sought package has been loaded with a statement like
  ## library("excotic", lib.loc="you/will/never/find")
  ## we generate an error
  ##@edescr
  ##
  ##@in  path    : [string] path to installed package directory
  ##@in  package : [string] name of package
  ##@ret         : [data.frame]  key - value pair entries for each line found in file

  
  if (is.null(package))
  {
    warning(" Argument 'package' not provided!\n")
    return(NULL)
  }
  
  if (is.null(path))
  {
    path <- .find.package(package, lib.loc=.libPaths())
  }

  if (file.exists(paste(path,"/DESCRIPTION",sep="")))
  {
    v <- read.dcf(paste(path,"/DESCRIPTION",sep=""))
  }
  else
  {
    warning("\n File DESCRIPTION not found.\n")
    v <- NULL
  }

  return(v)
}



getpkgVersion <- function(package="RUnit")
{
  ##@bdescr
  ## utility function
  ## returns the current package version as stated in the DESCRIPTION file
  ##@edescr
  ##@ret  : [character] version number
  
  v <- getDescriptionInfo(package=package)
  v[,"Version"]
}


getpkgName <- function(package="RUnit")
{
  ##@bdescr
  ## utility function
  ## returns the current package name as stated in the DESCRIPTION file
  ##@edescr
  ##@ret  : [charatcer] version number
  
  v <- getDescriptionInfo(package=package)
  v[,"Package"]
}
