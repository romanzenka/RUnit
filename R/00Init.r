######################################################################
##
## RUnit
## =====================================
##
## 00Init.r
## =====================================
## initialization of classes, namespace, ...
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


.First.lib <- function(lib, package, where)
{
  ##  load required packages
  ##  first load standard or base packages
  ##  thus ensuring our definitions have highest precedence (as long as we don't
  ##  make use of namespaces)
  
  require(methods) || stop("\n Loading require package 'methods' failed. EpiR.dev could not be loaded. Check your package installation path.\n")


  ## name space support is currently experimental
  ## to ensure its working this package
  ## HAS TO BE loaded before loading EpiR.base
  ## it cannot be done from within this init script

  ## initialise random number generator (to make .Random.seed available)
  runif(1)


  ##  method to pass on namespace for EpiR.base specific classes and methods
  if (missing(where)) {
    ##  check if package 'namespace' has been preloaded
    if (exists("topenv")) where <- topenv()
    else where <- pos.to.env(match(paste("package:",package,sep=""), search()))
  }

  ## initialise generated classes in correct order
  .initGeneratedRUnit(where)

  ##  start up message
  ##GLOBAL$log(paste(getEpiRpkgName(package="EpiR.dev")," Version ",getEpiRpkgVersion(package="EpiR.dev")," loaded.\n"))
  cat("\n\nPackage RUnit loaded.\n\n")
}



.onLoad <- function(lib, pkg)
{
  ##@bdescr
  ## has the same role as .First.lib in case a package provides a namespace
  ## when .First.lib is disregarded
  ## does not yield conflicts when loaded into R < 1.7.0 this function is simply ignored
  ##
  ## library calls are executed via import() statements in the inst/NAMESPACE file
  ##@edescr

  ## initialise random number generator (to make .Random.seed available)
  runif(1)

  
  ## initialise generated classes in correct order
  if (exists("topenv")) where <- topenv()
  else where <- pos.to.env(match(paste("package:",pkg,sep=""), search()))
  
  .initGeneratedRUnit(where)
  
  ##GLOBAL$log(paste(getEpiRpkgName(package="EpiR.dev")," Version ",getEpiRpkgVersion(package="EpiR.dev")," loaded.\n"))
  cat("\n\nPackage RUnit loaded.\n\n")
}









