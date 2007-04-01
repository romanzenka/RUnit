##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2006-2007 Matthias Burger, Thomas Koenig, Klaus Juenemann
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


.defineTestCaseTestResultDataClass <- function(where=environment()) {
  ##@bdescr
  ##
  ##  Initialisation function
  ##  Not to be called by the user(s).
  ##  Initialization for class 'TestCaseTestResultData'.
  ##  
  ##@edescr
  ##
  ##@class : [TestCaseTestResultData]
  ##
  ##@slot  name        : [character] test suite name 
  ##@slot  dir         : [character]  
  ##@slot  deactivated : [logical]  
  ##@slot  failure     : [logical]  
  ##@slot  error       : [logical]  
  ##@slot  execTime    : [integer]  

  
  if (.GLOBAL$getDebug()) {
    
    cat(".defineTestCaseTestResultDataClass ... ")
  }
  
  setClass("TestCaseTestResultData",
           representation(name        = "character",
                          dir         = "character",
                          deactivated = "logical",
                          failure     = "logical",
                          error       = "logical",
                          execTime    = "integer"),
           prototype(deactivated = as.logical(FALSE),
                     failure     = as.logical(FALSE),
                     error       = as.logical(FALSE),
                     execTime    = as.integer(NA)),
           validity = NULL,
           sealed   = .GLOBAL$getSealed(),
           where    = where)

  if (.GLOBAL$getDebug()) {
    
    cat("o.k.\n")
  }
}
