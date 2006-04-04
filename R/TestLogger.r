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
##
##  S4 implementation
##
##  $Id$
##

.defineTestLoggerClass <- function(where=environment()) {
  ##@bdescr
  ##
  ##@edescr
  ##
  ##@class [TestLogger] : <add class description>
  ##@slot  [<slotType>] : <add slot description>
 
  if (GLOBAL$getDebug()) {
    cat(".defineTestLoggerClass ... ")
  }

  setClass("TestLogger",
           representation("Logger",
                          errorHandler     = "character",
                          currentTraceBack = "character",
                          checkNum        = "integer"),
           validity = NULL,
           sealed   = GLOBAL$SEALED,
           where    = where)

  if (GLOBAL$getDebug()) {
    cat("o.k.\n")
  }
}
