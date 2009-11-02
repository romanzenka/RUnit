######################################################################
##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003-2009  Thomas Koenig, Matthias Burger, Klaus Juenemann
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; version 2 of the License.
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


.First.lib <- function(lib, package, where)
{
  ##@bdescr
  ## Internal Function.
  ## Not to be called by users.
  ##
  ##@edescr
  
  ##  load required packages
  errMsg <- paste("\nLoading required package 'methods' failed. RUnit could not be loaded.",
                  "\nCheck your library installation path.\n")
  require(methods) || stop(errMsg)

  runitVersion <- packageDescription("RUnit", lib.loc=lib, fields="Version")
}


.onLoad <- function(lib, pkg)
{
  ##@bdescr
  ## Internal Function.
  ## Not to be called by users.
  ##
  ## has the same role as .First.lib in case a package provides a namespace
  ## when .First.lib is disregarded
  ## does not yield conflicts when loaded into R < 1.7.0 this function is simply ignored
  ## library calls are executed via import() statements in the inst/NAMESPACE file
  ##@edescr

  ##  load required packages
  errMsg <- paste("\nLoading required package 'methods' failed. RUnit could not be loaded.",
                  "\nCheck your library installation path.\n")
  require(methods) || stop(errMsg)

  
  runitVersion <- packageDescription("RUnit", lib.loc=lib, fields="Version")
  ##  avoid check NOTE messages
  assign(".testLogger", NULL, envir=.GlobalEnv)
}




