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
##
##  $Id$


cat("\n\nRUnit test cases for 'RUnit:inspect' functions\n\n")

.tearDown <- function() {
  if (exists("track", envir=.GlobalEnv)) {
    rm(track)
  }
}


foo <- function(x) {
  y <- 0
  for(i in 1:100)
  {
    y <- y + i
  }
  return(y)
}


testRUnit.inspect <- function() {

  ## the name track is necessary
  track <<- tracker()
  
  ## initialize the tracker
  track$init()
  
  ## inspect the function
  ## res will collect the result of calling foo
  res <- inspect(foo(10), track=track)
  checkEquals( res, 5050)

}


testRUnit.getTrackInfo <- function() {

  ## the name track is necessary
  track <<- tracker()
  
  ## initialize the tracker
  track$init()
  
  ## inspect the function
  checkTrue( exists("foo"))
  
  ## res will collect the result of calling foo
  res <- inspect(foo(10), track=track)
  checkEquals( res, 5050)

  ## get the tracked function call info
  resTrack <- track$getTrackInfo()
  checkTrue( is.list(resTrack))
  checkEquals( names(resTrack), c("R/foo"))

  checkEquals( names(resTrack$"R/foo"),
              c("src", "run", "time", "graph", "nrRuns", "funcCall"))
}
