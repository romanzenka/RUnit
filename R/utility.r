##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003-2008  Thomas Koenig, Matthias Burger, Klaus Juenemann
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


.sop <- function(number, word, plext="s") {
  ##@bdescr
  ## internal helper function to get singular or plural right
  ##@edescr
  ##@in  number : [numeric]
  ##@in  word   : [character]
  ##@in  plext  : [character] provide plural extension if any
  
  ifelse(number == 1, paste(number, word),
         paste(number, paste(word, plext, sep="")))
}
