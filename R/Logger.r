##
##
##  RUnit
##  S4 implementation
##
##  $Id$
##

.defineLoggerClass <- function(where=environment()) {
  ##@bdescr
  ##
  ##@edescr
  ##
  ##@class [Logger] : <add class description>
  ##@slot  [<slotType>] : <add slot description>
 
  if (GLOBAL$getDebug()) {
    cat(".defineLoggerClass ... ")
  }

  setClass("Logger",
           representation("VIRTUAL"),
           validity = NULL,
           sealed   = GLOBAL$SEALED,
           where    = where)

  if (GLOBAL$getDebug()) {
    cat("o.k.\n")
  }
}
