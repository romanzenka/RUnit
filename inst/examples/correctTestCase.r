######################################################################
##
## library: RUnit
## =====================================
##
## correctTestCase.r
## =====================================
##  test function definition which by construction executes wo error or failure
##
##
##
## Version
## =====================================
##  $Id$
##
##
######################################################################

test.correctTestCase <- function() {

  checkTrue( TRUE)
  checkTrue( !identical(TRUE, FALSE))
}
