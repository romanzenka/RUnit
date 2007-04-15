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

##  $Id$


##  global variable
##  logging mechanism used for
##   - error message handling
##   - global variable control
##   - log and error message stack mgmt
.GLOBAL <- .initGLOBAL()

.where <- getNamespace("RUnit")

##  ------------------------------------
##
##  class init calls
##
##  ------------------------------------
if (.GLOBAL$getDebug()) {
  cat("\n define classes ...\n")
}
.defineIsEqualBaseTypeMethod(where=.where)

.defineArrayClass(where=.where)
##  methods required for subsequent container class generator
.defineArrayMethods(where=.where)


.defineTestSuiteClass(where=.where)
##  container class
defineArrayClass("TestSuite", where=.where)

.defineTestCaseClass(where=.where)

.defineTestCaseTestResultDataClass(where=.where)
##  container class
defineArrayClass("TestCaseTestResultData", where=.where)

.defineSourceFileTestResultDataClass(where=.where)
##  container class
defineArrayClass("SourceFileTestResultData", where=.where)


.defineTestSuiteTestResultDataClass(where=.where)
##  container class
defineArrayClass("TestSuiteTestResultData", where=.where)


.defineTestResultDataClass(where=.where)


.defineTestLoggerClass(where=.where)



##  ------------------------------------
##
##  method init calls
##
##  ------------------------------------
if (.GLOBAL$getDebug()) {
  cat("\n define methods ...\n")
}
#.defineTestSuiteMethods(where=.where)

.defineTestCaseMethods(where=.where)

.defineTestCaseTestResultDataMethods(where=.where)

.defineSourceFileTestResultDataMethods(where=.where)

.defineTestSuiteTestResultDataMethods(where=.where)

##  to be implemented
.defineTestResultDataMethods(where=.where)


.defineTestLoggerMethods(where=.where)

