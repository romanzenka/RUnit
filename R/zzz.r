##
##
##
##GLOBAL <- .initGLOBAL()

##.where <- getNamespace("RUnit")
.where <- .GlobalEnv

## ------------------------
##  classes
## ------------------------
## array class definition of primitive R classes (provided by 'base')
if (GLOBAL$getDebug())
{
  cat("\n\n RUnit initialization ...")
}

if (GLOBAL$getDebug())
{
  cat("\n\n Class definitions:\n\n")
}

##  make formal S4 representations from S3 class used
##.defineS3Classes(where = .where)

##.initArrayTemplate(.where)  ## defined in arrayTemplate.r, called only once

.defineTestResultClass(.where)
.defineTestFunctionResultClass(.where)
defineArrayClass("TestFunctionResult", where = .where)
.defineTestFileResultClass(.where)
defineArrayClass("TestFileResult", where = .where)
.defineTestSuiteResultClass(.where)



######################################
##
## Methods
##
######################################

if (GLOBAL$getDebug()) {
  cat("\n\n Method defintions:\n\n")
}

.defineTestResultMethods(where= .where)
.defineTestFileResultMethods(where= .where)
.defineTestSuiteResultMethods(where= .where)


if (GLOBAL$getDebug()) {
  cat("\n\n Initalization completed.\n")
}

