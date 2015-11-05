# RUnit

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/RUnit)](http://cran.r-project.org/web/packages/RUnit)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/RUnit)](http://cran.rstudio.com/web/packages/RUnit/index.html)
[![Travis-CI Build Status](https://travis-ci.org/romanzenka/RUnit.svg?branch=master)](https://travis-ci.org/romanzenka/RUnit)

RUnit is a testing package for R code, inspired by the xUnit family of testing tools. 

Originally implemented by Thomas Koenig, Klaus Juenemann, and Matthias Burger, 
this package has served the R community for over a decade.

Since RUnit is no longer actively developed, I provide maintenance of this package 
mostly to support older projects that still rely on RUnit.

# Using RUnit

To make RUnit work with `R CMD check`, create a following file in the `tests` subdirectory. 
This would run the actual tests stored in the packages `inst/tests` subdirectory.

    # Our package. Used for the test suite name
    pkgname <- "your package name"
    require(pkgname, quietly=TRUE, character.only=TRUE) || stop("package '", pkgname, "' not found")

    # How to determine which files to load (have to start with test_ and end with .R)
    pattern <- "^test_.*\\.R$"
  
    # Which functions to run. Have to start with 'test.'
    testFunctionRegexp = "^test.+"

    # Path to the unit tests folder in the package
    dir <- system.file(subdir, package=pkgname)
  
    # Define RUnit test suite
    suite <- defineTestSuite(name=paste(pkgname, "RUnit Tests"),
                             dirs=dir,
                             testFileRegexp=pattern,
                             testFuncRegexp = testFunctionRegexp,
                             rngKind="default",
                             rngNormalKind="default")
  
    # Run tests
    result <- runTestSuite(suite)
  
    # Display result tests on the console
    printTextProtocol(result)
  
    # Write results in JUnit-like xml format
    printJUnitProtocol(result, fileName="junit.xml")
    
    
A typical unit test would then live for example in `inst/tests/test.operations.R`.
Each such file can contain multiple functions like so:

    test.additionWorks <- function() {
      checkEquals(2, 1+1, "one plus one must be two")
    }
    
    test.divisionByZeroFails <- function() {
      checkException(1/0, "division by zero is expected to fail")
    }

