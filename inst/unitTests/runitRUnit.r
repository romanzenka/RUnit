##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003-2012  Thomas Koenig, Matthias Burger, Klaus Juenemann
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
##
##  $Id$


cat("\n\nRUnit test cases for 'RUnit:check' functions\n\n")


testRUnit.checkEquals <- function()
{
  ##@bdescr
  ## test case for function checkEquals of class: none
  ##@edescr

  ##  integer
  x <- 1:10
  checkEquals(x, x)
  ##  return value
  checkTrue( checkEquals(x, x))
  namedInt <- 1:10
  names(namedInt) <- letters[namedInt]
  checkEquals(namedInt, namedInt)
  checkEquals(namedInt, x, checkNames=FALSE)
  
  ##  numeric
  checkEquals(pi, pi)
  y <- 1/0
  checkEquals(Inf, y)
  checkEquals(y, Inf)
  y <- log(-1)
  checkEquals(NaN, y)
  checkEquals(rep(NaN, 23), rep(y, 23))
  checkEquals(9, 9.0)
  checkEquals(NA, NA)
  checkEquals(rep(NA, 14), rep(NA, 14))
  checkEquals( numeric(1), numeric(1))
  checkEquals( 0.01, 0.02, tolerance=0.01)
  checkEquals( 0.01, 0.02+100, tolerance=100.01)
  tmp <- c(0.01, NA, 0.02, Inf, -Inf, NaN, 1.0)
  checkEquals( tmp, tmp, tolerance=0.01)
  ##  named numeric
  tmp2 <- tmp
  names(tmp) <- letters[seq(along=tmp)]
  checkEquals( tmp, tmp, tolerance=0.01)
  
  ##  complex
  checkEquals(complex(0), complex(0))
  checkEquals(complex(2), complex(2))
  i1 <- complex(2, imaginary=1)
  checkEquals(i1, complex(2, imaginary=1))
  ##  named complex
  i2 <- i1
  names(i2) <- 1:2
  checkEquals(i1, i2, checkNames=FALSE)
  
  ##  character
  checkEquals( character(1), character(1))
  checkEquals( letters, letters)
  l1 <-  letters
  l2 <- l1
  names(l2) <- rev(letters)
  checkEquals(l1, l2, checkNames=FALSE)
  
  ##  matrix
  mat <- matrix(1, 3, 5)
  checkEquals( mat, matrix(1, 3, 5))
  rMatrix <- matrix(rnorm(250000), 50000, 50)
  checkEquals(rMatrix , rMatrix,
		  "large matrix not identified as equal")
  matNamed <- mat
  dimnames(matNamed) <- list(paste("X", 1:3, sep=""), 1:5)
  ##  use all.equals.numeric parameter to ignore differing dimnames
  checkEquals(matNamed, mat, check.attributes=FALSE)
  matNamed2 <- mat
  dimnames(matNamed2) <- list(X=paste("X", 1:3, sep=""), Y=1:5)
  checkEquals(matNamed, matNamed2, check.attributes=FALSE)
  
  ##  named
  rMatrixNamed <- rMatrix
  names(rMatrixNamed) <- letters
  checkEquals(rMatrixNamed , rMatrixNamed,
		  "large named matrix not identified as equal")
  checkEquals(rMatrix , rMatrixNamed, checkNames=FALSE)
  
  ##  array
  arrVec <- array(1:21, dim=1L)
  checkEquals( arrVec, arrVec)
  arrVecNamed <- array(1:21, dim=1L, dimnames="X")
  checkEquals( arrVecNamed, arrVec, checkNames=FALSE)
  
  arrMat <- as.array(matrix(1:20, 4, 5))
  checkEquals( arrMat, arrMat)
  arrMatNamed <- arrMat
  dimnames(arrMatNamed) <- list(X=toupper(letters[1:4]), Y=letters[1:5])
  checkException(checkEquals( arrMatNamed, arrMat, checkNames=FALSE))  
  ## Error in checkEquals(arrMatNamed, arrMat, checkNames = FALSE) : 
  ## Attributes: < Length mismatch: comparison on first 1 components >
  ##  use check.attributes to only compare array values but not dimnames
  checkEquals( arrMatNamed, arrMat, check.attributes=FALSE)
  
  arrArr <- array(1:60, dim=c(3, 4, 5), dimnames=list(paste("X", 1:3, sep=""), paste("Y", 1:4, sep=""), paste("Z", 1:5, sep="")))
  checkEquals( arrArr, arrArr)
  checkException(checkEquals( arrArr, array(1:60, dim=c(3, 4, 5)), checkNames=FALSE))
  ##  same, use check.attributes in this case
  
  arrArrFail <- array(1:60, dim=c(3, 4, 5), dimnames=list(NULL, paste("Y", 1:4, sep=""), paste("Z", 1:5, sep="")))
  checkEquals( arrArr, arrArrFail, check.attributes=FALSE)
  arrArrFail <- array(1:60, dim=c(3, 4, 5), dimnames=list(paste("X", 1:3, sep=""), NULL, paste("Z", 1:5, sep="")))
  checkEquals( arrArr, arrArrFail, check.attributes=FALSE)
  arrArrFail <- array(1:60, dim=c(3, 4, 5), dimnames=list(paste("X", 1:3, sep=""), paste("Y", 1:4, sep=""), NULL))
  checkEquals( arrArr, arrArrFail, check.attributes=FALSE)
  
  ##  data.frame
  dF <- as.data.frame(matrix(ncol=2, nrow=3))
  checkEquals(dF, dF)
  ##  ignore names
  dF2 <- dF
  colnames(dF2) <- NULL
  checkEquals(dF, dF2, checkNames=FALSE)
  
  ##  language
  ep <- expression(2)
  checkEquals( ep, expression(2))
  checkEquals( call("mean", "median"), call("mean", "median"))
  ##  ignore names
  names(ep) <- "Expr"
  checkEquals( ep, expression(2), checkNames=FALSE)
  
  ##  formula
  simpleForm <- x ~ 1
  checkEquals( simpleForm, simpleForm,
              "simple formula not identified as equal")
  ##  named
  names(simpleForm) <- "Simple"
  checkEquals( simpleForm, simpleForm,
              "simple named formula not identified as equal")
  
  compForm <- y ~ x + y + x*y + offset(x)
  checkEquals( compForm, compForm,
              "formula not identified as equal")
  
  ##  factor
  alphaFac <- factor(letters)
  checkEquals( alphaFac, alphaFac,
              "factor not identified as equal")
  ##  named
  names(alphaFac) <- seq(along=letters)
  checkEquals( alphaFac, alphaFac,
              "named factor not identified as equal")
  ##  list
  checkEquals( list(100), list(100))
  checkEquals( list(100), list(100), tolerance=1)
  alphaList <- seq_along(letters)
  names(alphaList) <- letters
  checkEquals( alphaList, alphaList)
  alphaList2 <- alphaList
  names(alphaList2) <- rev(letters)
  checkEquals( alphaList, alphaList2, checkNames=FALSE)

  ##  nested list with NA, NaN, Inf
  nl <- list(a=list(1), b=list(1:4),
             c=list(ab=1, bc=list(list(2), list(NA), list(NaN)) ),
             d=list(m1=matrix(NA, 2,3), m2=matrix(1+1i, 4,5)),
             e=list(e1=NaN, e2=list(Inf), e3=list(a=Inf, b=-Inf, c=NaN, d=-0/0)))
  checkEquals(nl, nl)
  
  ##  example from ?glm
  counts <- c(18,17,15,20,10,20,25,13,12)
  outcome <- gl(3,1,9)
  treatment <- gl(3,3)
  lmFit <- glm(counts ~ outcome + treatment, family=poisson())
  checkEquals( lmFit, lmFit, checkNames=FALSE)
  checkEquals( lmFit, lmFit)
  lmFitUnnamed <- lmFit
  names(lmFitUnnamed) <- NULL
  checkEquals( lmFit, lmFitUnnamed, checkNames=FALSE)
  
  ##  POSIXct
  sysTime <- as.POSIXct(Sys.time())
  checkEquals( sysTime, sysTime)
  ##  named
  sysTime2 <- sysTime
  names(sysTime2) <- "Today"
  checkEquals( sysTime, sysTime2, checkNames=FALSE)
  
  ##  raw
  checkEquals( raw(14), raw(14))
  namedRaw <-  as.raw(1:14)
  names(namedRaw) <- letters[1:14]
  checkEquals( namedRaw, namedRaw)
  checkEquals( namedRaw, as.raw(1:14), checkNames=FALSE)
  
  ##  formula
  a <- 1:10
  f <- gl(2,5)
  checkEquals( a~f, a~f)
  
  ##  S4 objects
  if (identical(TRUE, require(methods))) {
    setClass("track1",
             representation(x="numeric", y="numeric"),
             where=.GlobalEnv)
    on.exit(removeClass("track1", where=.GlobalEnv))
    
    s4Obj <- try(new("track1"))
    s4Obj@x <- 1:10
    s4Obj@y <- 10:1
    checkEquals( s4Obj, s4Obj)

    ##  S4 class containing S4 class slot
    setClass("trackPair",
             representation(trackx = "track1",
                            tracky = "track1"),
             where=.GlobalEnv)
    
    on.exit(removeClass("trackPair", where=.GlobalEnv), add=TRUE)

    tPair <- new("trackPair")
    tPair@trackx <- s4Obj
    tPair@tracky <- s4Obj
    checkEquals( tPair, tPair)
  }

  if (require(Biobase)) {
    ##   class still available?
    #if (isClass(Class="ExpressionSet", formal=TRUE)) {
    #  ES <- new("ExpressionSet", exprs=matrix(runif(1000), nrow=100, ncol=10))
    #  checkEquals(ES, ES)
    #}
    ##  cleanup workspace
    ##  catch error if this ns is required by some other package
    ##  and therefore cannot be unloaded
    try(unloadNamespace("Biobase"))
  }

  
  ##
  ##  incorrect input
  checkException( checkEquals() )
  checkException( checkEquals(checkNames=TRUE) )
  checkException( checkEquals(1) )
  checkException( checkEquals(1, checkNames=TRUE) )
  checkException( checkEquals(1, 1, checkNames="yes") )
  checkException( checkEquals(1, 1, checkNames=logical(0)) )
  checkException( checkEquals(1, 1, checkNames=logical(2)) )
  checkException( checkEquals(1, 1, tolerance=TRUE) )
  checkException( checkEquals(1, 1, tolerance=numeric(0)) )
  checkException( checkEquals(1, 1, tolerance=numeric(2)) )
  
  ##  detect differences
  ##  integer
  namedInt <- 1:9
  checkException( checkEquals( namedInt, 1:8))
  namedInt[1] <- 2
  checkException( checkEquals( namedInt, 1:9))
  namedInt <- 1:9
  names(namedInt) <- letters[namedInt]
  checkException( checkEquals( namedInt, 1:9))
  checkException( checkEquals( namedInt, 1:8, checkNames=FALSE))
  
  ##  numeric
  checkException( checkEquals( 8, 9))
  checkException( checkEquals( 0.01, 0.02, tolerance=0.009))
  checkException(checkEquals(NaN, NA))
  checkException(checkEquals(NaN, Inf))
  checkException(checkEquals(NaN, -Inf))
  checkException(checkEquals(NA, Inf))
  checkException(checkEquals(NA, -Inf))
  checkException(checkEquals(numeric(2), numeric(3)))
  checkException(checkEquals(numeric(3), numeric(2)))
  
  ##  complex
  checkException( checkEquals(complex(0), complex(1)))
  checkException( checkEquals(complex(2), complex(1)))
  checkException( checkEquals(complex(2, imaginary=1), complex(2, imaginary=0)))
  checkException( checkEquals(complex(2, real=1, imaginary=1), complex(2, real=1, imaginary=0)))
  checkException( checkEquals(complex(2, real=1, imaginary=1), complex(2, real=0, imaginary=1)))
  checkException( checkEquals(complex(2, real=1, imaginary=1), complex(2, real=0, imaginary=0)))
  
  ##  character
  named <- character(1)
  names(named) <- "name"
  checkException( checkEquals( character(1), named))
  checkException( checkEquals( letters, letters[-1]))
  
  ##  matrix
  mat1 <- mat2 <- matrix(1:6, ncol=2)
  mat2[1,1] <- 0
  checkException( checkEquals(mat1, 1))
  checkException( checkEquals(mat1, as.data.frame(mat1)))
  checkException( checkEquals(mat1, t(mat1)))
  checkException( checkEquals(mat1, mat2))
  mat2 <- mat1
  colnames(mat2) <- 1:2
  checkException( checkEquals(mat1, mat2, checkNames=TRUE))
  
  ##  data.frame
  dF <- data.frame(a=letters[1:10], b=1:10, c=rnorm(n=10), d=factor(1:10), e=complex(real=rnorm(n=10), imaginary=1:10))
  dF2 <- dF
  dF2[1,1] <- "b"
  checkException( checkEquals(dF, dF2))
  checkException( checkEquals(dF, dF2, checkNames=FALSE))
  dF2 <- dF
  dF2[1,2] <- 2
  checkException( checkEquals(dF, dF2, checkNames=FALSE))
  dF2 <- dF
  rownames(dF2) <- as.character(c(0,2:10))
  checkException( checkEquals(dF, dF2, checkNames=TRUE))
  
  ##  language
  ep <- expression(2)
  checkException(checkEquals( ep, expression(3)))
  checkException(checkEquals( ep, expression(1)))
  
  checkException(checkEquals( call("mean", "median"), call("mean", "sqrt")))
  
  ##  ignore names
  names(ep) <- "Expr"
  checkException(checkEquals( ep, expression(2), checkNames=TRUE))
  
  ##  formula
  checkException( checkEquals( lmFit, lmFitUnnamed))
  lmFitInter <- glm(counts ~ outcome * treatment, family=poisson())
  checkException( checkEquals( lmFitInter, lmFit))
  
  ##  factor
  alphaFacRecoded <- factor(alphaFac, labels=as.character(seq_along(levels(alphaFac))))
  checkException( checkEquals(alphaFacRecoded, alphaFac))
  ##  named
  alphaFac <- factor(letters)
  alphaFacNamed <- alphaFac
  names(alphaFacNamed) <- seq(along=letters)
  checkException(checkEquals( alphaFacNamed, alphaFac,
		  "named factor not identified as unequal", checkNames=TRUE))
  
  ##  list
  checkException( checkEquals( list(1), list("1"=1)))
  checkException( checkEquals( list(), list("1"=1)))
  checkException( checkEquals( list(list(), list(list()), list(list(list()))),
                              list(list(), list(list()), list(list(list(), list())))))
  ##  named
  aList <- list(list(a="A"), list(list(b=1)))
  aListNamed <- aList
  names(aListNamed) <- "my"
  checkException( checkEquals(aListNamed, aList, checkNames=TRUE))
  
  ##  POSIXct
  posixct <- as.POSIXct("2007-04-04 16:00:00")
  checkException( checkEquals(as.POSIXct(Sys.time()), posixct))
  checkException( checkEquals(as.POSIXlt(Sys.time()), posixct))
  ## named
  posixctNamed <- posixct
  names(posixctNamed) <- "Past Time"
  checkException( checkEquals(posixctNamed, posixct, checkNames=TRUE))
  ##  nested type
  sysTime <- as.POSIXct(Sys.time())
  checkException( checkEquals( list(a=2, list(time=sysTime)), list(a=2, time=list(sysTime))))

  ##  raw
  checkException( checkEquals(raw(1), raw(2)))
  checkException( checkEquals(raw(1E5), raw(100001)))
  raw3 <- raw(3)
  raw3mod <- raw3
  raw3mod[1] <- as.raw(3)
  checkException( checkEquals(raw3, raw3mod))
  checkException( checkEquals(as.raw(1:1000), as.raw(c(1:99,-1,101:1000)) ) )
  ##  named
  raw3Named <- raw3
  names(raw3Named) <- letters[1:3]
  checkException( checkEquals(raw3Named, raw3, checkNames=TRUE))
  
  
  ##  S4 objects
  if (identical(TRUE, require(methods))) {
    ##  class defined above
    s4Obj <- new("track1")
    s4Obj@x <- 1:10
    checkException( checkEquals( s4Obj, new("track1")))

    tPair <- new("trackPair")
    tPair@trackx <- s4Obj
    checkException( checkEquals( tPair, new("trackPair")))
  }

}


testRUnit.checkEqualsNumeric <- function()
{
  ##@bdescr
  ## test case for function checkEqualsNumeric of class: none
  ##@edescr

  ##  integer
  checkEqualsNumeric( 9L, 9L)
  checkEqualsNumeric( as.integer(NA), as.integer(NA))
  checkEqualsNumeric( -11L, -11L)
  
  ##  numeric  
  checkEqualsNumeric( 9, 9)
  checkEqualsNumeric( 9.1,9.2, tolerance=0.1)
  checkEqualsNumeric( 9.1,11.2, tolerance=2.1)
  
  x <- 1:10
  attributes(x) <- list(dummy="nonsense")
  checkEqualsNumeric( x, x)
  checkEqualsNumeric( 1:10, x, check.attributes=FALSE)
  
  rvec <- rnorm(132)
  checkEqualsNumeric( rvec, rvec)
  checkEqualsNumeric( rvec, rvec + 100, tolerance=100)
  ##  named
  rvec2 <- rvec
  names(rvec2) <- 1:132
  checkEqualsNumeric( rvec2, rvec2)
  ##  names ignored
  checkEqualsNumeric( rvec, rvec2)
  
  ##  matrix
  mat <- matrix(rvec, 12, 11)
  checkEqualsNumeric( mat, mat)
  ##  dimnames
  mat2 <- mat
  dimnames(mat2) <- list(1:12, 1:11)
  checkEqualsNumeric( mat2, mat2)
  ##  dimnames ignored
  checkEqualsNumeric( mat2, mat)
  
  ##  array
  arr <- array(1:128, dim=c(8,8,2))
  checkEqualsNumeric( arr, arr)
  
  ##  dimnames are ignored
  arr <- array(1:128, dim=c(8,8,2))
  dimnames(arr) <- list(1:8, 1:8, 1:2)
  arr2 <- arr
  dimnames(arr) <- list(1:8, 1:8, c(1,3))
  checkEqualsNumeric( arr, arr2)
  
  ##  same class 
  arr <- array(1:64, dim=c(8,8), dimnames=list(1:8, 1:8))
  mat <- matrix(1:64, ncol=8, nrow=8, dimnames=list(1:8, 1:8))
  checkEqualsNumeric( arr, mat)
  
  ##  data.frame
  dF <- data.frame(A=seq(along=letters), B=seq(along=letters))
  checkEqualsNumeric(dF, dF)
  
  dF <- data.frame(mat)
  dF2 <- as.data.frame(mat)
  checkEqualsNumeric(dF, dF)
  checkEqualsNumeric(dF2, dF2)
  ##  but
  checkException( checkEqualsNumeric(dF, dF2))
  ##  so use check.attributes
  checkEqualsNumeric(dF, dF2, check.attributes=FALSE)
  
  
  ##  raw
  checkEqualsNumeric( raw(9), raw(9))
  checkEqualsNumeric( as.raw(9), as.raw(9))
  araw <- charToRaw("A")
  attributes(araw) <- list(name="A")
  checkEqualsNumeric(charToRaw("A"), araw, check.attributes=FALSE)
  
  
  ##  special constants
  checkEqualsNumeric( pi, pi)
  checkEqualsNumeric( NA, NA)
  checkEqualsNumeric( NA_integer_, NA_integer_)
  checkEqualsNumeric( NA_character_, NA_character_)
  checkEqualsNumeric( NaN, NaN)
  checkEqualsNumeric( Inf, Inf)
  checkEqualsNumeric( -Inf, -Inf)
  checkEqualsNumeric( NULL, NULL)
  ##  function call as argments
  checkEqualsNumeric( exp(1), exp(1))
  
  checkEqualsNumeric( c(1, NA, 3), c(1, NA, 3))
  checkEqualsNumeric( c(1, NaN, 3), c(1, NaN, 3))
  checkEqualsNumeric( c(1, Inf, 3), c(1, Inf, 3))
  checkEqualsNumeric( c(1, -Inf, 3), c(1, -Inf, 3))
  
  
  ##  differences
  checkException( checkEqualsNumeric( NA, NA_integer_))
  checkException( checkEqualsNumeric( NA_character_, NA_integer_))
  checkException( checkEqualsNumeric( NA, NaN))
  checkException( checkEqualsNumeric( Inf, NA))
  checkException( checkEqualsNumeric( -Inf, Inf))
  checkException( checkEqualsNumeric( Inf, NaN))
  checkException( checkEqualsNumeric( 9, 10))
  ##  length/dimension
  checkException( checkEqualsNumeric( integer(9), integer(10)))
  checkException( checkEqualsNumeric( integer(9), logical(9)))
  checkException( checkEqualsNumeric( list(9), list(10)))
  checkException( checkEqualsNumeric( matrix(9), matrix(10)))
  checkException( checkEqualsNumeric( data.frame(A=1:3, B=3:6), data.frame(A=1:3, B=3:6, C=7:9)))
  checkException( checkEqualsNumeric( data.frame(A=1:3, B=3:6), data.frame(A=1:4, B=3:7)))
  checkException( checkEqualsNumeric( arry(1:128, dim=c(8,2,8)), array(1:112, dim(8,2,7))))
  
  ##  numeric differences
  rvec2 <- rnorm(132)
  checkException( checkEqualsNumeric( matrix(rvec, 12, 11), matrix(rvec2, 12, 11)))

  ##  complex
  checkException( checkEqualsNumeric(complex(0), complex(1)))
  checkException( checkEqualsNumeric(complex(2), complex(1)))
  checkException( checkEqualsNumeric(complex(2, imaginary=1), complex(2, imaginary=0)))
  checkException( checkEqualsNumeric(complex(2, real=1, imaginary=1), complex(2, real=1, imaginary=0)))
  checkException( checkEqualsNumeric(complex(2, real=1, imaginary=1), complex(2, real=0, imaginary=1)))
  checkException( checkEqualsNumeric(complex(2, real=1, imaginary=1), complex(2, real=0, imaginary=0)))
  
  
  ##  array
  arr <- array(1:128, dim=c(8,8,2))
  arr2 <- array(1:112, dim=c(8,7,2))
  checkException( checkEqualsNumeric( arr, arr2))
 
  arr2 <- arr
  arr2[1,1,2] <- 0
  checkException( checkEqualsNumeric( arr, arr2, tolerance=0.1))
  
  ##  data.frame
  dF <- data.frame(A=letters, B=seq(along=letters))
  dFNA <- dF
  dFNA$A[1] <- as.character(NA)
  checkException( checkEqualsNumeric( dF, dFNA))
  dFINF <- dF
  dFINF$B[1] <- Inf
  checkException( checkEqualsNumeric( dF, dFINF))
  dFNaN <- dF
  dFNaN$B[1] <- NaN
  checkException( checkEqualsNumeric( dF, dFNaN))
  dF2 <- data.frame(A=letters[1:13], B=1:13)
  checkException( checkEqualsNumeric( dF, dF2))
  
  ##  exception handling
  ##  type not supported
  checkException( checkEqualsNumeric( list(rvec), list(rvec)))
  
  
  if (require(Biobase)) {

    ##   class still available?
    if (isClass(Class="ExpressionSet", formal=TRUE)) {
      ES <- new("ExpressionSet", exprs=matrix(runif(1000), nrow=100, ncol=10))
      checkException(checkEqualsNumeric(ES, ES))
    }
    ##  cleanup workspace
    try(unloadNamespace("Biobase"))
  }

}


testRUnit.checkIdentical <- function()
{
  ##@bdescr
  ## test case for function checkIdentical of class: none
  ##@edescr

  checkIdentical( TRUE, TRUE)
  ##  return value
  checkTrue( checkIdentical( TRUE, TRUE))
  checkIdentical( FALSE, FALSE)

  ##  bit representation identical
  checkIdentical( NA, NA)
  checkIdentical( c(1, NA, 3), c(1, NA, 3))
  checkIdentical( NaN, NaN)
  checkIdentical( c(1, NaN, 3), c(1, NaN, 3))
  checkIdentical( Inf, Inf)
  checkIdentical( c(1, Inf, 3), c(1, Inf, 3))
  checkIdentical( -Inf, -Inf)
  checkIdentical( c(1, -Inf, 3), c(1, -Inf, 3))
  
  checkIdentical( as.integer(2), as.integer(2))
  checkIdentical( as.character(2), as.character(2))
  checkIdentical( as.complex(2), as.complex(2))
  checkIdentical( as.numeric(2), as.numeric(2))
  checkIdentical( as.expression("2+4"), as.expression("2+4"))
  checkIdentical( as.expression(2+4), as.expression(2+4))
  checkIdentical( as.factor(letters), factor(letters))
  
  ##  nested list with NA, NaN, Inf
  nl <- list(a=list(1), b=list(1:4),
             c=list(ab=1, bc=list(list(2), list(NA), list(NaN)) ),
             d=list(m1=matrix(NA, 2,3), m2=matrix(1+1i, 4,5)),
             e=list(e1=NaN, e2=list(Inf), e3=list(a=Inf, b=-Inf, c=NaN, d=-0/0)))
  checkIdentical(nl, nl)

  ##  POSIX
  sysTime <- as.POSIXlt(Sys.time())
  checkIdentical( sysTime, sysTime)

  ##  raw
  checkIdentical( raw(14), raw(14))
  namedRaw <-  as.raw(1:14)
  names(namedRaw) <- letters[1:14]
  checkIdentical( namedRaw, namedRaw)

  ##  formula
  a <- 1:10
  f <- gl(2,5)
  checkIdentical( a~f, a~f)

  ##  call
  cl <- call("round", 10.5)
  checkIdentical( cl, cl)

  ##  expression
  ep <- expression(v,w, 2+3)
  checkIdentical( ep, ep)
  
  ##  matrix
  mat <- matrix(rnorm(20000), 500, 40)
  checkIdentical( mat, mat)
  dimnames(mat) <- list(X=paste("X", 1:500, sep=""), Y=1:40)
  checkIdentical( mat, mat)
  
  ##  data.frame
  dF <- as.data.frame(mat)
  checkIdentical( dF, dF)
  
  ##  array
  arr <- array(rnorm(50000), dim=c(500, 50, 2), dimnames=list(X=paste("X", 1:500, sep=""), Y=1:50, Z=1:2))
  checkIdentical( arr, arr)
  
  ##  S3 objects (ie. lists with attributes)
  ##  from ?lm Example
  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
  trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
  group <- gl(2,10,20, labels=c("Ctl","Trt"))
  weight <- c(ctl, trt)
  lm.D9 <- lm(weight ~ group)
  checkIdentical( lm.D9, lm(weight ~ group))


  ##  S4 objects
  if (identical(TRUE, require(methods))) {
    setClass("track1",
             representation(x="numeric", y="numeric"),
             where=.GlobalEnv)
    on.exit(removeClass("track1", where=.GlobalEnv))

    s4Obj <- try(new("track1"))
    checkIdentical( s4Obj, new("track1"))
    rm(s4Obj)
  }

  
  ##  exception handling
  ##  type mismatches
  checkException( checkIdentical( as.integer(2), as.numeric(2)))
  checkException( checkIdentical( as.integer(2), as.character(2)))
  checkException( checkIdentical( as.integer(2), as.list(2)))
  checkException( checkIdentical( as.integer(2), as.complex(2)))
  checkException( checkIdentical( as.integer(2), as.expression(2)))

  ##  value mismatches
  checkException( checkIdentical( as.integer(2), as.integer(3)))
  checkException( checkIdentical( as.character(2), as.character(3)))
  checkException( checkIdentical( as.complex(2), as.complex(3)))
  checkException( checkIdentical( as.numeric(2), as.numeric(3)))
  checkException( checkIdentical( as.expression("2+4"), as.expression("2+3")))
  checkException( checkIdentical( as.factor(letters), factor(letters[-1])))
  fac <- factor(letters)
  levels(fac) <- c("1", letters[-1])
  checkException( checkIdentical( fac,  as.factor(letters)))
  
  ##  nested list with NA, NaN, Inf
  checkException( checkIdentical( ))

  ##  POSIX
  sysTime <- as.POSIXlt(Sys.time())
  checkException( checkIdentical( sysTime, as.POSIXlt(Sys.time(), tz="GMT")))
  
  ##  raw
  checkException(checkIdentical( raw(14), raw(13)))
  namedRaw <-  as.raw(1:14)
  names(namedRaw) <- letters[1:14]
  checkException(checkIdentical( namedRaw, as.raw(1:14)))
  
  ##  formula
  a <- 1:10
  f <- gl(2,5)
  h <- gl(1,10)
  checkException(checkIdentical( a~f, a~h))
  
  ##  call
  cl <- call("round", 10.5)
  cl2 <- call("round2", 10.5)
  checkException(checkIdentical( cl, cl2))
  
  ##  expression
  ep <- expression(u,v, 1+ 0:9)
  ep2 <- expression(u,v, 2+ 0:9)
  checkException(checkIdentical(ep, ep2))
  
  ##  matrix
  mat <- matrix(rnorm(20000), 500, 40)
  mat2 <- mat
  mat2[500, 40] <- 123
  checkException(checkIdentical( mat, mat2))
  dimnames(mat) <- list(X=paste("X", 1:500, sep=""), Y=1:40)
  mat2 <- mat
  dimnames(mat2) <- list(X=paste("X", 1:500, sep=""), Y=c(1:39, 41))
  checkException(checkIdentical( mat, mat2))
  
  ##  data.frame
  dF <- as.data.frame(mat)
  dF2 <- as.data.frame(mat2)
  checkException(checkIdentical( dF, dF2))
  
  ##  array
  arr <- array(rnorm(50000), dim=c(500, 50, 2), dimnames=list(X=paste("X", 1:500, sep=""), Y=1:50, Z=1:2))
  arr2 <- arr
  arr2[500, 50, 1] <- as.numeric(NA)
  checkException(checkIdentical( arr, arr2))
  
  ##  S3 objects (ie. lists with attributes)
  ##  from ?lm Example
  lm.D9base <- lm(weight ~ group - 1)
  checkException( checkIdentical( lm.D9base, lm.D9))

  ##  S4 objects
  if (identical(TRUE, require(methods))) {
    setClass("track2",
             representation(x="numeric", y="numeric"),
             prototype(x=as.numeric(1:23), y=as.numeric(23:1)),
             where=.GlobalEnv)
    on.exit(removeClass("track2", where=.GlobalEnv), add=TRUE)

    s4Obj <- try(new("track2"))
    s4ObjDiff <- s4Obj
    s4ObjDiff@y <- s4ObjDiff@x
    checkException( checkIdentical( s4Obj, s4ObjDiff))
  }

}


testRUnit.checkTrue <- function()
{
  ##@bdescr
  ## test case for function checkTrue of class: none
  ##@edescr


  checkEquals( checkTrue( TRUE), TRUE)

  ##  named arguments
  namedArg <- TRUE
  names(namedArg) <- "Yes"
  checkEquals( checkTrue( namedArg), TRUE)

  ##  use function call as argument
  checkEquals( checkTrue(isTRUE(TRUE)), TRUE)
  
  ##  errorr handling
  ##  missing argument
  checkException( checkTrue( ))
  
  namedArg <- FALSE
  names(namedArg) <- "No"
  checkException( checkTrue( namedArg))
  
  checkException( checkTrue( FALSE))
  
  ##  incorrect length
  checkException( checkTrue( c(TRUE, TRUE)))
  checkException( checkTrue( c(FALSE, TRUE)))
  checkException( checkTrue( logical(0)))
  checkException( checkTrue( logical(2)))
  ##  mising value
  checkException( checkTrue( as.logical(NA)))
  
  ##  use function call as argument
  checkException( checkTrue(isTRUE(FALSE)))
  ##  use function call as argument, which throws an exception
  checkException( checkTrue(stop()))
}


testRUnit.checkException <- function()
{
  ##@bdescr
  ## test case for function checkException of class: none
  ##@edescr

  checkException( checkTrue( FALSE))
  checkException( checkTrue( ))
  checkException( checkEquals( ))
  checkException( checkEquals( 24))
  checkException( checkEquals( 24, 24, tolerance="dummy"))
  checkException( checkEqualsNumeric( ))
  checkException( checkEqualsNumeric( 24))
  checkException( checkEqualsNumeric( 24, 24, tolerance="dummy"))

  checkException( stop("with message"), silent=FALSE)
  checkException( stop("wo message"), silent=TRUE)

  ##  R 2.5.0 devel example that failed
  ##  minimal example provided by Seth Falcon
  ll = list()
  ll[[1]] = function(x) stop("died")
  checkException( do.call(ll[[1]], list(1)))

  ##  S4 objects
  if (identical(TRUE, require(methods))) {
    setClass("track2",
             representation(x="numeric", y="numeric"),
             prototype(x=as.numeric(1:23), y=as.numeric(23:1)),
             where=.GlobalEnv)
    on.exit(removeClass("track2", where=.GlobalEnv))

    s4Obj <- try(new("track2"))
    checkException( slot(s4Obj, "z"))
    checkException( slot(s4Obj, "z") <- 1:10)
  
    ##  missing method argument
    ##  coerce(from, to)
    checkException( coerce(s4Obj))
  }
  
}


testRUnit.DEACTIVATED <- function()
{
  ##@bdescr
  ## test case for function DEACTIVATED of class: none
  ##@edescr

  checkException( DEACTIVATED())
  checkException( DEACTIVATED("some message"))
  ##  compound text
  checkException( DEACTIVATED(c("some message", "some more", "and more")))
}


testRUnit.defineTestSuite <- function()
{
  ##@bdescr
  ## test case for function defineTestSuite of class: none
  ##@edescr
  
  ##  correct working
  testSuite <- defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), 
                               testFileRegexp="correctTestCase.r")
  
  ##  this also works for S3 objects
  checkTrue( inherits(testSuite, "RUnitTestSuite"))
  checkTrue( is.list(testSuite))
  checkTrue( all(c("name", "dirs", "testFileRegexp", "testFuncRegexp",
                   "rngKind", "rngNormalKind") %in% names(testSuite)))
  checkTrue( isValidTestSuite(testSuite))
  
  
  ##  error handling
  ##  'name' argument
  checkException(defineTestSuite())
  checkException(defineTestSuite( name=integer(1)))
  checkException(defineTestSuite( name=character(0)))
  
  ##  'dirs' argument
  checkException(defineTestSuite(name="RUnit Example"))
  checkException(defineTestSuite(name="RUnit Example", dirs=logical(1)))
  checkException(defineTestSuite(name="RUnit Example", dirs=character(0)))
  checkException(defineTestSuite(name="RUnit Example", dirs=as.character(NA)))
  
  ##  'testFileRegexp' argument
  checkException(defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), 
				  testFileRegexp=logical(1)))
  checkException(defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), 
				  testFileRegexp=character(0)))
  checkException(defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), 
				  testFileRegexp=character(2)))
  checkException(defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), 
				  testFileRegexp=as.character(NA)))
  
  ##  'testFuncRegexp' argument
  checkException(defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), 
				  testFuncRegexp=logical(1)))
  checkException(defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), 
				  testFuncRegexp=character(0)))
  checkException(defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), 
				  testFuncRegexp=character(2)))
  checkException(defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), 
				  testFuncRegexp=as.character(NA)))
  
  ##  'rngKind' argument
  checkException(defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), 
				  rngKind=logical(1)))
  checkException(defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), 
				  rngKind=character(0)))
  checkException(defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), 
				  rngKind=character(2)))
  checkException(defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), 
				  rngKind=as.character(NA)))
  
  ##  'rngNormalKind' argument
  checkException(defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), 
				  rngNormalKind=logical(1)))
  checkException(defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), 
				  rngNormalKind=character(0)))
  checkException(defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), 
				  rngNormalKind=character(2)))
  checkException(defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), 
				  rngNormalKind=as.character(NA)))
}


testRUnit.isValidTestSuite <- function()
{
  ##@bdescr
  ## test case for function isValidTestSuite of class: none
  ##@edescr
  
  ##  correct working
  testSuite <- defineTestSuite("RUnit Example",
                               system.file("examples", package="RUnit"),
                               testFileRegexp="correctTestCase.r")
  checkTrue( isValidTestSuite(testSuite))
  
  
  ##  error handling
  ##  has to be S3 class 'RUnitTestSuite'
  testSuiteFail <- testSuite
  class(testSuiteFail) <- "NotUnitTestSuite"
  checkTrue( !isValidTestSuite(testSuiteFail))
  
  
  ##  name
  ##  expecting list elements
  testSuiteFail <- testSuite
  testSuiteFail[["name"]] <- NULL
  checkTrue( !isValidTestSuite(testSuiteFail))
  ##  has to be character
  testSuiteFail <- testSuite
  testSuiteFail[["name"]] <- list()
  checkTrue( !isValidTestSuite(testSuiteFail))
  testSuiteFail <- testSuite
  testSuiteFail[["name"]] <- character(0)
  checkTrue( !isValidTestSuite(testSuiteFail))
  testSuiteFail <- testSuite
  testSuiteFail[["name"]] <- character(2)
  checkTrue( !isValidTestSuite(testSuiteFail))
  
  
  ##  dirs
  ##  expecting list elements
  testSuiteFail <- testSuite
  testSuiteFail[["dirs"]] <- NULL
  checkTrue( !isValidTestSuite(testSuiteFail))
  ##  has to be character
  testSuiteFail <- testSuite
  testSuiteFail[["dirs"]] <- list()
  checkTrue( !isValidTestSuite(testSuiteFail))
  ##  director has to exist
  testSuiteFail <- testSuite
  testSuiteFail[["dirs"]] <- "doesNotExist"
  checkTrue( !isValidTestSuite(testSuiteFail))
  testSuiteFail <- testSuite
  testSuiteFail[["dirs"]] <- c(tempdir(), "doesNotExist", tempdir())
  checkTrue( !isValidTestSuite(testSuiteFail))
  ##  same, '' has to return FALSE
  testSuiteFail <- testSuite
  testSuiteFail[["dirs"]] <- c(tempdir(), "", tempdir())
  checkTrue( !isValidTestSuite(testSuiteFail))
  ##  has to contain at least one element
  testSuiteFail <- testSuite
  testSuiteFail[["dirs"]] <- character(0)
  checkTrue( !isValidTestSuite(testSuiteFail))
  ##  may not be NA
  testSuiteFail <- testSuite
  testSuiteFail[["dirs"]] <- as.character(c(NA, NA))
  checkTrue( !isValidTestSuite(testSuiteFail))
  
  
  ##  testFileRegexp
  ##  expecting list elements
  testSuiteFail <- testSuite
  testSuiteFail[["testFileRegexp"]] <- NULL
  checkTrue( !isValidTestSuite(testSuiteFail))
  ##  has to be character
  testSuiteFail <- testSuite
  testSuiteFail[["testFileRegexp"]] <- list()
  checkTrue( !isValidTestSuite(testSuiteFail))
  ##  length 1 required
  testSuiteFail <- testSuite
  testSuiteFail[["testFileRegexp"]] <- character(0)
  checkTrue( !isValidTestSuite(testSuiteFail))
  testSuiteFail <- testSuite
  testSuiteFail[["testFileRegexp"]] <- character(2)
  checkTrue( !isValidTestSuite(testSuiteFail))
  ##  may not be NA
  testSuiteFail <- testSuite
  testSuiteFail[["testFileRegexp"]] <- as.character(NA)
  checkTrue( !isValidTestSuite(testSuiteFail))
  
  
  ##  testFuncRegexp
  ##  expecting list elements
  testSuiteFail <- testSuite
  testSuiteFail[["testFuncRegexp"]] <- NULL
  checkTrue( !isValidTestSuite(testSuiteFail))
  ##  has to be character
  testSuiteFail <- testSuite
  testSuiteFail[["testFuncRegexp"]] <- list()
  checkTrue( !isValidTestSuite(testSuiteFail))
  ##  length 1 required
  testSuiteFail <- testSuite
  testSuiteFail[["testFuncRegexp"]] <- character(0)
  checkTrue( !isValidTestSuite(testSuiteFail))
  testSuiteFail <- testSuite
  testSuiteFail[["testFuncRegexp"]] <- character(2)
  checkTrue( !isValidTestSuite(testSuiteFail))
  ##  may not be NA
  testSuiteFail <- testSuite
  testSuiteFail[["testFuncRegexp"]] <- as.character(NA)
  checkTrue( !isValidTestSuite(testSuiteFail))
  
  
  ##  rngKind
  ##  expecting list elements
  testSuiteFail <- testSuite
  testSuiteFail[["rngKind"]] <- NULL
  checkTrue( !isValidTestSuite(testSuiteFail))
  ##  has to be character
  testSuiteFail <- testSuite
  testSuiteFail[["rngKind"]] <- list()
  checkTrue( !isValidTestSuite(testSuiteFail))
  ##  length 1 required
  testSuiteFail <- testSuite
  testSuiteFail[["rngKind"]] <- character(0)
  checkTrue( !isValidTestSuite(testSuiteFail))
  testSuiteFail <- testSuite
  testSuiteFail[["rngKind"]] <- character(2)
  checkTrue( !isValidTestSuite(testSuiteFail))
  ##  may not be NA
  testSuiteFail <- testSuite
  testSuiteFail[["rngKind"]] <- as.character(NA)
  checkTrue( !isValidTestSuite(testSuiteFail))
  
  
  ##  rngNormalKind
  ##  expecting list elements
  testSuiteFail <- testSuite
  testSuiteFail[["rngNormalKind"]] <- NULL
  checkTrue( !isValidTestSuite(testSuiteFail))
  ##  has to be character
  testSuiteFail <- testSuite
  testSuiteFail[["rngNormalKind"]] <- list()
  checkTrue( !isValidTestSuite(testSuiteFail))
  ##  length 1 required
  testSuiteFail <- testSuite
  testSuiteFail[["rngNormalKind"]] <- character(0)
  checkTrue( !isValidTestSuite(testSuiteFail))
  testSuiteFail <- testSuite
  testSuiteFail[["rngNormalKind"]] <- character(2)
  checkTrue( !isValidTestSuite(testSuiteFail))
  ##  may not be NA
  testSuiteFail <- testSuite
  testSuiteFail[["rngNormalKind"]] <- as.character(NA)
  checkTrue( !isValidTestSuite(testSuiteFail))
  
}
  

testRUnit.runTestFile <- function()
{
  ##@bdescr
  ## test case for function runTestFile of class: none
  ##@edescr

  testFile <- file.path(system.file("examples", package="RUnit"), "correctTestCase.r")
  checkTrue( file.exists(testFile))

  ## execute runTestFile
  ## The issue: .testLogger is the hard coded logger object
  ## regenerated by each new run call in the global environment
  ## thereby overwriting the existing logger.
  ## So we copy the baseenv() logger
  checkTrue( RUnit:::.existsTestLogger())
  tmp <- get(".testLogger", envir = .GlobalEnv)
  testCaseFile <- file.path(system.file(package="RUnit"), "examples", "runitVirtualClassTest.r")
  testData <- runTestFile(testCaseFile, useOwnErrorHandler=FALSE, verbose=0L)
  checkTrue(is(testData, "RUnitTestData"))
  ##  reinstate original .testLogger
  assign(".testLogger", tmp, envir = .GlobalEnv)
  
  ##  error handling
  ##  all argument checks delegated to runTestSuite so no need for comprehensive check here
  ##  check if any argument check is reached/performed
  ##  useOwnErrorHandler
  ##  type logical
  checkException( runTestFile(testFile, useOwnErrorHandler=integer(1)))
}


testRUnit.runTestSuite <- function()
{
  ##@bdescr
  ## test case for function runTestSuite of class: none
  ##@edescr

  testSuiteTest <- defineTestSuite("RUnit Example", system.file("examples", package="RUnit"),
                                   testFileRegexp="correctTestCase.r")

  checkTrue( isValidTestSuite(testSuiteTest))

  ## execute runTestSuite
  ## The issue: .testLogger is the hard coded logger object
  ## regenerated by each new run call in the global environment
  ## thereby overwriting the existing logger.
  ## So we copy the baseenv() logger
  checkTrue( RUnit:::.existsTestLogger())
  tmp <- get(".testLogger", envir = .GlobalEnv)
  testCaseDir <- file.path(system.file(package="RUnit"), "examples")
  testSuiteInternal <- defineTestSuite("RUnit Self Test", testCaseDir, "correctTestCase.r")
  testData <- runTestSuite(testSuiteInternal, useOwnErrorHandler=FALSE, verbose=0L)
  checkTrue(is(testData, "RUnitTestData"))
  ##  reinstate original .testLogger
  assign(".testLogger", tmp, envir = .GlobalEnv)
  
  
  ##  error handling
  ##
  ##  useOwnErrorHandler
  ##  type logical
  tS <- testSuiteTest
  checkException( runTestSuite(tS, useOwnErrorHandler=integer(1)))
  ##  length 1
  checkException( runTestSuite(tS, useOwnErrorHandler=logical(0)))
  checkException( runTestSuite(tS, useOwnErrorHandler=logical(2)))
  ##  missing value
  checkException( runTestSuite(tS, useOwnErrorHandler=as.logical(NA)))
  ##  verbose
  ##  type
  checkException( runTestSuite(tS, verbose=TRUE))
  ##  length
  checkException( runTestSuite(tS, verbose=integer(0)))
  checkException( runTestSuite(tS, verbose=integer(2)))
  ##  out of range
  checkException( runTestSuite(tS, verbose= -1L))
  ##  missing value
  checkException( runTestSuite(tS, verbose=as.integer(NA)))
  
}
