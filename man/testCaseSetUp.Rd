%%  RUnit : A unit test framework for the R programming language
%%  Copyright (C) 2003-2009  Thomas Koenig, Matthias Burger, Klaus Juenemann
%%
%%  This program is free software; you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation; version 2 of the License.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with this program; if not, write to the Free Software
%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

%%  $Id$

\encoding{latin1}
\name{.setUp}
\alias{.setUp}
\alias{.tearDown}
\title{Definition of RUnit Test Case code files.}

\usage{
.setUp()
.tearDown()
}

\description{
  Both functions have to be provided by the test case author, take
  precedence over the dummy definitions provided by the \pkg{RUnit}
  package and are called one for every test case identified.
}

\details{
  To be written ...
  Meanwhile take a look at a simple test case file excerpt:
  \verb{

    ##  test case file header

    ##  function setUp with no argument
    ##  defined only for this test case file
    .setUp <- function() {
      require(lme4)

      ##  generate reference data
      fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
    }

    .tearDown <- function() {

      ##  clean up
      if (exists("fm1")) {
        rm(fm1)        
      }

      ##  catch error in case lme4 is required by another loaded package
      ##  whence R cannot unload the namespace
      try(unloadNamespace("lme4"))
    }


    test_doSomeChecks <- function() {

      fm2 <- lmer(Reaction ~ Days + (1|Subject) + (0+Days|Subject), sleepstudy)
      result <- anova(fm1, fm2)

      checkTrue( inherits(result, "anova"))

      ##  some sensible checks
      ##  ...
    }
    
  }
}

\value{
    Functions do not return a value; called for their side effects.}
    
\author{Thomas \enc{König}{Koenig}, Klaus \enc{Jünemann}{Juenemann} 
\ifelse{html}{\out{&amp;}}{\&} Matthias Burger}

\seealso{
  \code{\link{runTestFile}}.
}

\keyword{programming}
\concept{RUnit}
