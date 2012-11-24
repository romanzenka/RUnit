%%  RUnit : A unit test framework for the R programming language
%%  Copyright (C) 2003-2012  Thomas Koenig, Matthias Burger, Klaus Juenemann
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
  Each test case has its enclosing environment, the so called test fixture. This is 
  relevant so as to ensure test cases do not influence each other e.g. by leaving modified 
  test data around or altering global options. To ease test case authoring repetitive setup steps required 
  for each test case in a set (identified as the set of test cases grouped into one test case code file) can  
  be defined in one function: \code{.setUp()}, which will be executed 
  before execution of each single test case. Likewise \code{.tearDown()} can be defined to reset and clean up after execution of the test case.
  RUnit by default provides dummy implementations for both function that have no effect. 
  By overriding the default the test fixture can be modified in various ways. I.e. required \code{options()}, 
  \code{par()} settings, test variables,
  test data objects, file, url, or data base connections can be set, established, opened/loaded.
  If used within a test fixture (code file) the corresponding clean up/restore/free/free connection operations should be
  coded in function \code{.tearDown()}. Care should be taken to leave no changed option/par settings or environment changes
  that might have side effects on subsequent test cases - such side effects are notoriously hard to spot.
  
}

\details{
  Either one or both functions may be provided by the test case author, take
  precedence over the dummy definitions provided by the RUnit
  package and are called once for every test case identified within a test code file.
  
  See the test cases provided for RUnit itself for examples e.g. file\cr \file{RUnit/unitTests/runitTestProtocol.r}.
}

\value{
    Functions do not return a value; called for their side effects.
}
    
\author{
Thomas \enc{König}{Koenig}, Klaus \enc{Jünemann}{Juenemann} 
\ifelse{html}{\out{&amp;}}{&} Matthias Burger}

\seealso{
  \code{\link{runTestFile}}.
}

\keyword{programming}
\concept{RUnit}
