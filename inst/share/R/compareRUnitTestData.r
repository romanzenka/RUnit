

##  $Id$

compare <- function(td1, td2, tolerance=100) {
  ##@bdescr
  ##  compare two test suite result data objects
  ##  identify timing differences exceeding 'tolerance' [secconds]
  ##@edescr
  ##
  ##
  ##@in  td1 : [RUnitTestData]
  ##@in  td2 : [RUnitTestData]
  ##@in  tolerance : [numeric] 
  ##@ret  : []
  ##
  ##@codestatus : untested

  ##  preconditions
  if (!is(td1, "RUnitTestData")) {
    stop("argument 'td1' has to be of class 'RUnitTestData'.")
  }
  if (!is(td2, "RUnitTestData")) {
    stop("argument 'td2' has to be of class 'RUnitTestData'.")
  }
  if (length(tolerance) != 1 || is.na(tolerance) || tolerance < 0) {
    stop("argument 'tolerance' has to be positive scalar.")
  }

  ##  helper functions
  commonNames <- function(x1, x2) {
    return(intersect(names(x1), names(x2)))
  }
  compareTiming <- function(x1, x2, tol=0) {
    d <- x1 - x2
    if (abs(d) > tol) {
      return(d)
    } else {
      return(as.numeric(0))
    }
  }
 
  comparePerSourceFile <- function(sf1,sf2, tol=0) {
    commonTests <- commonNames(sf1, sf2)
    t(sapply(commonTests, function(x, obj1, obj2) {
      ##cat("\n test:", x, "\n")
      if(obj1[[x]][["kind"]] == obj2[[x]][["kind"]]) {
        if (obj1[[x]][["kind"]] == "success") {
          return(c(x, obj1[[x]][["kind"]], obj1[[x]][["time"]],
                   obj2[[x]][["kind"]], obj2[[x]][["time"]],
                   compareTiming(obj1[[x]][["time"]],
                                 obj2[[x]][["time"]],
                                 tol=tol)))
        } else {
          return(c(x, obj1[[x]][["kind"]], as.numeric(NA),
                   obj2[[x]][["kind"]], as.numeric(NA), as.numeric(NA)))
        }
      } else {
        ##  no timing delta
        ##  should chekc for timing in second case
        ##  obj2[[x]][["time"]])
        return(c(x, obj1[[x]][["kind"]], as.numeric(NA),
                 obj2[[x]][["kind"]], as.numeric(NA), as.numeric(NA)))

      }
      
    }, obj1=sf1, obj2=sf2))
  }
  
  comparePerSuite <- function(s1,s2, tol=0) {
    ##  absolute file names recorded, strip path
    commonFiles <- intersect(basename(names(s1[["sourceFileResults"]])),
                             basename(names(s2[["sourceFileResults"]])))
    do.call("rbind", sapply(commonFiles, function(x, obj1, obj2) {
      ##  match exact file name in abs. name
      idx1 <- match(x, basename(names(obj1)))
      idx2 <- match(x, basename(names(obj2)))
      if (length(idx1) != 1 || is.na(idx1) || length(idx2) != 1 || is.na(idx2)) {
        stop("ambigous file name.")
        next;
      }
      comparePerSourceFile(obj1[[idx1]], obj2[[idx2]], tol=tol)
    }, obj1=s1[["sourceFileResults"]], obj2=s2[["sourceFileResults"]]))

  }
           

  ##  main
  ##  test suites to compare
  commonTestSuites <- commonNames(td1, td2)

  res <- matrix(ncol=6, nrow=0)
  colnames(res) <- c("TestCase", "Suite1 State", "Suite1 Timing", "Suite2 State", "Suite2 Timing", "Delta")
  for (ti in seq(along=commonTestSuites)) {
    res <- rbind(res, comparePerSuite(td1[[commonTestSuites[ti]]], td2[[commonTestSuites[ti]]],
                                      tol=tolerance))
  }
  
  ##  postcondition
  return(res)
}

