test.foo <- function() {

  checkTrue(is.numeric(foo(1:10)))
  checkEquals(length(foo(1:10)), 10)
  checkEqualsNumeric(foo(1), 2)
}
