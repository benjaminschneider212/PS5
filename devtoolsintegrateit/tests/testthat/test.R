context("integrateIt")

test_that("integrate it has errors",{
 expect_that(integrateIt(c(4,5), c(4,5), c(4,5),"banana"),
             equals("Please select a valid rule"))
 expect_that(integrateIt(c(4,6), c(4,5), c(4,5),"trapexoid"),
             equals("make sure your x and y are of equal length"))
})
test_that("correct values are provided",{
 expect_that(integrateIt(c(4,5), c(4,5), c(4,5),"Trap"),
             equals(new("Trapezoid", result=4.5, x=c(4,5), y=c(4,5))))
  expect_that(integrateIt(x=c(4,5), y=c(4,5), bounds=c(4,5), rule="Simpson"),
              equals(new("Simpson", result=c, x=c(4,5), y=c(4,5))))
})

context("print")

test_that("integrate it has errors",{
  expect_that(print(c(4,5), c(4,5), c(4,5),"banana"),
              equals("Please select a valid rule"))
  expect_that(print(c(4,6), c(4,5), c(4,5),"trapexoid"),
              equals("make sure your x and y are of equal length"))
})
test_that("correct values are provided",{
  expect_that(print(c(4,5), c(4,5), c(4,5),"Trap"),
              equals(4.5))
  expect_that(print(x=c(4,5), y=c(4,5), bounds=c(4,5), rule="Simpson"),
              equals(3))
})