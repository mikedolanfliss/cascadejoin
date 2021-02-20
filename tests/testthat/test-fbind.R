test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
test_that("fbind() binds a factor or char", {
 x = letters
 x_fact = factor(x)
 y = LETTERS
 y_fact = factor(y)
 z = factor(c(letters, LETTERS))

 expect_identical(fbind(x, y), z)
 expect_identical(fbind(x_fact, y), z)
})
# TODO Use covr for code test coverage
