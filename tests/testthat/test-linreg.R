test_that("Whether 'linr' function have the same output values with the 'lm' function", {
  # Test model 1
  y = rnorm(300)
  x = rnorm(300)
  model.linr <- linreg(y,x)
  model.lm <- lm(y ~ x)
  sum = summary(model.lm)
  expect_equal(as.vector(model.linr$coefficients[,1]), as.vector(sum$coefficients[,1]))
  expect_equal(as.vector(model.linr$coefficients[,2]), as.vector(sum$coefficients[,2]))
  expect_equal(as.vector(model.linr$coefficients[,3]), as.vector(sum$coefficients[,3]))
  expect_equal(as.vector(model.linr$coefficients[,4]), as.vector(sum$coefficients[,4]))
})
