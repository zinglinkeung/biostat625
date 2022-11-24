test_that("Whether two function are equal in simple linear regression with intercept", {
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

test_that("Whether two function are equal in simple linear regression without intercept", {
  # Test model 2
  y = rnorm(300)
  x = rnorm(300)
  model.linr <- linreg(y,x,intercept = F)
  model.lm <- lm(y ~ -1+x)
  sum = summary(model.lm)
  expect_equal(as.vector(model.linr$coefficients[,1]), as.vector(sum$coefficients[,1]))
  expect_equal(as.vector(model.linr$coefficients[,2]), as.vector(sum$coefficients[,2]))
  expect_equal(as.vector(model.linr$coefficients[,3]), as.vector(sum$coefficients[,3]))
  expect_equal(as.vector(model.linr$coefficients[,4]), as.vector(sum$coefficients[,4]))
})

test_that("Whether two function are equal in multiple linear regression with intercept", {
  # Test model 3
  y = rnorm(300)
  x = matrix(rnorm(900),nrow=300,ncol=3)
  model.linr <- linreg(y,x)
  model.lm <- lm(y ~ x)
  sum = summary(model.lm)
  expect_equal(as.vector(model.linr$coefficients[,1]), as.vector(sum$coefficients[,1]))
  expect_equal(as.vector(model.linr$coefficients[,2]), as.vector(sum$coefficients[,2]))
  expect_equal(as.vector(model.linr$coefficients[,3]), as.vector(sum$coefficients[,3]))
  expect_equal(as.vector(model.linr$coefficients[,4]), as.vector(sum$coefficients[,4]))
})

test_that("Whether two function are equal in multiple linear regression without intercept", {
  # Test model 4
  y = rnorm(300)
  x = matrix(rnorm(900),nrow=300,ncol=3)
  model.linr <- linreg(y,x,intercept=F)
  model.lm <- lm(y ~ -1+x)
  sum = summary(model.lm)
  expect_equal(as.vector(model.linr$coefficients[,1]), as.vector(sum$coefficients[,1]))
  expect_equal(as.vector(model.linr$coefficients[,2]), as.vector(sum$coefficients[,2]))
  expect_equal(as.vector(model.linr$coefficients[,3]), as.vector(sum$coefficients[,3]))
  expect_equal(as.vector(model.linr$coefficients[,4]), as.vector(sum$coefficients[,4]))
})
