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

test_that("Whether two function are equal in multiple linear regression with svd", {
  # Test model 5
  y = rnorm(300)
  x = matrix(rnorm(900),nrow=300,ncol=3)
  model.linr <- linreg(y,x,method="svd")
  model.lm <- lm(y ~ x)
  sum = summary(model.lm)
  expect_equal(as.vector(model.linr$coefficients[,1]), as.vector(sum$coefficients[,1]))
  expect_equal(as.vector(model.linr$coefficients[,2]), as.vector(sum$coefficients[,2]))
  expect_equal(as.vector(model.linr$coefficients[,3]), as.vector(sum$coefficients[,3]))
  expect_equal(as.vector(model.linr$coefficients[,4]), as.vector(sum$coefficients[,4]))
})

test_that("Whether two function are equal in multiple linear regression with svd", {
  # Test model 6
  y = rnorm(300)
  x = matrix(rnorm(900),nrow=300,ncol=3)
  model.linr <- linreg(y,x,method="inverse")
  model.lm <- lm(y ~ x)
  sum = summary(model.lm)
  expect_equal(as.vector(model.linr$coefficients[,1]), as.vector(sum$coefficients[,1]))
  expect_equal(as.vector(model.linr$coefficients[,2]), as.vector(sum$coefficients[,2]))
  expect_equal(as.vector(model.linr$coefficients[,3]), as.vector(sum$coefficients[,3]))
  expect_equal(as.vector(model.linr$coefficients[,4]), as.vector(sum$coefficients[,4]))
})

test_that("Error 1", {
  y = rnorm(10)
  x = matrix(rnorm(200),nrow=10,ncol=20)
  expect_error(linreg(y,x), "Number of covariates should more than number of observations.")
})

test_that("Error 2", {
  y = rnorm(300)
  x = matrix(rnorm(900),nrow=300,ncol=3)
  expect_error(linreg(y,x, method = "wrong"), "method = 'wrong' is not supported. Using 'qr', 'svd' or 'inverse'")
})

test_that("Whether two function are equal in multiple linear regression with svd", {
  # Test model 7
  y <- aids$y
  x <- as.matrix(aids[,c(2,3)])
  model.linr <- linreg(y,x)
  model.lm <- lm(y ~ x)
  sum = summary(model.lm)
  expect_equal(as.vector(model.linr$coefficients[,1]), as.vector(sum$coefficients[,1]))
  expect_equal(as.vector(model.linr$coefficients[,2]), as.vector(sum$coefficients[,2]))
  expect_equal(as.vector(model.linr$coefficients[,3]), as.vector(sum$coefficients[,3]))
  expect_equal(as.vector(model.linr$coefficients[,4]), as.vector(sum$coefficients[,4]))
})

test_that("Whether two function are equal in multiple linear regression with svd", {
  # Test model 7
  y <- aids$y
  x <- as.matrix(aids[,c(2,3)])
  model.linr <- linreg(y,x,intercept = F)
  model.lm <- lm(y ~ -1+x)
  sum = summary(model.lm)
  expect_equal(as.vector(model.linr$coefficients[,1]), as.vector(sum$coefficients[,1]))
  expect_equal(as.vector(model.linr$coefficients[,2]), as.vector(sum$coefficients[,2]))
  expect_equal(as.vector(model.linr$coefficients[,3]), as.vector(sum$coefficients[,3]))
  expect_equal(as.vector(model.linr$coefficients[,4]), as.vector(sum$coefficients[,4]))
})
