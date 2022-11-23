#'linreg
#'
#'fit simple regression models and multiple regression models.
#'
#'@importFrom stats pf pt qt
#'
#'@import boot bench
#'
#'@param y a column vector, treated as outcome.
#'
#'@param x a numerical matrix, treated as predictor.
#'
#'@param method Character value, "qr" (by default), "svd" or "inverse"
#'
#'@param intercept logical, "TRUE" by default.
#'If "FALSE", the model will not include the intercept.
#'
#'@param CI_level Numeric number, 0.95 (by default).
#'Should be in (0,1) and directly change alpha used in confidence interval.
#'
#'@return A list contains regression results.
#'
#'@examples
#'  x <- matrix(c(-1, -2, -3, -4, -5, 1, 3, 5, 7, 9), nrow = 5)
#'  y <- c(2, 5, 8, 11, 19)
#'  # fit the linear model without an intercept
#'  linreg(y, x, intercept = FALSE)
#'
#'@export
#'

linreg <- function(y,x,
                   method = "qr",
                   intercept=T,
                   CI_level = 0.95){

  method_type = c("inverse","qr",'svd')
  slr <- FALSE

  #define covariate matrix and outcome matrix.
  x <- as.matrix(x)
  y <- as.matrix(y)
  ybar <- mean(y)

  n <- nrow(y)
  p <- ncol(x)

  if (n < p){
    stop("Number of covariates should more than number of observations.")
  }

  #fit regression model with intercept
  if (intercept == T){
    p = p + 1
    SSY <- sum((y-ybar)^2)

    if (p == 2L){  #fit simple linear model
      x <- as.vector(x)
      y <- as.vector(y)

      xbar <- mean(x)

      SSX <- sum((x-xbar)^2)
      SSXY <- sum((y-ybar)*(x-xbar))

      hat_beta1 <- SSXY/SSX
      hat_beta0 <- ybar - hat_beta1*xbar
      hat_beta <- c(hat_beta0,hat_beta1)

      hat_y <- hat_beta0 + hat_beta1 * x
      residuals <- y - hat_y
      SSE <- sum(residuals ^ 2)
      MSE <- SSE/(n-2)

      se_b1 <- sqrt(MSE / SSX)
      se_b0 <- sqrt(MSE * (1 / n + xbar ^ 2 / SSX))
      se_beta <- c(se_b0,se_b1)

      slr <- TRUE
    }

    else {
      x = cbind(rep(1, n), x)
    }
  }

  else{
    SSY <- sum(y^2)
  }

  if (slr == F){
    if (!method %in% method_type){
      stop(gettextf("method = '%s' is not supported. Using 'qr', 'svd' or 'inverse'", method), domain = NA)
    }

    else if (method == "qr"){
      inter_qr <- qr(x, lapack=TRUE)
      hat_beta <- qr.coef(inter_qr, y)
    }

    else if (method =="svd"){
      inter_svd <- svd(x)
      m1 <-  crossprod(inter_svd$u, y)
      hat_beta <- crossprod(t(inter_svd$v), (m1 / inter_svd$d))
    }

    else{
      hat_beta <- solve(t(x) %*% x) %*% t(x) %*% y
    }

   hat_y <- as.vector(crossprod(t(x), hat_beta))
   residuals <- as.vector(y) - hat_y

   SSE <- sum(residuals**2)
   MSE <- SSE/(n-p)

   var_beta <- MSE * diag(solve(t(x) %*% x))
   se_beta <- sqrt(var_beta)
  }

  R_squared <- 1 - SSE/SSY
  Adj.R_squared <- 1 - (n - 1) * MSE / SSY
  R.squared <- list(R.squared = R_squared, Adjusted.R.squared = Adj.R_squared)

  t_stat <- hat_beta / se_beta
  p_value_t = pt(abs(t_stat), n - p, lower.tail = FALSE) * 2

  df1 <- p - intercept
  df2 <- n - p
  f_stat <- (SSY - SSE) / df1 / MSE
  p_value_f <- pf(f_stat, df1, df2, lower.tail = FALSE)
  f_table <- list(F_statistics = f_stat,
                  p_value = p_value_f,
                  df1 = df1,
                  df2 = df2)

  ci_t <- qt(p = 1 - (1 - CI_level) / 2, df = (n - p))
  upper_ci <- hat_beta + ci_t * se_beta
  lower_ci <- hat_beta - ci_t * se_beta
  ci <- paste0("(",lower_ci,", ",upper_ci,")")

  if (length(colnames(x)) == 0){
    if (intercept == T){
      vars <- c("(Intercept)",paste0("variable_",1:(p-1)))
      call_name <- c(paste0("variable_",1:(p-1)),"intercept")
    }
    else{
      vars <- c(paste0("variable_",1:(p)))
      call_name <- vars
    }
  }

  else{
    if (intercept == T){
      vars <- c("(Intercept)",colnames(x)[-1])
      call_name <- c(colnames(x)[-1],"intercept")
    }
    else{
      vars <- colnames(x)
      call_name <- vars
    }
  }

  names(hat_beta) = vars
  names(se_beta) = vars
  names(ci) = vars
  names(t_stat) = vars
  names(p_value_t) = vars
  names(hat_y) = 1:n
  names(residuals) = 1:n

  if (length(colnames(y)) == 0){
    outcome <- "y"
  }
  else{
    outcome <- colnames(y)
  }
  coef_table <- cbind(Estimate = c(hat_beta),
                "Std. Error" = c(se_beta),
                #ci = c(ci),
                "t value" = c(t_stat),
                "Pr(>|t|)"  = c(p_value_t))
  #colnames(coef_table)[3] <- gettextf("%.f%% CI",CI_level*100)

  call <- paste0 (outcome," ~ ",paste0(call_name,collapse = "+"))

  output <- list(Call = call,
                 coefficients = coef_table,
                 fitted.values = hat_y,
                 residuals = residuals,
                 MSE = MSE,
                 lower_ci = lower_ci,
                 upper_ci = upper_ci,
                 R_squared = R.squared,
                 F_test = f_table)
  return(output)
}










