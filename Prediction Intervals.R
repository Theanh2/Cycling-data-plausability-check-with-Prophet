#-------------------------------# 
#----PI with data variability----
#-------------------------------# 

theoryPI <- function(model, data, direction, confidence) {
  pred <- predict(model, data, type = "link")
  mu <- model$family$linkinv(pred)
  theta <- model$family$getTheta(trans = TRUE)
  
  alpha = 1 - confidence
  
  if (direction == 1) {
    data <- data %>%
      mutate(y_pred = mu,
             lb = qnbinom(p = alpha/2, size = theta, mu = mu),
             ub = qnbinom(p = 1-alpha/2, size = theta, mu = mu),
             cov = direction_1 <= ub & direction_1 >= lb)
  } else {
    data <- data %>%
      mutate(y_pred = mu,
             lb = qnbinom(p = alpha/2, size = theta, mu = mu),
             ub = qnbinom(p = 1-alpha/2, size = theta, mu = mu),
             cov = direction_2 <= ub & direction_2 >= lb)
  }
  return(data)
}

#---------------------------------------------------------------# 
#----PI with model and data variabilities based on simulation----
#---------------------------------------------------------------#
simulation <- function(model, data, direction, n_sims, confidence, seed) {
  
  set.seed(seed)
  
  Z <- predict(model, data, type = "lpmatrix")
  
  require(mgcv)
  B <- rmvn(n_sims, coef(model), model$Vp)
  
  mu_mat <- matrix(0, nrow = nrow(data), ncol = n_sims)
  
  for(i in 1:n_sims) {
    eta <- Z %*% B[i, ]
    mu_mat[, i] <- exp(eta)
  }
  
  theta <- model$family$getTheta(trans = TRUE)
  
  ret <- matrix(
    rnbinom(nrow(data) * n_sims, mu = as.vector(mu_mat), size = theta),
    nrow = nrow(data)
  )
  
  ret <- as.data.frame(ret)
  
  alpha = 1 - confidence
  pis <- t(apply(ret, 1, quantile, c(alpha/2, 1-alpha/2), na.rm = TRUE))
  
  if (direction == 1) {
    data <- data %>%
      mutate(
        y_pred = model$family$linkinv(predict(model, data, type = 'link')),
        lb = pis[, 1],
        ub = pis[, 2],
        cov = direction_1 <= ub & direction_1 >= lb)
  } else { # eventually ifelse to catch user error
    data <- data %>%
      mutate(
        y_pred = model$family$linkinv(predict(model, data, type = 'link')),
        lb = pis[, 1],
        ub = pis[, 2],
        cov = direction_2 <= ub & direction_2 >= lb)
  }
  
  return(data)
}
