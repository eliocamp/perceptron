perceptron_train <- function(X, y, epochs = 100, lambda = 10) {
  X <- cbind(X)
  W <- rnorm(ncol(X))
  Ws <- vector("list", epochs)
  accuracy <- rep(NA,  epochs)

  for (e in seq_len(epochs)) {
    for (i in seq_len(nrow(X))) {
      y_hat <- sum(W * X[i, ])

      pred <- sign(as.numeric(y_hat >= 0) - 0.5)

      W <- W + lambda*(y[i] - pred)*X[i, ]
    }
    
    W <- W/sqrt(sum(W^2))
    y_hat <- X %*% matrix(W, ncol = 1)
    pred <- sign(as.numeric(y_hat >= 0) - 0.5)

    accuracy[e] <- mean((y - pred) == 0)
    Ws[[e]] <- W
    cat(accuracy[e], "\n")
    if (accuracy[e] == 1) break
    
  }

  return(Ws[[which.max(accuracy)]])
}

