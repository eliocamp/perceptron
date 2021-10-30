perceptron_train <- function(X, y, epochs = 100, lambda = 10) {

  X <- cbind(1, X)
  W <- rnorm(ncol(X))

  accuracy <- rep(NA,  epochs)

  for (e in seq_len(epochs)) {
    for (i in seq_len(nrow(X))) {
      y_hat <- sum(W * X[i, ])

      pred <- sign(as.numeric(y_hat >= 0) - 0.5)
      # pred <- y_hat

      W <- W + lambda*(y[i] - pred)*X[i, ]
    }
    W <- scale(W)
    y_hat <- X %*% matrix(W, ncol = 1)
    pred <- sign(as.numeric(y_hat >= 0) - 0.5)
    # pred <- y_hat

    # accuracy[e] <- sd(y - pred)
    accuracy[e] <- mean((y - pred) == 0)

    cat(accuracy[e], "\n")
    if (accuracy[e] == 1) break
  }
  return(W)
}

