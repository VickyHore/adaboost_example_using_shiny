## code to run the adaboost on a binary classification problem

simulate_data <- function(n, m) {
  # n : size of training set
  # m : size of test set

  data <- data.frame(X1=runif(n+m, -1, 1), 
                     X2=runif(n+m, -1, 1))
  data$y <- ifelse((data$X1>0&data$X2>0)|(data$X1<0&data$X2<0), 1, -1)

  return(list(train=data[1:n,], test=data[(n+1):(n+m),]))
}

classifier_weight <- function(error) {

  0.5 * log((1-error)/error)
}

weak_classifier <- function(train, test, weights) {
  # use logistic regression as a weak classifier
  # train on a sub-sample of the data, sampling points according to weight distribution

  n <- nrow(train)
  sams <- sample(n, round(n/10), prob=weights)

  train_sams <- train[sams,]
  train_sams$y <- ifelse(train_sams$y == 1, 1, 0)
  model <- glm(y ~ X1 + X2, 
             family = binomial(link='logit'), data=train_sams)

  class <- ifelse(predict(model, newdata=train, type="response")>0.5, 1, -1)
  error <- sum(weights * (class != train$y)) / sum(weights) # weighed error
  
  alpha <- classifier_weight(error)
  class_test <- ifelse(predict(model, newdata=test, type='response')>0.5, 1, -1)

  list(class=class, coeff=model$coeff, error=error, 
      alpha=alpha, sams=sams, class_test=class_test)
}

data_weights <- function(D, alpha, h_class, trainy) {

  weights <- D * exp(-alpha * h_class * trainy)
  weights / sum(weights) # normalise
}


adaboost_initialise <- function(data, n_it) {
  # data : list consisting of a training and test set
  # n_it : maximum number of iterations

  ab <- list()

  ab$n <- nrow(data$train)
  ab$m <- nrow(data$test)

  ab$D <- rep(1/ab$n, ab$n) # training data weights

  # combined classifier results/output on training set
  ab$H_error <- rep(NA, n_it) # error
  ab$H_class <- rep(0, ab$n) # predictions 
  ab$H_probs <- ab$H_class # weighted sum of weak classifier output (used to evaluate predictions)

  # combined classifier results/output on test set
  ab$H_error_test <- rep(NA, n_it) 
  ab$H_class_test <- rep(0, ab$m) 
  ab$H_probs_test <- ab$H_class_test

  ab$coeffs <- matrix(NA, 3, n_it) # coeff defining weak classifier decision boundary
  ab$alpha <- rep(NA, n_it) # classifier weights

  ab$it <- 0
  ab$n_it <- n_it 

  ab$test <- data$test
  ab$train <- data$train

  return(ab)
}

adaboost_one_it <- function(ab) {
  # ab : list from adaboost_initialise, or output from this function

  # train weak classifier
  repeat{
    ab$h <- weak_classifier(ab$train, ab$test, ab$D)
    if (ab$h$error < 0.5) {
      break
    }
  }

  ab$it <- ab$it + 1

  ab$coeffs[, ab$it] <- ab$h$coeff
  ab$alpha[ab$it] <- ab$h$alpha
  
  # update adaboost classifier
  ab$H_probs <- ab$H_probs + ab$h$class * ab$alpha[ab$it]
  ab$H_class <- ifelse(ab$H_probs > 0, 1, -1)
  ab$H_error[ab$it] <- sum(ab$H_class != ab$train$y) / ab$n

  ab$H_probs_test <- ab$H_probs_test + ab$h$class_test * ab$alpha[ab$it]
  ab$H_class_test <- ifelse(ab$H_probs_test > 0, 1, -1)
  ab$H_error_test[ab$it] <- sum(ab$H_class_test != ab$test$y) / ab$m

  ab$D <- data_weights(ab$D, ab$h$alpha, ab$h$class, ab$train$y)

  ab
}

