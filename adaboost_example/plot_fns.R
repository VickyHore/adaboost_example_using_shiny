## plotting functions for adaboost example 

to_tr <- function(d) {
  # map log(d) onto the range (mina, maxa)

  d <- log(d)
  mina <- 0.2; maxa <- 1
  mind <- min(d); maxd <- max(d)
  (d - mind) / (maxd - mind) * (maxa - mina) + mina
}

to_col <- function(y) {

  ifelse(y == 1, 'red', 'blue')
}

desc_boundary <- function(x, coeff) {
  # evaluate points on a line defined by coeff at point(s) x

  (-coeff[1] - coeff[2]*x) / coeff[3]
}

predict_desc_boundary <- function(grid, coeff) {
  # evaluate whether points on a grid are above/below the decision boundary defined by coeff

  vals <- coeff[1] + grid[,1] * coeff[2] + grid[,2] * coeff[3]
  ifelse(vals > 0, 1, -1)
}

plot_weak_classifier <- function(ab) {

  coeff <- ab$coeffs[,ab$it]

  dbx <- c(-10, 10)
  dby <- desc_boundary(dbx, coeff)

  plot(ab$train$X1[ab$h$sams], ab$train$X2[ab$h$sams], 
         col=alpha(to_col(ab$train$y[ab$h$sams]), 1), 
         bty='n', pch=19, cex=0.9, 
         xlab=expression('x'[1]), ylab=expression('x'[2]), 
         main="Weak classifier", 
         xlim=range(ab$train$X1), ylim=range(ab$train$X2)) 
  points(ab$train$X1[-ab$h$sams], ab$train$X2[-ab$h$sams], 
         col=alpha(to_col(ab$train$y[-ab$h$sams]), 0.2), 
         pch=19, cex=0.7) 
  lines(dbx, dby, lwd=2, lty=2) # plot decision boundary
  grid()

  # colour background of plot according to classification
  a <- coeff; ay <- desc_boundary(dbx, a)
  fn <- coeff[1] + dbx[1]*coeff[2] + ay[1]*coeff[3]

  cols <- c(rgb(1, 0, 0, 0.2), rgb(0, 0, 1, 0.2))

  b <- a; b[1] <- b[1] - 10; by <- desc_boundary(dbx, b)
  polygon(c(dbx, rev(dbx)), c(ay, rev(by)), 
      col=ifelse(fn > 0, cols[2], cols[1]), border='black')

  a <- coeff; ay <- desc_boundary(dbx, a)
  b <- a; b[1] <- b[1] + 10; by <- desc_boundary(dbx, b)
  polygon(c(dbx, rev(dbx)), c(ay, rev(by)), 
      col=ifelse(fn > 0, cols[1], cols[2]), border='black')

}


plot_point_weights <- function(ab) {

  # size of the points correspond to strength of weight
  plot(ab$train$X1, ab$train$X2, 
         col=alpha(to_col(ab$train$y), 0.5),
         bty='n', pch=19, cex=to_tr(ab$D), 
         xlab=expression('x'[1]), ylab=expression('x'[2]), 
         main="Weight distribution", 
         xlim=range(ab$train$X1), ylim=range(ab$train$X2)) 
}



plot_errors <- function(ab) {

  plot(1:length(ab$H_error), ab$H_error, bty="n", 
    type='l', ylab="Error", xlab="Iteration", 
    ylim=c(0, 0.6), cex=0.5, col='blueviolet', 
    main="AdaBoost classifier error", lwd=2)
  points(1:length(ab$H_error_test), ab$H_error_test, 
      type='l', cex=0.5, col="deeppink", lwd=2)
  legend('topright', c('training error', 'test error'), 
      lty=1, col=c('blueviolet', "deeppink"), lwd=2)
}



plot_best_classifier <- function(ab) {

  plot(ab$train$X1, ab$train$X2,
     col=to_col(ab$train$y), 
     bty='n', cex=0.7, pch=19,
     xlab=expression('x'[1]), ylab=expression('x'[2]), 
     main="AdaBoost classifier", 
     xlim=range(ab$train$X1), ylim=range(ab$train$X2)) 

  # background colour indicates the classification
  # create a grid and evaluate the prediction at each point on the grid
  nl <- 100
  gridx <- seq(-1.2, 1.2, len=nl)
  gridy <- seq(-1.2, 1.2, len=nl)
  gridx <- rep(gridx, nl)
  gridy <- sort(rep(gridy, nl))
  grid <- cbind(gridx, gridy)

  segs <- apply(ab$coeffs[,1:ab$it, drop=FALSE], 2, function(o) predict_desc_boundary(grid, o))
  segs <- ifelse(segs %*% ab$alpha[1:ab$it] > 0, 1, -1)

  blues <- which(segs == -1)

  cols <- c(rgb(1, 0, 0, 0.2), rgb(0, 0, 1, 0.2))

  points(grid[blues,1], grid[blues,2], col=cols[2], pch=15, cex=0.6)
  points(grid[-blues,1], grid[-blues,2], col=cols[1], pch=15, cex=0.6)
}

