#' DALMKL 
#'
#' This function conducts DALMKL for precomputed gramm matrices
#' @param K The multiple kernel cube (3-d array)
#' @param y The outome variable, must be -1/1
#' @param loss The loss function to be used, must be either 'hinge' or 'logistic', default to be 'hinge'
#' @param C tuning parameter for block one norm, default to be .5
#' @param tolOuter change between to iterations is smaller than this, algorithms is considered to have converged for outer loop, default to be .01
#' @param tolInner change between to iterations is smaller than this, algorithms is considered to have converged for inner loop, default to be .000001
#' @param OuterMaxiter maximum number of allowed iteratons for outer loop, default to be 500
#' @param InnerMaxiter maximum number of allowed iteratons for inner loop, default to be 500
#' @param calpha Lagrangian parameter, default to be 10
#' @return b Estimated Intercept 
#' @return alpha coeffiencents of the dual of MKL
#' @return weight Estimated between kernel weight
#' @return rho Estimated within kernel weight
#' @export
SpicyMKL <- function(K, y, loss = 'hinge', C = .5, tolOuter = .01, tolInner = .000001, OuterMaxiter = 500, InnerMaxIter = 500, calpha = 10) {
  if (loss == 'hinge') {
    SpicySVM(K, y, C, tolOuter, tolInner, OuterMaxIter, InnerMaxIter, calpha)
  } else {
    SpicyLogit(K, y, C, tolOuter, tolInner, OuterMaxIter, InnerMaxIter, calpha)
  }
}

#' Predict SpicyMKL
#' 
#' This function is used to predict SpicyMKL models
#' @param alpha coefficient
#' @param b intercept
#' @param k0 the kernel cube needs prediction
#' @return The predicted score
#' @export
predict_Spicy <- function(alpha, b, k0) {
  predictspicy(alpha, b, k0)
}