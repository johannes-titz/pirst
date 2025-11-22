# functions for simulating data

# Create Data for a single ST-Plot for one Person
pirstsim_sc <- function(npercon,      # Number of Points per Condition
                        ncons,        # Number of Conditions
                        overlap,      # Points overlapping
                        interactsize, # Single or multiple process model, vector
                        noise,        # Gaussian Noise
                        curve) {      # Linear, Concave, Sigmoid
  ##producing the linear funtion
  npoints <- npercon * ncons
  Condition <- c()
  for (i in 1:ncons) {
    Condition <- c(Condition, rep(i, npercon))
  }
  p <- overlap
  M <- matrix(nrow = ncons, ncol = npercon)
  # producing the points
  for (i in 1:ncons) {
    for (j in 1:npercon){
      M[i, j] <- 0.7 * (j / npercon) + 0.15
      M[i, j] <- 0.7 * (j / npercon) + 0.15
    }
  }
  X <- c()
  for (i in 1:nrow(M)){
    X <- c(X, M[i, ])
  }
  # Evenly strechtch points out
  if (ncons == 1) {
    correct <- ncons+1
  } else {
    correct <- ncons
  }

  for (i in 1:npoints) {
    X[i] <- X[i] + (-0.35 + 0.7 * (Condition[i] - 1) / (correct - 1)) * ((p - 1 / ncons) / npercon)
  }
  ## Linear, curve, curve down, sigmoid or mirrored-sigmoid cases
  if (curve == 0) {
    # Linear
    Y <- X
    X <- X
  } else if (curve == 1) {
    # Transfroming to Concave Up
    Y <- sin((90 * X + 270) * pi / 180) + 1
    X <- cos((90 * X + 270) * pi / 180)
  } else if (curve == -1) {
    # Transforming to Concave Down
    Y <- cos((90 * X + 270) * pi / 180)
    X <- sin((90 * X + 270) * pi / 180) + 1
  } else if (curve == 2) {
    Y <- 0.5 * (1 + tanh(X * 10 - 5))
    X <- X
  }else if (curve == -2) {
    Y <- X
    X <- 0.5 * (1 + tanh(X * 10 - 5))
  } else {
    warning("invalid Curve input")
  }
  ## Adding the Interaction constant to Conditions
  for (i in 1:npoints) {
    Y[i] <- Y[i] + interactsize[Condition[i]]
  }
  ## Adding noise
  Y <- Y + stats::rnorm(npoints, 0, noise)
  X <- X + stats::rnorm(npoints, 0, noise)
  ## Combine to a dataframe
  Trace <- rep(seq(1, npercon, 1), ncons)
  simdata <- as.data.frame(cbind(X, Y, Condition, Trace))
  simdata
}

#STEP 2
#Repeat pirstsim_sc for multiple measures on one Person
pirstsim_rep_sc <- function(npercon,
                            ncons,
                            overlap,
                            interactsize,
                            noise,
                            curve,
                            nmeasures){
  measurements <- c()
  for (i in 1: nmeasures) {
    m <- pirstsim_sc(npercon, ncons, overlap, interactsize, noise, curve)
    m$Measurement <- rep(i, nrow(m))
    measurements <- rbind(measurements, m)
  }
  measurements
}

#' Create STA Data for Simulations
#'
#' Generates a complete state-trace analysis (STA) dataset for simulation
#' purposes. The dataset includes multiple participants, multiple conditions,
#' and multiple measures.
#'
#' @param npercon Integer. Number of data points per condition. Default is 4.
#' @param ncons Integer. Number of experimental conditions. Default is 2.
#' @param overlap Numeric. ??
#' @param interactsize Numeric vector of length ... ??
#' @param noise Numeric. Amount of random noise to add to the measures. Default
#'   is 0.
#' @param curve Numeric. ?? Default is 0.
#' @param nmeasures Integer. Number of measures per participant. Default is 10.
#' @param cases Integer. Number of cases (participants). Default is 100.
#'
#' @return A data frame containing the simulated STA data. Columns: X, Y,
#'   Condition, Trace, Measurement, Person
#'
#' @examples
#' # Generate a simple STA dataset with default parameters
#' sim_data <- pirstsim()
#'
#' # Generate a dataset with 5 participants per condition and 3 conditions
#' sim_data <- pirstsim(npercon = 5, ncons = 3)
#'
#' @export
pirstsim <- function(npercon = 4,
                     ncons = 2,
                     overlap = 1,
                     interactsize = rep(0, ncons),
                     noise = 0,
                     curve = 0,
                     nmeasures = 10,
                     cases = 100) {

  if (length(interactsize) != ncons) {
    stop("Number of Conditions must match length(interactsize)")
  }
  exp_simdata <- c()
  for (i in 1:cases) {
    simdata_part <- pirstsim_rep_sc(npercon, ncons, overlap, interactsize, noise, curve, nmeasures)
    simdata_part$Person <- c(rep(i, npercon * ncons))
    exp_simdata <- rbind(exp_simdata, simdata_part)
  }
  exp_simdata
}
