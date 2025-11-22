# cmr functions

#calculates isotonic regressions for X and Y Values for a specific order
#Needed for easy_cmr
combined_isoreg_fit <- function(data) {
  isoY <- fastiso(data$Y, data$Order)
  isoX <- fastiso(data$X, data$Order)
  ErrorY <- sum((isoY - data$Y) ^ 2)
  ErrorX <- sum((isoX - data$X) ^ 2)
  fit <- ErrorY + ErrorX
  fit
}

#runs CMR for every condition seperately
#Needed for easy_cmr
preorder <- function(data) {
  for (i in 1:max(data$Condition)) {
    getpreordered_X <- fastiso(data$X[data$Condition == i],
                               data$Trace[data$Condition == i])
    getpreordered_Y <- fastiso(data$Y[data$Condition == i],
                               data$Trace[data$Condition == i])
    data$X[data$Condition == i] <- getpreordered_X
    data$Y[data$Condition == i] <- getpreordered_Y
  }
  data
}

#runs an easy cmr_Version with a sorting-algorithm
easy_cmr <- function(data) {
  data <- preorder(data)
  data$Order <- rep(0, nrow(data))
  cmr_dat <- data[data$Condition == 1, ]
  cmr_dat <- cmr_dat[order(cmr_dat$Condition, cmr_dat$Trace), ]
  cmr_dat$Order <- seq(1, nrow(cmr_dat))
  # Searches for the best fitting spot of a Point of an ordered Condition
  # in another ordered Condition and continues until the whole condition is
  # sorted in
  for (i in 2:max(data$Condition)) {
    shorten_search <- 0.5
    for (j in 1:max(data$Trace)) {
      best_order <- matrix(0, nrow = nrow(cmr_dat) + 1, ncol = 2)
      for (k in seq(from = shorten_search,
                    to = max(cmr_dat$Order) + 1,
                    by = 1)) {
        newrow <- data[data$Condition == i & data$Trace == j, ]
        newrow$Order[1] <- k
        cmr_dat_plus <- cmr_dat
        cmr_dat_plus <- dplyr::bind_rows(cmr_dat_plus, newrow)
        fit <- combined_isoreg_fit(cmr_dat_plus)
        best_order[k + 0.5, 1] <- fit
        best_order[k + 0.5, 2] <- k
      }
      if (shorten_search > 0.5) {
        best_order <- as.matrix(best_order[-c(1:(shorten_search - 0.5)), ])
      }
      #sometimes the best_order matrix get automatically transformed into a vector
      #this fixes indices and continues the algorithm
      if (ncol(best_order) > 1) {
        placement <- best_order[which.min(best_order[, 1]), 2]
      } else {
        placement <- best_order[2, 1]
      }
      newrow$Order <- placement
      cmr_dat <- rbind(cmr_dat, newrow)
      cmr_dat <- cmr_dat[order(cmr_dat$Order), ]
      cmr_dat$Order <- seq(1, nrow(cmr_dat))
      shorten_search <-
        cmr_dat$Order[cmr_dat$Condition == i & cmr_dat$Trace == j] + 0.5
    }
  }
  isoY <- fastiso(cmr_dat$Y, cmr_dat$Order)
  isoX <- fastiso(cmr_dat$X, cmr_dat$Order)
  cmr_dat$X <- isoX
  cmr_dat$Y <- isoY
  cmr_dat
}
