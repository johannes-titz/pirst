# functions for significance test

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

#get the deviation-distribution of every point for every person
#uses the raw data and the pirst(data,...)-output
get_dist <- function(data, test) {
  datanew <- c()
  for (i in 1:max(data$Person)) {
    for (j in 1:max(data$Condition)) {
      for (h in 1:max(data$Trace)) {
        distx <- data[data$Person == i &
                        data$Condition == j &
                        data$Trace == h, ]$X -
          test[["Data"]][test[["Data"]]$Person == i &
                           test[["Data"]]$Condition == j &
                           test[["Data"]]$Trace == h, ]$X

        disty <-  data[data$Person == i &
                         data$Condition == j &
                         data$Trace == h, ]$Y -
          test[["Data"]][test[["Data"]]$Person == i &
                           test[["Data"]]$Condition == j &
                           test[["Data"]]$Trace == h, ]$Y

        newcondition <- rep(j, length(distx))
        newtrace <- rep(h, length(distx))
        newperson <- rep(i, length(distx))
        newrow <-
          cbind(distx, disty, newcondition, newtrace, newperson)
        datanew <- rbind.data.frame(datanew, newrow)
      }
    }
  }
  names(datanew) <-
    c("distX", "distY", "Condition", "Trace", "Person")
  datanew
}

# creates the nullmodel and the nulleffect-Dataframe
create_null <- function(test) {
  #first create the nullmodel
  testdata <- test[["Data"]]
  nulldata <- c()
  for (i in 1:max(testdata$Person)) {
    nulldata_part <- easy_cmr(testdata[testdata$Person == i,])
    nulldata <- rbind(nulldata, nulldata_part)
  }
  nulldata <- cbind.data.frame(
    nulldata$X,
    nulldata$Y,
    nulldata$Condition,
    nulldata$Trace,
    rep(1, nrow(nulldata)),
    nulldata$Person
  )
  names(nulldata) <-
    c("X", "Y", "Condition", "Trace", "Measurement", "Person")
  nulldata
  #nullmodel is complete, now deviations can be added to points to generate
}
create_null_plus_dev <- function(nulldata, dist) {
  for (i in 1:max(dist$Person)) {
    for (j in 1:max(dist$Condition)) {
      for (h in 1:max(dist$Trace)) {
        randx <- dist[dist$Person == i &
                        dist$Condition == j &
                        dist$Trace == h,]$distX
        randy <- dist[dist$Person == i &
                        dist$Condition == j &
                        dist$Trace == h,]$distY
        randxmean <- 0
        randymean <- 0
        #####    UPDATE
        for (k in 1:length(randx)) {
          randdev <- sample(1:length(randx), 1)
          randxmean <- randxmean + randx[randdev]
        }
        for (k in 1:length(randy)) {
          randdev <- sample(1:length(randy), 1)
          randymean <- randymean + randy[randdev]
        }
        randxmean <- randxmean / length(randx)
        randymean <- randymean / length(randy)
        #####    UPDATE
        nulldata[nulldata$Person == i &
                   nulldata$Condition == j &
                   nulldata$Trace == h,]$X <-
          nulldata[nulldata$Person == i &
                     nulldata$Condition == j &
                     nulldata$Trace == h, ]$X +
          randxmean
        nulldata[nulldata$Person == i &
                   nulldata$Condition == j &
                   nulldata$Trace == h,]$Y <-
          nulldata[nulldata$Person == i &
                     nulldata$Condition == j &
                     nulldata$Trace == h,]$Y +
          randymean
      }
    }
  }
  nulldata
}

# create null-distribution by drawing nulleffect-dataframes and collect mean p-greater
create_null_dist <- function(test, dist, ndraws, nperms = 100) {
  null_dist <- c()
  null_without_dev <- create_null(test)
  for (i in 1:ndraws) {
    nulldata <- create_null_plus_dev(null_without_dev, dist)
    null_test <- pirst(nulldata, nperms)
    null_dist[i] <- null_test[["Mean P greater"]]
  }
  null_dist
}


#Combines previous functions
#runs a significance Test for the found effect given the null-distribution
pirst_conf <- function(data, test, ndraws, nperms = 100, conf = 0.95) {
  dist <- get_dist(data, test)
  nulldist <- create_null_dist(test, dist, ndraws, nperms)
  upper <- round(conf * length(nulldist))
  nulldist <- sort(nulldist)
  upperbound <- nulldist[upper]
  p <- sum(nulldist > test[["Mean P greater"]]) / length(nulldist)
  significance <- FALSE
  if (upperbound < test[["Mean P greater"]]) {
    significance <- TRUE
  }
  Significancetest <-
    list(test[["Mean P greater"]], upperbound, significance, p, conf, nulldist)
  names(Significancetest) <-
    c("Mean P greater",
      "Upper",
      "Significance",
      "p-Value",
      "Confidence",
      "Null-Distribution")
  Significancetest
}

#' PIRST significance test
#'
#' @param data data set with columns:
#' @param nperms number of permutations
#' @param nboot number of bootstrap samples
#' @param conf confidence (1 - Alpha)
#' @export
pirst_test <- function(data, nperms = 100, nboot = 100, conf = 0.95) {
  test <- pirst(data, nperms)
  sigtest <- pirst_conf(data = data,
                        test = test,
                        ndraws = nboot,
                        nperms = nperms,
                        conf = conf)
  sigtest
}
