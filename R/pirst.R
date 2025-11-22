# functions for pirst algorithm

#Aggregates X and Y over repeated measures of one Person
aggregate <- function(data) {
  newx <- c()
  newy <- c()
  newcondition <- c()
  newtrace <- c()
  newperson <- c()
  datanew <- c()
  for (i in 1:max(data$Person)) {
    for (j in 1:max(data$Condition)) {
      for (h in 1:max(data$Trace)) {
        newx <-  mean(data[data$Person == i &
                             data$Condition == j &
                             data$Trace == h, ]$X, na.rm = TRUE)
        newy <-  mean(data[data$Person == i &
                             data$Condition == j &
                             data$Trace == h, ]$Y, na.rm = TRUE)
        newcondition <- j
        newtrace <- h
        newperson <- i
        newrow <- cbind(newx, newy, newcondition, newtrace, newperson)
        datanew <- rbind.data.frame(datanew, newrow)
      }
    }
  }
  names(datanew) <- c("X", "Y", "Condition", "Trace", "Person")
  datanew
}

#Identifies eligible points
find_eligible_points <- function(data) {
  names(data) <- c("X", "Y", "Condition", "Trace")
  #Create a Dataframe of eligibilityvalues for the data
  eligibilityframe <-
    as.data.frame(matrix(1, nrow = nrow(data), max(data$Condition)))
  eligibilitynames <- c()
  for (i in 1:(max(data$Condition))) {
    eligibilitynames[i] <- paste0("eligibility ", i)
  }
  names(eligibilityframe) <- eligibilitynames
  data <- cbind(data, eligibilityframe) # Add it to the data
  # in the next process the algorithm identifies all conditions  a point is
  # eligible with and adds that to the Dataframe
  data <-
    data[order(data$Condition, data$X), ] # needed for next function
  for (p in 1:max(data$Condition)) {
    for (q in 1:max(data$Condition)) {
      for (i in 1:(nrow(data))) {
        if ((data$Condition)[i] == p) {
          data[i, p + 4] <- 1
        } else if (data$Condition[i] == q) {
          if ((data$X[i] >= max(data$X[data$Condition == p]) &&
               data$Y[i] >= max(data$Y[data$Condition == p])) ||
              data$X[i] <= min(data$X[data$Condition == p]) &&
              data$Y[i] <= min(data$Y[data$Condition == p])) {
            data[i, p + 4] <- 0
          }
        }
      }
    }
  }
  data
}

#Identifies the Conditions a point is eligible with (for ncons > 2)
combfinder <- function(data) {
  eligibilityframe <- data[, 5:(4 + max(data$Condition))]
  eligibilitycombs <- dplyr::distinct(eligibilityframe)
  combination <- seq(1, nrow(eligibilitycombs), 1)
  eligibilitycombs$combination <- combination
  eligibility <- c(rep(0, nrow((data))))
  for (i in 1:nrow(eligibilitycombs)) {
    for (j in 1:(nrow(data))) {
      if (all(data[j, 5:(4 + max(data$Condition))] == eligibilitycombs[i, 1:max(data$Condition)]) == TRUE) {
        eligibility[j] <- eligibilitycombs$combination[i]
      }
    }
  }
  data$eligibility <- eligibility # Every Point gets a specific eligibility
  data                            # decoding-number
}

#Identifies if data is not completely overlapping(ncons >2) or not overlapping at all
find_cuts <- function(data) {
  cuts <- data
  result <- 0
  #starts at condition one and checks from conditions to condition if there is
  #any overlap to find out if there is a cut between conditions
  for (j in 1:max(cuts$Condition)) {
    for (i in 1:nrow(cuts)) {
      if ((cuts[i, 4 + 1] == 1) && (cuts$Condition[i] != 1)) {
        cuts[, 4 + j][cuts$Condition == cuts$Condition[i]] = 1
        cuts$Condition[i] = 1
      }
    }
  }
  if (all(cuts$Condition == data$Condition) == TRUE) {
    result <- 1 #no permutations possible
  } else if (all(cuts$Condition == rep(1, nrow(cuts))) == TRUE) {
    result <- 0 #no cuts, overlap of all Conditions
  } else {
    result <- 2 #at least one condition is isolated
  }
  result
}

#runs extremely fast isotonic regression
fastiso <- function (y,x) {
  if (anyDuplicated (x) == 0) {
    switchback <- seq_along(y)
    isomat <- cbind(y,x,switchback)
    isomat <- isomat[Rfast::Order(isomat[, 2]),]
    isomat[, 1] <- monotone::monotone(isomat[, 1])
    isomat <- isomat[Rfast::Order(isomat[, 3]),]
    y <- isomat[, 1]
    y
  } else {#tie handling, because monotone does not have one (extremly few cases)
    isoreg <- isotone::gpava(x,y)
    y <- isoreg[["x"]]
    y
  }
}

#runs isotonic regression with x-axis a predictor and adds values to data
isoreg <- function(data) {
  Y_isoreg <- c()
  for (i in 1:max(data$Condition)) {
    isoreg <- fastiso(data$Y[data$Condition == i],
                      data$X[data$Condition == i])
    Y_isoreg <- c(Y_isoreg, isoreg)
  }
  data$Y_isoreg <- Y_isoreg
  data
}

#calculates SSE for the original Data
SSE_calc <- function(data) {
  SSE_gather <- c()
  for (i in 1:max(data$Condition)) {
    SSE_hold <- sum((data$Y_isoreg[data$Condition == i] -
                       data$Y[data$Condition == i]) ^ 2)
    SSE_gather[i] <- SSE_hold
  }
  SSE <- sum(SSE_gather)
  SSE
}

#Shuffles Conditionslabels from points of the same eligibilitycombination randomly
shuffle_more <- function(data) {
  new_perm <- data[c("Condition","eligibility")]
  for (i in 1:max(data$eligibility)) {
    if (length(new_perm$Condition[new_perm$eligibility == i]) > 1) {
      shuffle <- sample(new_perm$Condition[new_perm$eligibility == i])
      new_perm$Condition[data$eligibility == i] <- shuffle
    }
  }
  data$Condition <- new_perm$Condition
  data
}

#calculates SSE of a Permutation without adding any Values to a Dataframe
isoreg_ssepermcalc <- function(data) {
  SSE_gather <- c()
  for (i in 1:max(data$Condition)) {
    iso_reg <- fastiso(data$Y[data$Condition == i],data$X[data$Condition == i])
    SSE_gather[i] <- sum((iso_reg - data$Y[data$Condition == i]) ^ 2)
  }
  SSE <- sum(SSE_gather)
  SSE
}

#combines Shuffle_more and isoreg_ssepermcalc to permutate and get SSE in one step
permutate <- function(data, nperms) {
  cont_shuffle <-
    data$Condition #for shuffling again if permutation = original
  SSE_all <- c(rep(0, nperms))
  for (i in 1:nperms) {
    data <- shuffle_more(data)
    count <- 0
    while ((all(data$Condition == cont_shuffle) == TRUE) && (count < 1000) ) {
      data <- shuffle_more(data) #Bugfix for a very rare case of overlap but no
      count <- count + 1         # possible Permuations where the algorithm hangs up
      #...annoying
    }
    if (count < 1000){
      SSE_all[i] <- isoreg_ssepermcalc(data)
    } else {   #in this rare case, there are no possible permutations
      SSE_all <- c(rep(NaN, nperms))
      message("BUT no permutations are possible, this subject will not have impact on the Test-statistic")
      break
    }
  }
  SSE_all
} #nperms = number of Permutations

#HEART OF THE ALGORITHM
#combines all previous functions to a PIRST for a single case
pirst_sc <- function(data, nperms) {
  data <- find_eligible_points(data)
  data <- combfinder(data)
  data <- isoreg(data)
  SSE_compare <- SSE_calc(data)
  info <- find_cuts(data)
  if (info == 1) {
    message(
      "Because of no overlap, this subject will not have impact on the Test-statistic!"
    ) # No Overlap. Bad!
    SSE <- NaN
  } else if (info == 2) {
    message("Data is NOT completely overlapping!")
    SSE <- permutate(data, nperms) #just to inform Experimenter
  } else {
    message("Data is completely overlapping.")
    SSE <- permutate(data, nperms) #best case, everyting is fine
  }
  Proportion_greater <- sum(SSE > SSE_compare) / length(SSE)
  Proportion_equal <- sum(SSE == SSE_compare) / length(SSE)
  Proportion_smaller <- sum(SSE < SSE_compare) / length(SSE)
  Statistic <-
    list(Proportion_greater,
         Proportion_equal,
         Proportion_smaller,
         SSE,
         SSE_compare,
         data)
  names(Statistic) <-
    c(
      "Proportion greater",
      "Proportion equal",
      "Proportion smaller",
      "All SSE's",
      "SSE original",
      "Data"
    )
  Statistic
}

#applies pirst_sc to every person and calculates mean p-greater and other statistics
pirst <- function(data, nperms= 100) {
  names(data) <-
    c("X", "Y", "Condition", "Trace", "Measurement", "Person")
  SSE_greater <- c(rep(0, max(data$Person)))
  SSE_equal <- c(rep(0, max(data$Person)))
  SSE_smaller <- c(rep(0, max(data$Person)))
  SSE_original <- c(rep(0, max(data$Person)))
  output_data <- as.data.frame(c())
  data <- aggregate(data)
  for (i in 1:max(data$Person)) {
    calc_simdata <- data[data$Person == i, ]
    calc_simdata <- calc_simdata[, -5]
    message("For subject ", i)
    get_sc_data_greater <- pirst_sc(calc_simdata, nperms)
    SSE_greater[i] <- get_sc_data_greater[["Proportion greater"]]
    SSE_equal[i] <- get_sc_data_greater[["Proportion equal"]]
    SSE_smaller[i] <- get_sc_data_greater[["Proportion smaller"]]
    SSE_original[i] <- get_sc_data_greater[["SSE original"]]
    output_data <- rbind(output_data, get_sc_data_greater[["Data"]])
  }
  SSE_data <- as.data.frame(cbind(SSE_greater, SSE_equal, SSE_smaller, SSE_original))
  SSE_data <- stats::na.omit(SSE_data)
  Prop_greater <- mean(SSE_data$SSE_greater)
  Prop_equal <- mean(SSE_data$SSE_equal)
  Prop_smaller <- mean(SSE_data$SSE_smaller)
  SSE_original_mean <- mean(SSE_data$SSE_original)
  Prop_greater_all <- SSE_data$SSE_greater
  SSE_original_all <- SSE_data$SSE_original
  Person <- data$Person
  output_data <- cbind(output_data, Person)

  Statistic <-
    list(
      Prop_greater,
      Prop_equal,
      Prop_smaller,
      SSE_original_mean,
      Prop_greater_all,
      SSE_original_all,
      output_data
    )
  names(Statistic) <-
    c(
      "Mean P greater",
      "Mean P equal",
      "Mean P smaller",
      "Mean of original SSE's",
      "Proportion of SSE's greater original SSE",
      "All original SSE's",
      "Data"
    )
  Statistic
}
