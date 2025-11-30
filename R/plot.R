#' STA plot (base R version)
#' @param simdata A data frame with three columns: X variable, Y variable, Condition
#' @param xlab Label for the x-axis
#' @param ylab Label for the y-axis
#' @param legend_position position of legend (base R plotting)
#' @export
st_plot <- function(simdata, xlab = "Variable A", ylab = "Variable B",
                    legend_position = "topleft") {
  # treat third column as factor condition
  Condition <- as.factor(simdata[[3]])
  X <- simdata[[1]]
  Y <- simdata[[2]]

  # colors for conditions
  cond_levels <- levels(Condition)
  cols <- seq_along(cond_levels)

  # empty plot
  plot(
    X, Y,
    type = "n",
    xlab = xlab,
    ylab = ylab,
    main = "State-Trace-Plot"
  )

  # draw points per condition
  for (i in seq_along(cond_levels)) {
    cond <- cond_levels[i]
    idx <- Condition == cond
    points(X[idx], Y[idx], pch = 19, col = cols[i])
  }

  legend(
    legend_position,
    legend = cond_levels,
    col = cols,
    pch = 19,
    title = "Condition"
  )
}
