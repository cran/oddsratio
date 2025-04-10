#' @title Suppress plotting output of [plot] function
#'
#' @description This function suppresses plotting output of [plot] function
#'
#' @importFrom grDevices dev.off png
#' @importFrom graphics plot
#'
#' @inheritParams or_gam
#'
#' @details To prevent unwanted plot printing of [plot] in a function call
#' in which the only desire is to work with the returned information of
#' `plot`. Used in [plot_gam()].
#'
#' @seealso [plot_gam()]
#' @name no_plot
#' @keywords internal
#'
#' @examples
#' # load data (Source: ?mgcv::gam)
#' library(mgcv)
#' n <- 200
#' sig <- 2
#' dat <- gamSim(1, n = n, scale = sig, verbose = FALSE)
#' dat$x4 <- as.factor(c(
#'   rep("A", 50), rep("B", 50), rep("C", 50),
#'   rep("D", 50)
#' ))
#' fit_gam <- gam(y ~ s(x0) + s(I(x1^2)) + s(x2) +
#'   offset(x3) + x4, data = dat) # fit model
#'
#' tmp <- plot(fit_gam, pages = 1) # plot output
#' tmp <- no_plot(fit_gam) # no plot output
#' @export
no_plot <- function(model = NULL) {
  png("temp.xyz")
  plot_df <- plot(model, pages = 1)
  dev.off()
  file.remove("temp.xyz")
  return(invisible(plot_df))
}

#' @title Converts a fitted GAM model into a tidy data frame
#'
#' @description This function converts a fitted GAM model into a tidy data frame
#'
#' @inheritParams or_gam
#'
#' @details To be able to plot the smoothing function of a GAM using ggplot2,
#' some preprocessing is needed coming from the raw fitted GAM model output.
#'
#' Used in [plot_gam()].
#'
#' @name gam_to_df
#' @seealso [plot_gam()]
#' @keywords internal
#'
#' @examples
#' # load data (Source: ?mgcv::gam)
#' library(mgcv)
#' n <- 200
#' sig <- 2
#' dat <- gamSim(1, n = n, scale = sig, verbose = FALSE)
#' dat$x4 <- as.factor(c(
#'   rep("A", 50), rep("B", 50), rep("C", 50),
#'   rep("D", 50)
#' ))
#' fit_gam <- gam(y ~ s(x0) + s(I(x1^2)) + s(x2) +
#'   offset(x3) + x4, data = dat) # fit model
#'
#' tmp <- gam_to_df(fit_gam, "x2")
#' @export
gam_to_df <- function(model = NULL, pred = NULL) {
  plot_df <- no_plot(model) # nolint

  # trim whitespace in 'pred' otherwise regex matching doesn't work
  pred <- gsub("\\s+", "", pred)

  # get list index of spec. predictor
  # NB: the regex checks for a komma followed by a number to distinguish single predicts from mixed predictors to only return a single list index (e.g. "age" vs (age, los)). See https://github.com/pat-s/oddsratio/issues/54
  set_pred <- grep(paste0("\\b", pred, "\\b", ",[0-9]"), plot_df)

  df <- data.frame(
    x = plot_df[[set_pred]]$x,
    se_upr = plot_df[[set_pred]]$fit + plot_df[[set_pred]]$se,
    se_lwr = plot_df[[set_pred]]$fit - plot_df[[set_pred]]$se,
    y = plot_df[[set_pred]]$fit
  )
  return(df)
}
