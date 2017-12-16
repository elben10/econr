#' Fit linear model
#'
#' \code{mod_lm} estimates linear models by minimizing the sum of squared residuals.
#'
#'
#' @inheritParams mod_matrix
#'
#' @return Returns a list of class mod.
#' @export
#'
#' @examples mod_lm(mtcars, mpg~cyl)
mod_lm <- function(data, formula) UseMethod("mod_lm")

#' @export
mod_lm.default <- function(data, formula) {
  obj <- deparse(substitute(data))
  glue_abort("`{obj}` is not a data.frame")
}

#' @export
mod_lm.data.frame <- function(data, formula) {
  mf <- stats::model.frame(formula, data = data)
  X <- stats::model.matrix(attr(mf, "terms"), data=mf)
  y <-stats::model.response(mf)

  structure(c(lm_rcpp(X, y, colnames(X)),
              call = match.call(),
              formula = formula),
            class = c("mod_lm", "mod"))
}

#' @export
print.mod_lm <- function(x, ...) {
  cat("\n" %+% underline$bold("Call:") %+% "\n")
  print(x$call)
  cat(glue('\n\n{underline$bold("Info:")}\nNumbers of observations: {x$observations}',
             "Number of regressors: {x$regressors}",
             "Degrees of freedom: {x$observations - x$regressors}",
             "R squared: {formatC(x$r_squared, 3)}",
             "Adjusted R squared: {formatC(x$adj_r_squared, 3)}",
             "Residual standard error: {formatC(x$resid_err, 3)}",
             '\n{underline$bold("Residuals:")}\n\n', .sep = "\n"))
  print(summary(x$residuals)[-4])
  print(inline_hist(x$residuals, 25))
  cat("\n" %+% underline$bold("Results:" %+% "\n"))

  stats::printCoefmat(x$return)

  cat("\n")

  invisible(x)
}


