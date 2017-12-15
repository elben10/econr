#' Fit linear model
#'
#' \code{model_lm} estimates linear models by minimizing the sum of squared residuals.
#'
#'
#' @inheritParams model_matrix
#'
#' @return Returns a list of class mod.
#' @export
#'
#' @examples model_lm(mtcars, mpg~cyl)
model_lm <- function(data, formula) UseMethod("model_lm")

#' @export
model_lm.default <- function(data, formula) {
  obj <- deparse(substitute(data))
  glue_abort("`{obj}` is not a data.frame")
}

#' @export
model_lm.data.frame <- function(data, formula) {
  mf <- stats::model.frame(formula, data = data)
  X <- stats::model.matrix(attr(mf, "terms"), data=mf)
  y <-stats::model.response(mf)

  structure(c(lm_rcpp(X, y, colnames(X)), call = match.call()),
            class = c("model_lm", "mod"))
}
